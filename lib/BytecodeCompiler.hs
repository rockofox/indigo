module BytecodeCompiler (runTestProgram, locateLabel, printAssembly, compileProgram, CompilerState (..)) where

import Control.Monad.State (MonadIO (liftIO), StateT, evalStateT, gets, modify)
import Data.List (elemIndex, find)
import Data.Maybe (fromJust)
import Data.Text (splitOn)
import Data.Text qualified
import Data.Text qualified as T
import Debug.Trace
import Parser (CompilerFlags (CompilerFlags), parseProgram)
import Parser qualified
import Text.Megaparsec (errorBundlePretty)
import VM
    ( Action (Print)
    , Data (..)
    , Instruction (..)
    , StackFrame (StackFrame, locals, returnAddress)
    , VM (breakpoints, callStack, pc)
    , initVM
    , printAssembly
    , runVM
    )

data Function = Function
    { baseName :: String
    , funame :: String
    , function :: [Instruction]
    }
    deriving (Show)

data CompilerState = CompilerState
    { program :: Parser.Program
    , -- , functions :: [(String, String, [Instruction])]
      functions :: [Function]
    , funcDecs :: [Parser.Expr]
    , lastLabel :: Int
    }
    deriving (Show)

allocId :: StateT CompilerState IO Int
allocId = do
    s <- gets lastLabel
    modify (\s' -> s'{lastLabel = s + 1})
    return s

concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = concat <$> mapM f xs

compileProgram :: Parser.Program -> StateT CompilerState IO [Instruction]
compileProgram (Parser.Program expr) = do
    freePart <- concatMapM compileExpr expr
    functions' <- gets functions >>= \x -> return $ concatMap function (reverse x)
    return $ functions' ++ freePart ++ [Exit]

findFunction :: String -> [Function] -> Maybe Function
findFunction name xs = do
    -- TODO: I don't like this function
    let candidates = filter (\y -> baseName y == name && funame y /= "main") xs
    let ids = map ((!! 1) . splitOn "#" . Data.Text.pack . funame) candidates
    let ids' = map (read . Data.Text.unpack) ids :: [Int]
    let minId = minimum ids'
    find (\y -> funame y == (name ++ "#" ++ show minId)) candidates

unmangleFunctionName :: String -> String
unmangleFunctionName = takeWhile (/= '#')

compileExpr :: Parser.Expr -> StateT CompilerState IO [Instruction]
compileExpr (Parser.Add x y) = compileExpr x >>= \x' -> compileExpr y >>= \y' -> return (x' ++ y' ++ [Add])
compileExpr (Parser.Sub x y) = compileExpr x >>= \x' -> compileExpr y >>= \y' -> return (x' ++ y' ++ [Sub])
compileExpr (Parser.Mul x y) = compileExpr x >>= \x' -> compileExpr y >>= \y' -> return (x' ++ y' ++ [Mul])
compileExpr (Parser.Div x y) = compileExpr x >>= \x' -> compileExpr y >>= \y' -> return (x' ++ y' ++ [Div])
compileExpr (Parser.Modulo x y) = compileExpr x >>= \x' -> compileExpr y >>= \y' -> return (x' ++ y' ++ [Mod])
compileExpr (Parser.Gt x y) = compileExpr x >>= \x' -> compileExpr y >>= \y' -> return (x' ++ y' ++ [Gt])
compileExpr (Parser.Lt x y) = compileExpr x >>= \x' -> compileExpr y >>= \y' -> return (x' ++ y' ++ [Lt])
compileExpr (Parser.Not x) = compileExpr x >>= \x' -> return (x' ++ [Not])
compileExpr (Parser.Eq x y) = compileExpr x >>= \x' -> compileExpr y >>= \y' -> return (x' ++ y' ++ [Eq])
compileExpr (Parser.IntLit x) = return [Push $ DInt $ fromIntegral x]
compileExpr (Parser.StringLit x) = return [Push $ DString x]
compileExpr (Parser.DoBlock exprs) = concatMapM compileExpr exprs
compileExpr (Parser.FuncCall "print" [x]) = compileExpr x >>= \x' -> return (x' ++ [Intr Print])
compileExpr (Parser.FuncCall name args) = do
    functions' <- gets functions
    funcDecs' <- gets funcDecs

    let fun = case findFunction name functions' of
            (Just f) -> f
            Nothing -> Function{baseName = unmangleFunctionName name, funame = name, function = []}
    let funcDec = find (\(Parser.FuncDec name' _) -> name' == baseName fun) funcDecs'
    case funcDec of
        (Just fd) -> do
            if length args == length (Parser.ftypes fd) - 1
                then concatMapM compileExpr args >>= \args' -> return (args' ++ [Call (funame fun)])
                else
                    concatMapM compileExpr args >>= \args' ->
                        return $
                            args'
                                ++ [PushPf (funame fun) (length args')]
        Nothing ->
            concatMapM compileExpr args >>= \args' ->
                return $
                    args'
                        ++ [ LLoad name
                           , CallS
                           ]
compileExpr fd@(Parser.FuncDec _ _) = do
    modify (\s -> s{funcDecs = fd : funcDecs s})
    return [] -- Function declarations are only used for compilation
compileExpr (Parser.FuncDef name args body) = do
    funame <- if name /= "main" then ((name ++ "#") ++) . show <$> allocId else return "main"
    modify (\s -> s{functions = Function name funame [] : functions s})

    body' <- compileExpr body
    funcDecs' <- gets funcDecs
    args' <- concatMapM (`compileParameter` name) (reverse args)
    let funcDec = fromJust $ find (\(Parser.FuncDec name' _) -> name' == name) funcDecs'
    let function = Label funame : args' ++ body' ++ ([Ret | name /= "main"])
    modify (\s -> s{functions = Function name funame function : tail (functions s)})
    return [] -- Function definitions get appended at the last stage of compilation
  where
    compileParameter :: Parser.Expr -> String -> StateT CompilerState IO [Instruction]
    compileParameter (Parser.Var name) _ = return [LStore name]
    compileParameter lex@(Parser.ListLit l) name = do
        nextFunName <- ((name ++ "#") ++) . show . (+ 1) <$> allocId
        if null l
            then return [Dup, Push $ DList [], Eq, Jf nextFunName]
            else do
                lex' <- compileExpr lex
                return $ [Dup] ++ lex' ++ [Eq, Jf nextFunName] -- TODO: Check if this works
    compileParameter (Parser.ListPattern elements) n = do
        elements' <- concatMapM (`compileParameter` n) elements
        let paramsWithIndex = zip elements' [0 ..]
        let xToY = map (\(x, index) -> [Dup, Push $ DInt index, Index, x]) paramsWithIndex
        let rest = [Push $ DInt (length elements - 1), Push DNone, Slice, last elements']
        return $ concat xToY ++ rest
    compileParameter Parser.Placeholder _ = return []
    compileParameter x _ = error $ show x ++ ": not implemented as a function parameter"
compileExpr (Parser.Var x) = do
    functions' <- gets functions
    let fun = findFunction x functions'
    case fun of
        (Just f) -> compileExpr (Parser.FuncCall (funame f) [])
        Nothing -> return [LLoad x]
compileExpr (Parser.Let name value) = do
    value' <- compileExpr value
    return $ value' ++ [LStore name]
compileExpr (Parser.Function a b) = mapM_ compileExpr a >> compileExpr b >> return []
compileExpr (Parser.Flexible a) = compileExpr a >>= \a' -> return $ Meta "flex" : a'
compileExpr (Parser.ListConcat a b) = do
    a' <- compileExpr a
    b' <- compileExpr b
    let a'' = case a' of
            [Meta "flex", a'] -> [a'] ++ b' ++ [Cast]
            _ -> a'
    let b'' = case b' of
            [Meta "flex", b'] -> [b'] ++ a' ++ [Cast]
            _ -> b'
    return (b'' ++ a'' ++ [Concat 2])
compileExpr (Parser.ListLit elements) = concatMapM compileExpr elements >>= \elements' -> return (reverse elements' ++ [Concat $ length elements])
compileExpr (Parser.If cond then' else') = do
    cond' <- compileExpr cond
    then' <- compileExpr then'
    else' <- compileExpr else'
    elseLabel <- allocId >>= \x -> return $ "else" ++ show x
    endLabel <- allocId >>= \x -> return $ "end" ++ show x
    return $ cond' ++ [Jf elseLabel] ++ then' ++ [Jmp endLabel, Label elseLabel] ++ else' ++ [Label endLabel]
compileExpr (Parser.FloatLit x) = return [Push (DFloat x)]
compileExpr (Parser.BoolLit x) = return [Push (DBool x)]
compileExpr (Parser.TypeLit x) = case x of
    Parser.Int -> return [Push $ DInt 0]
    Parser.Float -> return [Push $ DFloat 0.0]
    Parser.String -> return [Push $ DString ""]
    Parser.Bool -> return [Push $ DBool False]
    _ -> error $ show x ++ " is not implemented"
compileExpr (Parser.Cast from to) = compileExpr from >>= \x -> compileExpr to >>= \y -> return (x ++ y ++ [Cast])
compileExpr x = error $ show x ++ " is not implemented"

locateLabel :: [Instruction] -> String -> Int
locateLabel program label = do
    let x = elemIndex (Label label) program
    case x of
        Just x' -> x'
        Nothing -> error $ "Label " ++ label ++ " not found"

runTestProgram :: IO ()
runTestProgram = do
    let p =
            parseProgram
                ( Data.Text.pack
                    ( unlines
                        [ "println :: String -> IO"
                        , "println x = do"
                        , "    print x"
                        , "    print \"\\n\""
                        , "end"
                        , "sayHi :: IO"
                        , "sayHi name = do"
                        , "    print \"Hi, \""
                        , "    print name"
                        , "    print \"!\""
                        , "end"
                        , "main => IO = do"
                        , -- , "    let name = \"Rocko\""
                          -- , "    sayHi name"
                          --   "   sayHi \"Rocko\""
                          "   let numbers = [1, 2, 3, 4, 5]"
                        , "   println (\"Hello\") : (\" \") : (\"World\")"
                        , "   println numbers"
                        , "end"
                        ]
                    )
                )
                Parser.CompilerFlags{verboseMode = False}
    case p of
        Left err -> putStrLn $ errorBundlePretty err
        Right program -> do
            putStrLn ""
            xxx <- evalStateT (compileProgram program) (CompilerState program [] [] 0)
            -- print program
            putStrLn $ printAssembly xxx True
            let xxxPoint = locateLabel xxx "main"
            runVM $ (initVM xxx){pc = xxxPoint, breakpoints = [], callStack = [StackFrame{returnAddress = xxxPoint, locals = []}]}
