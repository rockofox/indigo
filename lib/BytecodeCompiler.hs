module BytecodeCompiler (runTestProgram, locateLabel, printAssembly, compileProgram, CompilerState (..)) where

import Control.Monad.State (MonadIO (liftIO), StateT, evalStateT, gets, modify, when)
import Data.List (elemIndex, find)
import Data.Maybe (fromJust)
import Data.Text (splitOn)
import Data.Text qualified
import Data.Text qualified as T
import Debug.Trace
import Parser (CompilerFlags (CompilerFlags), parseProgram)
import Parser qualified
import Paths_indigo qualified
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
    prelude <- do
        i <- liftIO preludeFile
        let expr = case parseProgram (T.pack i) CompilerFlags{verboseMode = False} of -- FIXME: pass on flags
                Left err -> error $ "Parse error: " ++ errorBundlePretty err
                Right (Parser.Program expr) -> expr
        concatMapM compileExpr expr
    freePart <- concatMapM compileExpr expr
    functions' <- gets functions >>= \x -> return $ concatMap function (reverse x)
    return $ prelude ++ functions' ++ freePart ++ [Exit]

-- | Compile, but don't include the prelude
compileProgramBare :: Parser.Program -> StateT CompilerState IO [Instruction]
compileProgramBare (Parser.Program expr) = do
    functions' <- gets functions >>= \x -> return $ concatMap function (reverse x)
    return $ functions'

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

preludeFile :: IO String
preludeFile = Paths_indigo.getDataFileName "std/prelude.prism" >>= readFile

doBinOp :: Parser.Expr -> Parser.Expr -> Instruction -> StateT CompilerState IO [Instruction]
doBinOp x y op = do
    functions' <- gets functions
    let f = findFunction (Data.Text.unpack $ Data.Text.toLower $ Data.Text.pack $ show op) functions'
    x' <- compileExpr x
    y' <- compileExpr y
    case (x, y) of
        (_, Parser.FuncCall _ _) -> return (y' ++ x' ++ [Swp] ++ [op])
        (Parser.FuncCall _ _, _) -> return (x' ++ y' ++ [op])
        (Parser.Placeholder, Parser.Placeholder) -> return [PushPf (funame $ fromJust f) 0]
        (Parser.Placeholder, _) -> return $ y' ++ [PushPf (funame $ fromJust f) 1]
        (_, Parser.Placeholder) -> return $ x' ++ [PushPf (funame $ fromJust f) 1]
        _ -> return (x' ++ y' ++ [op])

compileExpr :: Parser.Expr -> StateT CompilerState IO [Instruction]
compileExpr (Parser.Add x y) = doBinOp x y Add
compileExpr (Parser.Sub x y) = doBinOp x y Sub
compileExpr (Parser.Mul x y) = doBinOp x y Mul
compileExpr (Parser.Div x y) = doBinOp x y Div
compileExpr (Parser.Modulo x y) = doBinOp x y Mod
compileExpr (Parser.Power x y) = doBinOp x y Pow
compileExpr (Parser.Gt x y) = doBinOp x y Gt
compileExpr (Parser.Lt x y) = doBinOp x y Lt
compileExpr (Parser.Not x) = compileExpr x >>= \x' -> return (x' ++ [Not])
compileExpr (Parser.Eq x y) = doBinOp x y Eq
compileExpr (Parser.IntLit x) = return [Push $ DInt $ fromIntegral x]
compileExpr (Parser.UnaryMinus (Parser.FloatLit x)) = return [Push $ DFloat (-x)]
compileExpr (Parser.UnaryMinus (Parser.IntLit x)) = return [Push $ DInt $ -fromInteger x]
compileExpr (Parser.StringLit x) = return [Push $ DString x]
compileExpr (Parser.DoBlock exprs) = concatMapM compileExpr exprs
compileExpr Parser.Placeholder = return []
compileExpr (Parser.FuncCall "print" [x]) = compileExpr x >>= \x' -> return (x' ++ [Builtin Print])
compileExpr (Parser.FuncCall "abs" [x]) = compileExpr x >>= \x' -> return (x' ++ [Abs])
compileExpr (Parser.FuncCall "root" [x, Parser.FloatLit y]) = compileExpr x >>= \x' -> return (x' ++ [Push $ DFloat (1.0 / y), Pow])
compileExpr (Parser.FuncCall "sqrt" [x]) = compileExpr x >>= \x' -> return (x' ++ [Push $ DFloat 0.5, Pow])
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
    args' <- concatMapM (`compileParameter` name) (reverse (filter (/= Parser.Placeholder) args))
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
        nextFunName <- ((name ++ "#") ++) . show . (+ 1) <$> allocId
        let lengthCheck = [Dup, Length, Push $ DInt $ fromIntegral $ length elements - 1, Lt, StackLength, Push $ DInt 1, Neq, And, Jt nextFunName]
        case last elements of
            Parser.ListLit l -> do
                elements' <- concatMapM (`compileParameter` n) (init elements)
                let paramsWithIndex = zip elements' [0 ..]
                let xToY = map (\(x, index) -> [Dup, Push $ DInt index, Index, x]) paramsWithIndex
                l' <- compileExpr (Parser.ListLit l)
                let listThing = [Comment "List thing", Dup, Push $ DInt (length elements - 1), Push DNone, Slice] ++ l' ++ [Eq, Jf nextFunName]
                return $ lengthCheck ++ concat xToY ++ listThing
            _ -> do
                elements' <- concatMapM (`compileParameter` n) elements
                let paramsWithIndex = zip elements' [0 ..]
                let xToY = map (\(x, index) -> [Dup, Push $ DInt index, Index, x]) paramsWithIndex
                let rest = [Push $ DInt (length elements - 1), Push DNone, Slice, last elements']
                return $ lengthCheck ++ concat xToY ++ rest
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
compileExpr (Parser.Import o from) = do
    when (o /= ["*"]) $ error "Only * imports are supported right now"
    let convertedPath = map (\x -> if x == '.' then '/' else x) from
    i <- liftIO $ readFile $ convertedPath ++ ".in"
    let expr = case parseProgram (T.pack i) CompilerFlags{verboseMode = False} of -- FIXME: pass on flags
            Left err -> error $ "Parse error: " ++ errorBundlePretty err
            Right (Parser.Program exprs) -> exprs
    concatMapM compileExpr (map mangleAST expr)
  where
    mangleAST :: Parser.Expr -> Parser.Expr
    mangleAST (Parser.FuncDec name types) = Parser.FuncDec (from ++ "." ++ name) types
    mangleAST (Parser.Function fdef fdec) = Parser.Function (map mangleAST fdef) (mangleAST fdec)
    mangleAST (Parser.FuncDef name args body) = Parser.FuncDef (from ++ "." ++ name) args (mangleAST body)
    mangleAST x = x
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
