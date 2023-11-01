-- module BytecodeCompiler (runTestProgram, locateLabel, printAssembly, compileProgram, CompilerState (..), Function) where
module BytecodeCompiler where

import AST qualified as Parser.Type (Type (Unknown))
import Control.Monad (when, (>=>))
import Control.Monad.State (MonadIO (liftIO), StateT, evalStateT, gets, modify)
import Data.Functor ((<&>))
import Data.List (elemIndex, find, intercalate)
import Data.Maybe (fromJust, isNothing)
import Data.Text (splitOn)
import Data.Text qualified
import Data.Text qualified as T
import Data.Vector qualified as V
import Foreign ()
import Foreign.C.Types ()
import GHC.Generics (Generic)
import Parser (CompilerFlags (CompilerFlags), name, parseProgram, types)
import Parser qualified
import Paths_indigo qualified
import System.Directory (doesFileExist)
import Text.Megaparsec (errorBundlePretty)
import VM
import Prelude hiding (lex)

data Function = Function
    { baseName :: String
    , funame :: String
    , function :: [Instruction]
    , types :: [Parser.Type]
    }
    deriving (Show, Generic)

data CompilerState = CompilerState
    { program :: Parser.Program
    , -- , functions :: [(String, String, [Instruction])]
      functions :: [Function]
    , funcDecs :: [Parser.Expr]
    , structDecs :: [Parser.Expr]
    , lastLabel :: Int
    , lets :: [(String, Parser.Type)]
    , traits :: [Parser.Expr]
    , impls :: [Parser.Expr]
    }
    deriving (Show)

initCompilerState :: Parser.Program -> CompilerState
initCompilerState prog = CompilerState prog [] [] [] 0 [] [] []

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
        let rootExpr = case parseProgram (T.pack i) CompilerFlags{verboseMode = False} of -- FIXME: pass on flags
                Left err -> error $ "Parse error: " ++ errorBundlePretty err
                Right (Parser.Program progExpr) -> progExpr
        concatMapM compileExpr rootExpr
    freePart <- concatMapM compileExpr expr
    createVirtualFunctions
    functions' <- gets functions >>= \x -> return $ concatMap function (reverse x)
    return $ prelude ++ functions' ++ freePart ++ [Exit]

compileProgramBare :: Parser.Program -> StateT CompilerState IO [Instruction]
compileProgramBare (Parser.Program expr) = do
    freePart <- concatMapM compileExpr expr
    functions' <- gets functions >>= \x -> return $ concatMap function (reverse x)
    return $ functions' ++ freePart ++ [Exit]

findAnyFunction :: String -> [Function] -> Maybe Function
findAnyFunction funcName xs = do
    -- TODO: I don't like this function
    let candidates = filter (\y -> baseName y == funcName && funame y /= "main") xs
    let ids = map ((!! 1) . splitOn "#" . Data.Text.pack . funame) candidates
    let ids' = map (read . Data.Text.unpack) ids :: [Int]
    let minId = minimum ids'
    find (\y -> funame y == funcName ++ "#" ++ show minId) candidates

findFunction :: String -> [Function] -> [Parser.Type] -> Maybe Function
findFunction funcName xs typess = do
    -- TODO: I don't like this function
    let candidates = filter (\y -> baseName y == funcName && funame y /= "main" && typesMatch y typess) xs
    let candidates' = case filter (`typesMatchExactly` typess) candidates of
            [] -> candidates
            candidates'' -> candidates''
    let ids = map ((!! 1) . splitOn "#" . Data.Text.pack . funame) candidates'
    let ids' = map (read . Data.Text.unpack) ids :: [Int]
    let minId = minimum ids'
    -- traceM $ "Looked for " ++ name ++ " with types " ++ show typess ++ " and found " ++ show candidates' ++ "\n"
    case find (\y -> funame y == funcName ++ "#" ++ show minId) candidates' of
        Just x -> Just x
        Nothing -> findAnyFunction funcName xs

typesMatch :: Function -> [Parser.Type] -> Bool
typesMatch fun typess = all (uncurry Parser.compareTypes) (zip (fun.types) typess) && length typess <= length (fun.types)

typesMatchExactly :: Function -> [Parser.Type] -> Bool
typesMatchExactly fun typess = all (uncurry (==)) (zip (fun.types) typess) && length typess <= length (fun.types)

unmangleFunctionName :: String -> String
unmangleFunctionName = takeWhile (/= '#')

-- FIXME: this is horrible
findSourceFile :: String -> IO String
findSourceFile fileName = do
    e1 <- doesFileExist fileName
    if e1
        then return fileName
        else do
            p2 <- Paths_indigo.getDataFileName fileName
            e2 <- doesFileExist p2
            if e2
                then return p2
                else do
                    e3 <- doesFileExist ("/usr/local/lib/indigo/" ++ fileName)
                    if e3
                        then return $ "/usr/local/lib/indigo/" ++ fileName
                        else do
                            e4 <- doesFileExist ("/usr/lib/indigo/" ++ fileName)
                            if e4
                                then return $ "/usr/lib/indigo/" ++ fileName
                                else error $ "Could not find file " ++ fileName

preludeFile :: IO String
preludeFile = findSourceFile "std/prelude.in" >>= readFile

doBinOp :: Parser.Expr -> Parser.Expr -> Instruction -> StateT CompilerState IO [Instruction]
doBinOp x y op = do
    functions' <- gets functions
    let f = findAnyFunction (Data.Text.unpack $ Data.Text.toLower $ Data.Text.pack $ show op) functions'
    x' <- compileExpr x
    y' <- compileExpr y
    cast <- case (x, y) of
        (Parser.Flexible _, Parser.Flexible _) -> error "Double cast"
        (Parser.Flexible _, _) -> return $ Cast : y'
        (_, Parser.Flexible _) -> return $ [Swp, Cast] ++ x'
        _ -> return []
    case (x, y) of
        (_, Parser.FuncCall _ _) -> return (y' ++ x' ++ [Swp] ++ cast ++ [op])
        (Parser.FuncCall _ _, _) -> return (x' ++ y' ++ cast ++ [op])
        (Parser.Placeholder, Parser.Placeholder) -> return [PushPf (funame $ fromJust f) 0]
        (Parser.Placeholder, _) -> return $ y' ++ [PushPf (funame $ fromJust f) 1] ---------------
        (_, Parser.Placeholder) -> return $ x' ++ [PushPf (funame $ fromJust f) 1]
        _ -> return (x' ++ y' ++ cast ++ [op])

typeOf :: Parser.Expr -> StateT CompilerState IO Parser.Type
typeOf (Parser.FuncCall funcName _) = do
    funcDecs' <- gets funcDecs
    let a =
            maybe
                [Parser.Type.Unknown]
                Parser.types
                (find (\x -> x.name == funcName) funcDecs')
    return $ last a
typeOf (Parser.Var varName) = do
    lets' <- gets lets
    let a = maybe Parser.Any snd (find (\x -> fst x == varName) lets')
    return a
typeOf x = return $ Parser.typeOf x

constructFQName :: String -> [Parser.Expr] -> StateT CompilerState IO String
constructFQName "main" _ = return "main"
constructFQName funcName args = mapM (typeOf >=> return . show) args <&> \x -> funcName ++ ":" ++ intercalate "," x

constructFQName' :: String -> [Parser.Type] -> StateT CompilerState IO String
constructFQName' "main" _ = return "main"
constructFQName' funcName args = return $ funcName ++ ":" ++ intercalate "," (map show args)

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
compileExpr (Parser.FuncCall funcName args) = do
    argTypes <- mapM typeOf args
    functions' <- gets functions
    funcDecs' <- gets funcDecs
    let fun = case findFunction funcName functions' argTypes of
            (Just f) -> f
            Nothing -> Function{baseName = unmangleFunctionName funcName, funame = funcName, function = [], types = []}
    let funcDec = find (\(Parser.FuncDec name' _) -> name' == baseName fun) funcDecs'
    case funcDec of
        (Just fd) -> do
            if length args == length (Parser.types fd) - 1
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
                        ++ [ LLoad funcName
                           , CallS
                           ]
compileExpr fd@(Parser.FuncDec _ _) = do
    modify (\s -> s{funcDecs = fd : funcDecs s})
    return [] -- Function declarations are only used for compilation
compileExpr (Parser.FuncDef name args body) = do
    -- let argTypes = map Parser.typeOf args
    funcDecs' <- gets funcDecs
    when (isNothing $ find (\(Parser.FuncDec name' _) -> name' == name) funcDecs') $ modify (\s -> s{funcDecs = Parser.FuncDec name (replicate (length args + 1) Parser.Any) : funcDecs s})

    funame <- if name /= "main" then ((name ++ "#") ++) . show <$> allocId else return "main"
    modify (\s -> s{functions = Function name funame [] [] : functions s})

    body' <- compileExpr body
    funcDecs'' <- gets funcDecs
    args' <- concatMapM (`compileParameter` name) (reverse (filter (/= Parser.Placeholder) args))
    let funcDec = fromJust $ find (\(Parser.FuncDec name' _) -> name' == name) funcDecs''
    let function = Label funame : args' ++ body' ++ ([Ret | name /= "main"])
    modify (\s -> s{functions = Function name funame function (funcDec.types) : tail (functions s)})
    return [] -- Function definitions get appended at the last stage of compilation
  where
    compileParameter :: Parser.Expr -> String -> StateT CompilerState IO [Instruction]
    compileParameter (Parser.Var varName) _ = return [LStore varName]
    compileParameter lex@(Parser.ListLit l) funcName = do
        nextFunName <- ((funcName ++ "#") ++) . show . (+ 1) <$> allocId
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
    let fun = findAnyFunction x functions'
    case fun of
        (Just f) -> compileExpr (Parser.FuncCall (funame f) [])
        Nothing -> return [LLoad x]
compileExpr (Parser.Let name value) = do
    typeOf value >>= \v -> modify (\s -> s{lets = (name, v) : lets s})
    value' <- compileExpr value
    return $ value' ++ [LStore name]
compileExpr (Parser.Function a b) = mapM_ compileExpr a >> compileExpr b >> return []
compileExpr (Parser.Flexible a) = compileExpr a >>= \a' -> return $ Meta "flex" : a'
compileExpr (Parser.ListConcat a b) = do
    a' <- compileExpr a
    b' <- compileExpr b
    let a'' = case a' of
            [Meta "flex", a'''] -> [a'''] ++ b' ++ [Cast]
            _ -> a'
    let b'' = case b' of
            [Meta "flex", b'''] -> [b'''] ++ a' ++ [Cast]
            _ -> b'
    return (b'' ++ a'' ++ [Concat 2])
compileExpr (Parser.ListLit elements) = concatMapM compileExpr elements >>= \elements' -> return (reverse elements' ++ [Concat $ length elements])
compileExpr (Parser.If ifCond ifThen ifElse) = do
    cond' <- compileExpr ifCond
    then' <- compileExpr ifThen
    else' <- compileExpr ifElse
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
compileExpr st@(Parser.Struct _ _) = do
    modify (\s -> s{structDecs = st : structDecs s})
    return []
compileExpr (Parser.StructLit name fields) = do
    fields' <- concatMapM compileExpr (map snd fields)
    let names = map (DString . fst) fields
    let instructions = zip names fields' >>= \(name', field) -> [Push name', field]
    return $ reverse instructions ++ [Push $ DString name, Push $ DString "__name", PackMap $ length instructions + 2]
compileExpr (Parser.StructAccess struct (Parser.Var field)) = do
    struct' <- compileExpr struct
    return $ struct' ++ [Access field]
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
    mangleAST (Parser.FuncDec name types) = Parser.FuncDec (from ++ "::" ++ name) types
    mangleAST (Parser.Function fdef dec) = Parser.Function (map mangleAST fdef) (mangleAST dec)
    mangleAST (Parser.FuncDef name args body) = Parser.FuncDef (from ++ "::" ++ name) args (mangleAST body)
    mangleAST x = x
compileExpr (Parser.Trait name methods) = do
    let methods' = map (\(Parser.FuncDec name' types) -> Parser.FuncDec name' types) methods
    modify (\s -> s{traits = Parser.Trait name methods : traits s})
    mapM_ compileExpr methods'
    return []
compileExpr (Parser.Impl name for methods) = do
    let methods' = map (\(Parser.FuncDef name' args body) -> Parser.FuncDef (name ++ "." ++ for ++ "::" ++ name') args body) methods
    modify (\s -> s{impls = Parser.Impl name for methods : impls s})
    mapM_ compileExpr methods'
    return []
compileExpr x = error $ show x ++ " is not implemented"

createVirtualFunctions :: StateT CompilerState IO ()
createVirtualFunctions = do
    impls' <- gets impls
    traits' <- gets traits
    let traitsAssoc = map (\x -> (x, filter (\y -> Parser.trait y == Parser.name x) impls')) traits'
    mapM_ compileTrait traitsAssoc
  where
    compileTrait :: (Parser.Expr, [Parser.Expr]) -> StateT CompilerState IO ()
    compileTrait (trait, impl) = do
        let methods = Parser.methods trait
        let fors = map Parser.for impl
        mapM_ (\method -> createBaseDef method trait fors) methods
    createBaseDef :: Parser.Expr -> Parser.Expr -> [String] -> StateT CompilerState IO ()
    createBaseDef (Parser.FuncDec name typess) trait fors = do
        let traitName = Parser.name trait
        let body =
                [Label name]
                    ++ replicate (length typess - 2) Pop
                    ++ [ DupN (length fors)
                       ]
                    ++ concatMap (\for -> [TypeOf, Push $ DString for, Eq, Jt (traitName ++ "." ++ for ++ "::" ++ name)]) fors
                    ++ [Pop, Push $ DString ("`" ++ name ++ "` from trait `" ++ traitName ++ "` called for unimplemented type"), Panic]
        modify (\s -> s{functions = functions s ++ [Function name (name ++ "#0") body typess]})
        return ()
    createBaseDef _ _ _ = return ()

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
            xxx <- evalStateT (compileProgram program) (initCompilerState program)
            -- print program
            putStrLn $ printAssembly (V.fromList xxx) True
            let xxxPoint = locateLabel xxx "main"
            runVM $ (initVM (V.fromList xxx)){pc = xxxPoint, breakpoints = [], callStack = [StackFrame{returnAddress = xxxPoint, locals = []}]}
