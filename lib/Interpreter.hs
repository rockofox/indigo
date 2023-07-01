module Interpreter (run, fromBytecode, toBytecode, pop, interpret, repl, initialState) where

import Control.Exception (Exception (toException), throw, try)
import Control.Exception.Base ()
import Control.Monad.Error
import Control.Monad.State (MonadState (get, put), StateT (runStateT), gets)
import Data.Binary (decode, encode)
import Data.ByteString.Lazy (LazyByteString)
import Data.Data (Typeable)
import Data.List (find, intercalate)
import Data.Maybe (isNothing)
import Data.Text (replace)
import Data.Text qualified
import GHC.IO.Handle (hFlush)
import GHC.IO.Handle.FD (stdout)
import Parser (CompilerFlags (CompilerFlags, verboseMode), Expr (..), Program (..), Type (..), parseProgram)
import Parser qualified as Type
import Paths_prisma qualified
import Text.Megaparsec

data VarTableEntry = VarTableEntry
    { name :: String
    , value :: Value
    , function :: String
    }
    deriving (Show)

data InterpreterState = InterpreterState
    { stack :: [Value]
    , heap :: [Value]
    , functions :: [Expr]
    , currentFunction :: String
    , variables :: [VarTableEntry]
    , structs :: [Expr]
    }
    deriving (Show)

data Value
    = IntValue Int
    | FloatValue Float
    | StringValue String
    | BoolValue Bool
    | PartialFunction {func :: Expr, args :: [Value]}
    | ListValue {values :: [Value]}
    | StructValue {struct :: String, fields :: [(String, Value)]}
    | IOValue
    | UnitValue
    deriving (Eq)

instance Show Value where
    show (IntValue i) = show i
    show (FloatValue f) = show f
    show (StringValue s) = s
    show (BoolValue b) = show b
    show (PartialFunction func args) = "<partial function: " ++ fname (fdec func) ++ " " ++ show args ++ ">"
    show (ListValue l) = "[" ++ intercalate "," (map show l) ++ "]"
    show (StructValue struct fields) = "<struct " ++ struct ++ " " ++ show fields ++ ">"
    show UnitValue = "()"
    show IOValue = "<IO>"

pop :: [a] -> [a]
pop [] = []
pop (x : xs) = xs

popN :: Int -> [a] -> [a]
popN 0 xs = xs
popN n xs = popN (n - 1) (pop xs)

valueToExpr :: Value -> Expr
valueToExpr (IntValue i) = IntLit (toInteger i)
valueToExpr (StringValue s) = StringLit s
valueToExpr (FloatValue f) = FloatLit f
valueToExpr (BoolValue b) = BoolLit b
valueToExpr (PartialFunction func args) = FuncCall (fname (fdec func)) (map valueToExpr args)
valueToExpr (ListValue l) = ListLit (map valueToExpr l)
valueToExpr (StructValue struct fields) = StructLit struct (map (\(name, value) -> (name, valueToExpr value)) fields)
valueToExpr IOValue = IOLit
valueToExpr UnitValue = Placeholder

typeOfV :: Value -> Type
typeOfV (IntValue _) = Type.Int
typeOfV (FloatValue _) = Type.Float
typeOfV (StringValue _) = Type.String
typeOfV (BoolValue _) = Type.Bool
typeOfV (PartialFunction func args) = Type.Fn{Type.args = popN (length args + 1) $ ftypes $ fdec func, Type.ret = last $ ftypes $ fdec func}
typeOfV (ListValue l) = Type.List $ if not (null l) then typeOfV $ head l else Type.Any
typeOfV (StructValue struct _) = Type.StructT struct
typeOfV IOValue = Type.IO
typeOfV _ = Type.Any

lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

stackPop :: StateT InterpreterState IO Value
stackPop = do
    state <- get
    put $ state{stack = pop $ stack state}
    if null (stack state)
        then return UnitValue
        else return $ head $ stack state

stackPopN :: Int -> StateT InterpreterState IO [Value]
stackPopN 0 = return []
stackPopN n = do
    state <- get
    mapM_ (const stackPop) [1 .. n]
    return $ take n $ stack state

stackPeekN :: Int -> StateT InterpreterState IO [Value]
stackPeekN 0 = return []
stackPeekN n = do
    gets (take n . stack)

stackPeek :: StateT InterpreterState IO Value
stackPeek = do
    state <- get
    if null (stack state)
        then return UnitValue
        else return $ head $ stack state

stackPush :: Value -> StateT InterpreterState IO ()
stackPush value = do
    state <- get
    put $ state{stack = value : stack state}

interpretAndPop :: [Expr] -> StateT InterpreterState IO [Value]
interpretAndPop exprs = do
    mapM_ interpret exprs
    stackPopN (length exprs)

showFunction :: String -> [Type] -> Bool -> String
showFunction name types argsOnly = do
    if not argsOnly
        then do
            let args = pop types
            let ret = if null types then None else head types
            name ++ " " ++ intercalate " -> " (map show args) ++ " => " ++ show ret
        else do
            name ++ " " ++ intercalate " -> " (map show types) ++ " => ?"

preludeFile :: IO String
preludeFile = Paths_prisma.getDataFileName "std/prelude.prism" >>= readFile

toBytecode :: Program -> LazyByteString
toBytecode (Program exprs) = encode exprs

fromBytecode :: LazyByteString -> Program
fromBytecode = decode

initialState :: InterpreterState
initialState = InterpreterState{stack = [], heap = [], functions = [], variables = [], currentFunction = "", structs = []}

newtype RuntimeError = RuntimeError String
    deriving (Show, Typeable)

instance Exception RuntimeError

runtimeError :: String -> StateT InterpreterState IO ()
runtimeError err = throw $ RuntimeError err

run :: Program -> IO ()
run (Program exprs) = do
    prelude <- liftIO $ (parseProgram . Data.Text.pack <$> preludeFile) <*> pure CompilerFlags{verboseMode = False}
    case prelude of
        Left err -> error $ errorBundlePretty err
        Right (Program preludeExprs) -> do
            let mainCall = FuncCall "main" []
            _ <- runStateT (mapM_ interpret (preludeExprs ++ exprs ++ [mainCall])) initialState
            return ()

interpret :: Expr -> StateT InterpreterState IO ()
interpret (DoBlock exprs) = do
    mapM_ interpret exprs
-- Operators
interpret (Eq x y) = do
    [a, b] <- interpretAndPop [x, y]
    interpret $ FuncCall "eq" [valueToExpr a, valueToExpr b]
interpret (Add x y) = do
    [a, b] <- interpretAndPop [x, y]
    interpret $ FuncCall "add" [valueToExpr a, valueToExpr b]
interpret (Sub x y) = do
    [a, b] <- interpretAndPop [x, y]
    interpret $ FuncCall "sub" [valueToExpr a, valueToExpr b]
interpret (Mul x y) = do
    [a, b] <- interpretAndPop [x, y]
    interpret $ FuncCall "mul" [valueToExpr a, valueToExpr b]
interpret (Div x y) = do
    [a, b] <- interpretAndPop [x, y]
    interpret $ FuncCall "div" [valueToExpr a, valueToExpr b]
interpret (Power x y) = do
    [a, b] <- interpretAndPop [x, y]
    interpret $ FuncCall "pow" [valueToExpr a, valueToExpr b]
interpret (UnaryMinus x) = do
    [a] <- interpretAndPop [x]
    interpret $ FuncCall "neg" [valueToExpr a]
-- Internal function
interpret (FuncCall "println" [arg]) = do
    interpret arg
    value <- stackPop
    liftIO $ print value
interpret (FuncCall "println" _) = runtimeError "println takes exactly one argument"
interpret (FuncCall "print" [arg]) = do
    interpret arg
    value <- stackPop
    liftIO $ putStr $ show value
interpret (FuncCall "print" _) = runtimeError "print takes exactly one argument"
interpret (FuncCall "internal_eq" [arg1, arg2]) = do
    [value1, value2] <- interpretAndPop [arg1, arg2]
    case (value1, value2) of
        (IntValue i1, IntValue i2) -> stackPush $ BoolValue (i1 == i2)
        (FloatValue f1, FloatValue f2) -> stackPush $ BoolValue (f1 == f2)
        (StringValue s1, StringValue s2) -> stackPush $ BoolValue (s1 == s2)
        (BoolValue b1, BoolValue b2) -> stackPush $ BoolValue (b1 == b2)
        (UnitValue, UnitValue) -> stackPush $ BoolValue True
        _ -> stackPush $ BoolValue False
interpret (FuncCall "internal_neg" [arg]) = do
    [value] <- interpretAndPop [arg]
    case value of
        IntValue i -> stackPush $ IntValue (-i)
        FloatValue f -> stackPush $ FloatValue (-f)
        _ -> runtimeError "Cannot negate value of non-numeric type"
interpret (FuncCall "internal_pow" [arg1, arg2]) = do
    [value1, value2] <- interpretAndPop [arg1, arg2]
    case (value1, value2) of
        (IntValue i1, IntValue i2) -> stackPush $ IntValue (i1 ^ i2)
        (FloatValue f1, FloatValue f2) -> stackPush $ FloatValue (f1 ** f2)
        _ -> runtimeError "Cannot raise values of different types"
interpret (FuncCall "internal_add" [arg1, arg2]) = do
    [value1, value2] <- interpretAndPop [arg1, arg2]
    case (value1, value2) of
        (IntValue i1, IntValue i2) -> stackPush $ IntValue (i1 + i2)
        (FloatValue f1, FloatValue f2) -> stackPush $ FloatValue (f1 + f2)
        (StringValue s1, StringValue s2) -> stackPush $ StringValue (s1 ++ s2)
        _ -> runtimeError "Cannot add values of different types"
interpret (FuncCall "internal_sub" [arg1, arg2]) = do
    [value1, value2] <- interpretAndPop [arg1, arg2]
    case (value1, value2) of
        (IntValue i1, IntValue i2) -> stackPush $ IntValue (i1 - i2)
        (FloatValue f1, FloatValue f2) -> stackPush $ FloatValue (f1 - f2)
        _ -> runtimeError "Cannot subtract values of different types"
interpret (FuncCall "internal_mul" [arg1, arg2]) = do
    [value1, value2] <- interpretAndPop [arg1, arg2]
    case (value1, value2) of
        (IntValue i1, IntValue i2) -> stackPush $ IntValue (i1 * i2)
        (FloatValue f1, FloatValue f2) -> stackPush $ FloatValue (f1 * f2)
        _ -> runtimeError "Cannot multiply values of different types"
interpret (FuncCall "internal_div" [arg1, arg2]) = do
    [value1, value2] <- interpretAndPop [arg1, arg2]
    case (value1, value2) of
        (IntValue i1, IntValue i2) -> stackPush $ IntValue (i1 `div` i2)
        (FloatValue f1, FloatValue f2) -> stackPush $ FloatValue (f1 / f2)
        _ -> runtimeError "Cannot divide values of different types"
interpret f@(Function _ _) = do
    state <- get
    put $ state{functions = f : functions state}
interpret (FuncCall name a) = do
    state <- get
    let args = filter (/= Placeholder) a
    mapM_ interpret $ reverse args
    interpretedArgs <- stackPopN (length args)
    let argumentTypes = map typeOfV interpretedArgs
    let argumentTypes' = if argumentTypes == [None] then [] else argumentTypes
    let availableFunctions = filter (\(Function _ dec) -> fname dec == name) [mf | mf@(Function _ _) <- functions state]
    let function = find (\(Function _ dec) -> take (length argumentTypes') (ftypes dec) == argumentTypes') availableFunctions
    case function of
        Just function -> do
            let definition = find (\x -> all (\(formal, actual) -> case formal of (Var _) -> True; (ListPattern _) -> True; _ -> formal == valueToExpr actual) $ zip (fargs x) interpretedArgs) $ fdef function
            case definition of
                Just definiton -> do
                    if length (ftypes $ fdec function) - 1 == length argumentTypes'
                        then do
                            if init (ftypes $ fdec function) == argumentTypes'
                                then do
                                    stackToVariables (fargs definiton) interpretedArgs
                                    interpret $ fbody definiton
                                else runtimeError $ "Function " ++ showFunction name argumentTypes' True ++ " not found (wrong argument types)"
                        else do
                            stackPush $ PartialFunction{func = function, args = interpretedArgs}
                Nothing -> runtimeError $ "Function " ++ showFunction name argumentTypes' True ++ " not found (wrong number of arguments)"
        Nothing -> do
            let vfunc = find (\(VarTableEntry vname _ vfunction) -> vname == name && vfunction == currentFunction state) [v | v@(VarTableEntry{}) <- variables state]
            case vfunc of
                Just vfunc -> do
                    case value vfunc of
                        PartialFunction{func = func, args = partialArgs} -> do
                            let newArgs = interpretedArgs ++ partialArgs
                            stackToVariables (fargs $ head $ fdef func) $ reverse newArgs
                            interpret $ fbody $ head $ fdef func
                        _ -> runtimeError $ "Value " ++ name ++ " is not a function"
                Nothing -> runtimeError $ "Function " ++ showFunction name argumentTypes' True ++ " not found"
            return ()
  where
    stackToVariables formal actual = do
        let stuff = zip formal actual
        mapM_
            ( \(formal, actual) -> do
                interpret $ Let formal $ valueToExpr actual
            )
            [(x, y) | (Var x, y) <- stuff]
        mapM_
            ( \(pattern, list) -> do
                if length list /= length pattern - 1
                    then do
                        mapM_ (\(a, b) -> interpret $ Let a $ valueToExpr b) $ zip (init pattern) (init list)
                        interpret (Let (last pattern) (valueToExpr (ListValue (drop (length (init pattern)) list))))
                    else do
                        mapM_ (\(a, b) -> interpret $ Let a $ valueToExpr b) $ zip pattern list
                        interpret (Let (last pattern) (valueToExpr (ListValue [])))
            )
            [(x, y) | (ListPattern x, ListValue y) <- stuff]
interpret (IntLit i) = stackPush $ IntValue (fromIntegral i)
interpret (FloatLit f) = stackPush $ FloatValue f
interpret (StringLit s) = stackPush $ StringValue s
interpret (Var vname) = do
    state <- get
    let vvalue = find (\v -> name v == vname && function v == currentFunction state) [v | v@(VarTableEntry{}) <- variables state]
    case vvalue of
        Just vvalue -> stackPush $ value vvalue
        Nothing -> runtimeError $ "Variable " ++ vname ++ " not found"
interpret (Let name expr) = do
    interpret expr
    state <- get
    value <- stackPeek
    put $ state{stack = pop $ stack state, variables = (VarTableEntry{value = value, name = name, function = currentFunction state}) : variables state}
interpret (If cond thenExpr elseExpr) = do
    interpret cond
    value <- stackPop
    case value of
        BoolValue True -> interpret thenExpr
        BoolValue False -> interpret elseExpr
        _ -> runtimeError "Condition must be of type Bool"
interpret (BoolLit b) = do
    stackPush $ BoolValue b
interpret (ExternDec{}) = return ()
interpret Placeholder = return ()
interpret (Then x y) = do
    interpret x
    interpret y
interpret (Bind a@(FuncCall _ _) (FuncCall bname bargs)) = do
    interpret a
    aResult <- stackPop
    interpret $ FuncCall bname (bargs ++ [valueToExpr aResult])
interpret (Bind a b) = do
    interpret a
    interpret b
interpret (ListLit expr) = do
    mapM_ interpret expr
    list <- stackPopN $ length expr
    stackPush $ ListValue $ reverse list
interpret (FuncDec name types) = do
    state <- get
    put $ state{functions = Function{fdec = FuncDec{fname = name, ftypes = types}, fdef = []} : functions state}
interpret (FuncDef name args body) = do
    state <- get
    let ffunc = find (\(Function _ dec) -> fname dec == name) [mf | mf@(Function _ _) <- functions state]
    case ffunc of
        Nothing -> runtimeError $ "No function declaration found for function " ++ name
        Just ffunc -> put $ state{functions = Function{fdec = fdec ffunc, fdef = fdef ffunc ++ [FuncDef{fname = name, fbody = body, fargs = args}]} : filter (\(Function _ dec) -> fname dec /= name) (functions state)}
interpret (ListConcat a b) = do
    interpret a
    interpret b
    b <- stackPop
    a <- stackPop
    case (a, b) of
        (_, ListValue b) -> stackPush $ ListValue $ a : b
        (ListValue a, _) -> stackPush $ ListValue $ a ++ [b]
        (StringValue a, _) -> stackPush $ StringValue $ a ++ show b
        (_, StringValue b) -> stackPush $ StringValue $ show a ++ b
        _ -> runtimeError $ "Cannot concatenate values of different types. Tried to concatenate " ++ show a ++ " and " ++ show b
interpret s@(Struct _ _) = do
    state <- get
    put $ state{structs = s : structs state}
interpret (StructLit name values) = do
    state <- get
    interpretedValues <- mapM (\(name, value) -> do interpret value; v <- stackPop; return (name, v)) values
    let struct = find (\(Struct sname _) -> sname == name) [s | s@(Struct{}) <- structs state]
    when (isNothing struct) $ runtimeError $ "Struct " ++ name ++ " not found"
    stackPush $ StructValue{struct = name, fields = interpretedValues}
interpret (StructAccess struct (Var field)) = do
    interpret struct
    struct <- stackPop
    case struct of
        StructValue{struct = sname, fields = fields} -> do
            let value = lookup field fields
            case value of
                Just value -> stackPush value
                Nothing -> runtimeError $ "Field " ++ field ++ " not found in struct " ++ sname
        _ -> runtimeError $ "Cannot access field " ++ field ++ " of non-struct value " ++ show struct
interpret (Import objects name) = do
    when (objects /= ["*"]) $ runtimeError "Only wildcard imports are supported right now"
    let file = Data.Text.unpack (replace "." "/" (Data.Text.pack name)) ++ ".prism"
    contents <- liftIO $ readFile file
    let parsed = parseProgram (Data.Text.pack contents) CompilerFlags{verboseMode = False}
    case parsed of
        Left err -> runtimeError $ errorBundlePretty err
        Right (Program exprs) -> do
            let treatedProgram = map (rewriteExpr name) exprs
            mapM_ interpret treatedProgram
  where
    rewriteExpr :: String -> Expr -> Expr
    rewriteExpr name (FuncCall fname args) = FuncCall (name ++ "." ++ fname) args
    rewriteExpr name (FuncDec fname ftypes) = FuncDec (name ++ "." ++ fname) (map (rewriteType name) ftypes)
    rewriteExpr name (FuncDef fname fargs fbody) = FuncDef (name ++ "." ++ fname) fargs fbody
    rewriteExpr name (Function def dec) = Function (map (rewriteExpr name) def) (rewriteExpr name dec)
    rewriteExpr name (Struct sname fields) = Struct (name ++ "." ++ sname) fields
    rewriteExpr _ x = x
    rewriteType :: String -> Type -> Type
    rewriteType name (StructT sname) = StructT (name ++ "." ++ sname)
    rewriteType _ x = x
interpret (Lambda args body) = do
    state <- get
    let name = "lambda" ++ show (length (functions state))
    let dec = FuncDec name ([Any | _ <- args] ++ [Any])
    let def = [FuncDef name args body]
    let function = Function{fdef = def, fdec = dec}
    interpret function
    stackPush $ PartialFunction{func = function, args = []}
interpret IOLit = do
    stackPush IOValue
interpret ex = runtimeError $ "Unimplemented expression: " ++ show ex

-- REPL
read_ :: IO String
read_ = do
    putStr "> "
    hFlush stdout
    getLine

eval_ :: String -> [Expr] -> StateT InterpreterState IO ()
eval_ input prelude = do
    let parsed = parseProgram (Data.Text.pack input) CompilerFlags{verboseMode = False}
    case parsed of
        Left err -> liftIO $ putStrLn $ errorBundlePretty err
        Right (Program exprs) -> do
            mapM_ interpretCatch (prelude ++ exprs)
            stackPop >>= liftIO . print
  where
    interpretCatch :: Expr -> StateT InterpreterState IO ()
    interpretCatch expr = do
        s <- get
        result <- liftIO $ Control.Exception.try $ runStateT (interpret expr) s
        case result of
            Left (RuntimeError ex) -> liftIO $ putStrLn ex
            Right x -> put $ snd x

repl :: StateT InterpreterState IO ()
repl = do
    prelude <- liftIO $ (parseProgram . Data.Text.pack <$> preludeFile) <*> pure CompilerFlags{verboseMode = False}
    case prelude of
        Left err -> runtimeError $ errorBundlePretty err
        Right (Program preludeExprs) -> do
            input <- liftIO read_
            case input of
                ":quit" -> return ()
                ":exit" -> return ()
                ":help" -> liftIO $ putStrLn "Prism REPL\nType ':quit' or ':exit' to exit"
                ":fn" -> do
                    functions <- gets functions
                    liftIO $ print functions
                    liftIO $ mapM_ (\x -> putStrLn $ showFunction (fname $ fdec x) (ftypes $ fdec x) True) functions
                ":state" -> do
                    state <- get
                    liftIO $ print state
                _ -> eval_ input preludeExprs
            repl