module Interpreter (run, fromBytecode, toBytecode) where

import CEmitter (pop)
import Control.Monad.State (MonadIO (liftIO), MonadState (get, put), StateT (runStateT))
import Data.Binary (decode, encode)
import Data.ByteString.Lazy (LazyByteString)
import Data.List (find, intercalate)
import Data.Maybe
import Data.Text qualified
import Parser (CompilerFlags (CompilerFlags, verboseMode), Expr (..), Program (..), Type (..), parseProgram, typeOf)
import Text.Megaparsec

data VarTableEntry = VarTableEntry
    { name :: String
    , value :: Value
    , function :: String
    }

data InterpreterState = InterpreterState
    { stack :: [Value]
    , heap :: [Value]
    , functions :: [Expr]
    , currentFunction :: String
    , variables :: [VarTableEntry]
    }

data Value
    = IntValue Int
    | StringValue String
    | UnitValue

instance Show Value where
    show (IntValue i) = show i
    show (StringValue s) = s
    show UnitValue = "()"

valueToExpr :: Value -> Expr
valueToExpr (IntValue i) = IntLit (toInteger i)
valueToExpr (StringValue s) = StringLit s
valueToExpr UnitValue = Placeholder

stackPop :: StateT InterpreterState IO Value
stackPop = do
    state <- get
    put $ state{stack = pop $ stack state}
    state <- get
    let val = last (stack state)
    return val

showFunction :: String -> [Type] -> Bool -> String
showFunction name types argsOnly = do
    if not argsOnly
        then do
            let args = pop types
            let ret = head types
            name ++ " " ++ intercalate " -> " (map show args) ++ " => " ++ show ret
        else do
            name ++ " " ++ intercalate " -> " (map show types) ++ " => ?"

-- prelude :: Either (ParseErrorBundle String Void) Program
prelude =
    parseProgram
        ( Data.Text.pack
            ( unlines
                [ "extern runtime internal_add :: Int -> Int => Int"
                , "add x:Int -> y:Int => Int = internal_add x,y"
                ]
            )
        )
        CompilerFlags{verboseMode = False}

toBytecode :: Program -> LazyByteString
toBytecode (Program exprs) = encode exprs

fromBytecode :: LazyByteString -> Program
fromBytecode = decode

run :: Program -> IO ()
run (Program exprs) = do
    let initialState = InterpreterState{stack = [], heap = [], functions = [], variables = [], currentFunction = ""}
    case prelude of
        Left err -> error $ errorBundlePretty err
        Right (Program preludeExprs) -> do
            let mainCall = FuncCall "main" []
            _ <- runStateT (mapM_ interpret (preludeExprs ++ exprs ++ [mainCall])) initialState
            return ()

interpret :: Expr -> StateT InterpreterState IO ()
interpret (DoBlock exprs) = do
    mapM_ interpret exprs
interpret (Add x y) = do
    interpret x
    interpret y
    state <- get
    interpret $ FuncCall "add" [valueToExpr $ head $ stack state, valueToExpr $ head $ pop $ stack state]
interpret (Sub x y) = do
    interpret x
    interpret y
    state <- get
    interpret $ FuncCall "sub" [valueToExpr $ head $ stack state, valueToExpr $ head $ pop $ stack state]
interpret (Mul x y) = do
    interpret x
    interpret y
    state <- get
    interpret $ FuncCall "mul" [valueToExpr $ head $ stack state, valueToExpr $ head $ pop $ stack state]
interpret (Div x y) = do
    interpret x
    interpret y
    state <- get
    interpret $ FuncCall "div" [valueToExpr $ head $ stack state, valueToExpr $ head $ pop $ stack state]
interpret f@(ModernFunc def dec) = do
    state <- get
    put $ state{functions = f : functions state}
interpret (FuncCall "println" [arg]) = do
    interpret arg
    state <- get
    let value = head $ stack state
    liftIO $ print value
interpret (FuncCall "println" _) = error "println takes exactly one argument"
interpret (FuncCall "internal_add" [arg1, arg2]) = do
    interpret arg1
    interpret arg2
    state <- get
    value1 <- stackPop
    value2 <- stackPop
    case (value1, value2) of
        (IntValue i1, IntValue i2) -> put $ state{stack = IntValue (i1 + i2) : stack state}
        (StringValue s1, StringValue s2) -> put $ state{stack = StringValue (s1 ++ s2) : stack state}
        _ -> error "Cannot add values of different types"
interpret (FuncCall "__sub" [arg1, arg2]) = do
    interpret arg1
    interpret arg2
    state <- get
    let value1 = head $ stack state
    let value2 = head $ pop $ stack state
    case (value1, value2) of
        (IntValue i1, IntValue i2) -> put $ state{stack = IntValue (i1 - i2) : stack state}
        _ -> error "Cannot subtract values of different types"
interpret (FuncCall "__mul" [arg1, arg2]) = do
    interpret arg1
    interpret arg2
    state <- get
    let value1 = head $ stack state
    let value2 = head $ pop $ stack state
    case (value1, value2) of
        (IntValue i1, IntValue i2) -> put $ state{stack = IntValue (i1 * i2) : stack state}
        _ -> error "Cannot multiply values of different types"
interpret (FuncCall "__div" [arg1, arg2]) = do
    interpret arg1
    interpret arg2
    state <- get
    let value1 = head $ stack state
    let value2 = head $ pop $ stack state
    case (value1, value2) of
        (IntValue i1, IntValue i2) -> put $ state{stack = IntValue (i1 `div` i2) : stack state}
        _ -> error "Cannot divide values of different types"
interpret (FuncCall name args) = do
    state <- get
    mapM_ interpret args
    stackToVariables
    let argumentTypes = map typeOf args
    let argumentTypes' = if argumentTypes == [None] then [] else argumentTypes
    -- let func = fromMaybe (error $ "Function " ++ name ++ " not found") $ find (\(ModernFunc def _) -> fname def == name) [mf | mf@(ModernFunc _ _) <- functions state]
    let func = fromMaybe (error $ "Function " ++ name ++ "(" ++ showFunction name argumentTypes' True ++ ") not found") $ find (\(ModernFunc def dec) -> fname def == name && pop (ftypes dec) == argumentTypes') [mf | mf@(ModernFunc _ _) <- functions state]
    interpret $ fbody $ fdef func
  where
    stackToVariables = do
        state <- get
        let argumentTypes = map typeOf args
        let argumentTypes' = if argumentTypes == [None] then [] else argumentTypes
        let function = fromMaybe (error $ "Function " ++ showFunction name argumentTypes' True ++ " not found") $ find (\(ModernFunc def _) -> fname def == name) [mf | mf@(ModernFunc _ _) <- functions state]
        mapM
            ( \x -> do
                value <- stackPop
                interpret $ Let x $ valueToExpr value
            )
            $ reverse
            $ fargs
            $ fdef function
interpret (IntLit i) = do
    state <- get
    put $ state{stack = IntValue (fromIntegral i) : stack state}
interpret (StringLit s) = do
    state <- get
    put $ state{stack = StringValue s : stack state}
interpret (Var vname) = do
    state <- get
    let vvalue = fromMaybe (error $ "Variable " ++ vname ++ " not found") $ find (\v -> name v == vname && function v == currentFunction state) [v | v@(VarTableEntry{}) <- variables state]
    put $ state{stack = value vvalue : stack state}
interpret (Let name expr) = do
    interpret expr
    state <- get
    let value = head $ stack state
    put $ state{stack = pop $ stack state, variables = (VarTableEntry{value = value, name = name, function = currentFunction state}) : variables state}
interpret _ = return ()