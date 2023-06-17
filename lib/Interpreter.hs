module Interpreter (run, fromBytecode, toBytecode, pop) where

import Control.Monad.State (MonadIO (liftIO), MonadState (get, put), StateT (runStateT), gets, when)
import Data.Binary (decode, encode)
import Data.ByteString.Lazy (LazyByteString)
import Data.List (find, intercalate)
import Data.Maybe
import Data.Text qualified
import Debug.Trace (traceShow, traceShowM)
import Parser (CompilerFlags (CompilerFlags, verboseMode), Expr (..), Program (..), Type (..), parseProgram)
import Parser qualified as Type
import Paths_prisma qualified
import Text.Megaparsec

data VarTableEntry = VarTableEntry
  { name :: String,
    value :: Value,
    function :: String
  }
  deriving (Show)

data InterpreterState = InterpreterState
  { stack :: [Value],
    heap :: [Value],
    functions :: [Expr],
    currentFunction :: String,
    variables :: [VarTableEntry]
  }
  deriving (Show)

data Value
  = IntValue Int
  | FloatValue Float
  | StringValue String
  | BoolValue Bool
  | PartialFunction {func :: Expr, args :: [Value]}
  | ListValue {values :: [Value]}
  | UnitValue
  deriving (Eq)

instance Show Value where
  show (IntValue i) = show i
  show (FloatValue f) = show f
  show (StringValue s) = s
  show (BoolValue b) = show b
  show (PartialFunction func args) = "<partial function: " ++ fname (fdec func) ++ " " ++ show args ++ ">"
  show (ListValue l) = "[" ++ intercalate "," (map show l) ++ "]"
  show UnitValue = "()"

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
valueToExpr UnitValue = Placeholder

typeOfV :: Value -> Type
typeOfV (IntValue _) = Type.Int
typeOfV (FloatValue _) = Type.Float
typeOfV (StringValue _) = Type.String
typeOfV (BoolValue _) = Type.Bool
typeOfV (PartialFunction func args) = Type.Fn {Type.args = popN (length args + 1) $ ftypes $ fdec func, Type.ret = last $ ftypes $ fdec func}
typeOfV (ListValue l) = Type.List $ if not (null l) then typeOfV $ head l else Type.Any
typeOfV _ = Type.Any

lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

stackPop :: StateT InterpreterState IO Value
stackPop = do
  state <- get
  put $ state {stack = pop $ stack state}
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
  put $ state {stack = value : stack state}

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

run :: Program -> IO ()
run (Program exprs) = do
  let initialState = InterpreterState {stack = [], heap = [], functions = [], variables = [], currentFunction = ""}
  prelude <- liftIO $ (parseProgram . Data.Text.pack <$> preludeFile) <*> pure CompilerFlags {verboseMode = False}
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
interpret (FuncCall "println" _) = error "println takes exactly one argument"
interpret (FuncCall "print" [arg]) = do
  interpret arg
  value <- stackPop
  liftIO $ putStr $ show value
interpret (FuncCall "print" _) = error "print takes exactly one argument"
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
    _ -> error "Cannot negate value of non-numeric type"
interpret (FuncCall "internal_pow" [arg1, arg2]) = do
  [value1, value2] <- interpretAndPop [arg1, arg2]
  case (value1, value2) of
    (IntValue i1, IntValue i2) -> stackPush $ IntValue (i1 ^ i2)
    (FloatValue f1, FloatValue f2) -> stackPush $ FloatValue (f1 ** f2)
    _ -> error "Cannot raise values of different types"
interpret (FuncCall "internal_add" [arg1, arg2]) = do
  [value1, value2] <- interpretAndPop [arg1, arg2]
  case (value1, value2) of
    (IntValue i1, IntValue i2) -> stackPush $ IntValue (i1 + i2)
    (FloatValue f1, FloatValue f2) -> stackPush $ FloatValue (f1 + f2)
    (StringValue s1, StringValue s2) -> stackPush $ StringValue (s1 ++ s2)
    _ -> error "Cannot add values of different types"
interpret (FuncCall "internal_sub" [arg1, arg2]) = do
  [value1, value2] <- interpretAndPop [arg1, arg2]
  case (value1, value2) of
    (IntValue i1, IntValue i2) -> stackPush $ IntValue (i1 - i2)
    (FloatValue f1, FloatValue f2) -> stackPush $ FloatValue (f1 - f2)
    _ -> error "Cannot subtract values of different types"
interpret (FuncCall "internal_mul" [arg1, arg2]) = do
  [value1, value2] <- interpretAndPop [arg1, arg2]
  case (value1, value2) of
    (IntValue i1, IntValue i2) -> stackPush $ IntValue (i1 * i2)
    (FloatValue f1, FloatValue f2) -> stackPush $ FloatValue (f1 * f2)
    _ -> error "Cannot multiply values of different types"
interpret (FuncCall "internal_div" [arg1, arg2]) = do
  [value1, value2] <- interpretAndPop [arg1, arg2]
  case (value1, value2) of
    (IntValue i1, IntValue i2) -> stackPush $ IntValue (i1 `div` i2)
    (FloatValue f1, FloatValue f2) -> stackPush $ FloatValue (f1 / f2)
    _ -> error "Cannot divide values of different types"
interpret f@(ModernFunc _ _) = do
  state <- get
  put $ state {functions = f : functions state}
interpret (FuncCall name a) = do
  state <- get
  let args = filter (/= Placeholder) a
  mapM_ interpret $ reverse args
  interpretedArgs <- stackPopN (length args)
  let argumentTypes = map typeOfV interpretedArgs
  let argumentTypes' = if argumentTypes == [None] then [] else argumentTypes
  let ffunc = find (\(ModernFunc _ dec) -> fname dec == name) [mf | mf@(ModernFunc _ _) <- functions state]
  case ffunc of
    Just ffunc -> do
      let ffdef = find (\x -> all (\(formal, actual) -> case formal of (Var _) -> True; (ListPattern _) -> True; _ -> formal == valueToExpr actual) $ zip (fargs x) interpretedArgs) $ fdef ffunc
      case ffdef of
        Just def -> do
          if length (ftypes $ fdec ffunc) - 1 == length argumentTypes'
            then do
              if init (ftypes $ fdec ffunc) == argumentTypes'
                then do
                  stackToVariables (fargs def) interpretedArgs
                  interpret $ fbody def
                else error $ "Function " ++ showFunction name argumentTypes' True ++ " not found"
            else do
              stackPush $ PartialFunction {func = ffunc, args = interpretedArgs}
        Nothing -> error $ "Function " ++ showFunction name argumentTypes' True ++ " not found"
    Nothing -> do
      let vfunc = fromMaybe (error $ "VFunction " ++ showFunction name argumentTypes' True ++ " not found") $ find (\(VarTableEntry vname _ vfunction) -> vname == name && vfunction == currentFunction state) [v | v@(VarTableEntry {}) <- variables state]
      case value vfunc of
        PartialFunction {func = func, args = partialArgs} -> do
          let newArgs = interpretedArgs ++ partialArgs
          stackToVariables (fargs $ head $ fdef func) $ reverse newArgs
          interpret $ fbody $ head $ fdef func
        _ -> error $ "Value " ++ name ++ " is not a function"
      return ()
  where
    stackToVariables argNames interpretedArgs = do
      let stuff = zip argNames interpretedArgs
      mapM_
        ( \(x, y) -> do
            interpret $ Let x $ valueToExpr y
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
  let vvalue = fromMaybe (error $ "Variable " ++ vname ++ " not found") $ find (\v -> name v == vname && function v == currentFunction state) [v | v@(VarTableEntry {}) <- variables state]
  stackPush $ value vvalue
interpret (Let name expr) = do
  interpret expr
  state <- get
  value <- stackPeek
  put $ state {stack = pop $ stack state, variables = (VarTableEntry {value = value, name = name, function = currentFunction state}) : variables state}
interpret (If cond thenExpr elseExpr) = do
  interpret cond
  value <- stackPop
  case value of
    BoolValue True -> interpret thenExpr
    BoolValue False -> interpret elseExpr
    _ -> error "Condition must be of type Bool"
interpret (BoolLit b) = do
  stackPush $ BoolValue b
interpret (ExternDec {}) = return ()
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
  put $ state {functions = ModernFunc {fdec = FuncDec {fname = name, ftypes = types}, fdef = []} : functions state}
interpret (FuncDef name args body) = do
  state <- get
  -- If the function is already defined, we need to update the body
  let ffunc = fromMaybe (error "No function declaration found") $ find (\(ModernFunc _ dec) -> fname dec == name) [mf | mf@(ModernFunc _ _) <- functions state]
  put $ state {functions = ModernFunc {fdec = fdec ffunc, fdef = fdef ffunc ++ [FuncDef {fname = name, fbody = body, fargs = args}]} : filter (\(ModernFunc def dec) -> fname dec /= name) (functions state)}
interpret (ListConcat a b) = do
  interpret a
  interpret b
  b <- stackPop
  a <- stackPop
  case (a, b) of
    (_, ListValue b) -> stackPush $ ListValue $ a : b
    _ -> error $ "Cannot concatenate values of different types. Tried to concatenate " ++ show a ++ " and " ++ show b
interpret ex = error $ "Unimplemented expression: " ++ show ex