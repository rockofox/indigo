module WASMEmitter (compileProgramToWAST) where

import Control.Monad.State (MonadState (get, put), State, evalState, execState, modify)
import Data.List (intercalate)
import Data.Text (replace, split)
import Data.Text qualified as T
import Parser (Expr (..), Program (Program))

pop :: [a] -> [a]
pop [] = []
pop xs = init xs

typeToWASTType :: String -> String
typeToWASTType "Int" = "i32"
typeToWASTType _ = ""

compileProgramToWAST :: Program -> String
compileProgramToWAST program = evalState (compileProgramToWAST' program) initialState
  where
    initialState = CompilerState "" "" ""

compileProgramToWAST' :: Program -> State CompilerState String
compileProgramToWAST' (Program exprs) = do
  mapM_ compileExprToWAST exprs
  state <- get
  return $
    "(module\n"
      ++ ";; Data Section\n"
      ++ "(memory 1)\n"
      ++ "(export \"memory\" (memory 0))\n"
      ++ dataSection state
      ++ ";; Program Section\n"
      ++ programSection state
      ++ "\n)"

data CompilerState = CompilerState
  { programSection :: String,
    dataSection :: String,
    functionLocal :: String
  }

compileExprToWAST :: Expr -> State CompilerState String -- (program section, data section, inline value)
compileExprToWAST (ExternDec language name types) =
  do
    --   ("", "(import " ++ oname' ++ " (func $import_" ++ sanitizedName ++ " " ++ args ++ " " ++ result ++ "))\n", "")
    modify $ \s ->
      s
        { programSection = programSection s ++ "(import " ++ name' ++ " (func $import_" ++ sanitizedName ++ " " ++ args ++ " " ++ result ++ "))\n"
        }
    return ""
  where
    types' = map typeToWASTType types
    name' = do
      if language == "js"
        then
          let nameParts = split (== '.') (T.pack name)
              namePartsListWithQuotes = map (\x -> "\"" ++ T.unpack x ++ "\"") nameParts
           in unwords namePartsListWithQuotes
        else "\"" ++ name ++ "\""
    sanitizedName = T.unpack $ replace "." "_" (T.pack name)
    args = do
      let args' = unwords $ pop types'
      if not (null args')
        then "(param " ++ args' ++ ")"
        else ""
    result = do
      let result' = last types'
      if not (null result')
        then "(result " ++ result' ++ ")"
        else ""
compileExprToWAST (StringLit str) = do
  --   ("(i32.store (i32.const 0) (i32.const " ++ offset ++ "))\n(i32.store (i32.const 4) (i32.const " ++ len ++ "))", "(data (i32.const " ++ len ++ ") \"" ++ str ++ "\")\n", "(i32.const " ++ offset ++ ")")
  modify $ \s ->
    s
      { functionLocal = functionLocal s ++ "(i32.store (i32.const 0) (i32.const " ++ offset ++ "))\n(i32.store (i32.const 4) (i32.const " ++ len ++ "))\n",
        dataSection = dataSection s ++ "(data (i32.const " ++ len ++ ") \"" ++ str ++ "\")\n"
      }
  return $ "(i32.const " ++ offset ++ ")"
  where
    len = show . length $ str
    offset = len
compileExprToWAST (IntLit int) = do
  --  ("", "", "(i32.const " ++ show int ++ ")")
  return $ "(i32.const " ++ show int ++ ")"
compileExprToWAST (ModernFunc def dec) = do
  --   ("(func $" ++ name ++ " " ++ args ++ " " ++ result ++ "\n" ++ body ++ "\ndrop)\n", "", "")
  body <- compileExprToWAST $ fbody def
  state <- get
  modify $ \s ->
    s
      { programSection = programSection s ++ "(func $" ++ name ++ " " ++ args ++ " " ++ result ++ "\n" ++ functionLocal state ++ "\n" ++ body ++ "\ndrop)\n",
        dataSection = dataSection s
      }
  return ""
  where
    name = fname dec
    types = map typeToWASTType (ftypes dec)
    args = do
      let args' = unwords $ pop types
      if not (null args')
        then "(param " ++ args' ++ ")"
        else ""
    result = do
      let result' = last types
      if not (null result')
        then "(result " ++ result' ++ ")"
        else ""
-- (body, _, _) = compileExprToWAST $ fbody def
-- compileExprToWAST (FuncCall name args) =
--   do
--     let args' = unwords $ map (\x -> let (_, _, a) = compileExprToWAST x in a) args
--      in -- ("(call $" ++ name ++ " " ++ args' ++ ")", "", "")
--         return $ "(call $" ++ name ++ " " ++ args' ++ ")"
compileExprToWAST (FuncCall name args) = do
  args' <- mapM compileExprToWAST args
  return $ "(" ++ name ++ " " ++ unwords args' ++ ")"
compileExprToWAST (DoBlock exprs) = do
  exprs' <- mapM compileExprToWAST exprs
  return $ unwords exprs'
compileExprToWAST _ = do
  return ""