module JSEmitter where

import Data.List (intercalate)
import Parser hiding (expr)

compileProgramToJS :: Program -> String
compileProgramToJS (Program x) =
  compileProgramToJS' x []
 where
  compileProgramToJS' :: [Expr] -> [(String, [String])] -> String
  compileProgramToJS' [] _ = ""
  compileProgramToJS' (expr : exprs) fTable = do
    _ <- case expr of
      Var name -> name
      IntLit i -> show i
      BoolLit b -> show b
      FuncDec name types -> do
        compileProgramToJS' exprs (fTable ++ [(name, types)])
      FuncDef name args body -> do
        let fType = lookup name fTable
        case fType of
          Just types -> do
            let argsWithTypes = zip args types
            let argsWithTypesString = intercalate ", " $ map (\(arg, type') -> arg ++ ": " ++ type') argsWithTypes
            let bodyString = compileProgramToJS' [body] fTable
            "function " ++ name ++ "(" ++ argsWithTypesString ++ ") { return " ++ bodyString ++ "; }"
          Nothing -> error "Function type not found. found: " ++ intercalate ", " (map fst fTable)
      FuncCall name args -> do
        let argsString = intercalate ", " $ map (\arg -> compileProgramToJS' [arg] fTable) args
        name ++ "(" ++ argsString ++ ")"
      _ -> show expr
    compileProgramToJS' exprs fTable
