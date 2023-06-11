module CEmitter where

import Data.Data (typeOf)
import Debug.Trace
import Parser (Expr (..), Program (..), Type (..), typeOf)

-- Remove first element from list
pop :: [a] -> [a]
pop [] = []
pop (x : xs) = xs

popcatMap :: (a -> String) -> [a] -> String
popcatMap f xs = pop $ concatMap f xs

typeToC :: Parser.Type -> String
typeToC Int = "int"
typeToC String = "char*"
typeToC Bool = "bool"
typeToC _ = "void*"

compileProgramToC :: Program -> String
compileProgramToC (Program exprs) = concatMap compileExprToC exprs

compileExprToC :: Expr -> String
compileExprToC (Eq e1 e2) = compileExprToC e1 ++ " == " ++ compileExprToC e2
compileExprToC (Neq e1 e2) = compileExprToC e1 ++ " != " ++ compileExprToC e2
compileExprToC (Lt e1 e2) = compileExprToC e1 ++ " < " ++ compileExprToC e2
compileExprToC (Gt e1 e2) = compileExprToC e1 ++ " > " ++ compileExprToC e2
compileExprToC (Le e1 e2) = compileExprToC e1 ++ " <= " ++ compileExprToC e2
compileExprToC (Ge e1 e2) = compileExprToC e1 ++ " >= " ++ compileExprToC e2
compileExprToC (And e1 e2) = compileExprToC e1 ++ " && " ++ compileExprToC e2
compileExprToC (Or e1 e2) = compileExprToC e1 ++ " || " ++ compileExprToC e2
compileExprToC (Not e) = "!" ++ compileExprToC e
compileExprToC (Add e1 e2) = compileExprToC e1 ++ " + " ++ compileExprToC e2
compileExprToC (Sub e1 e2) = compileExprToC e1 ++ " - " ++ compileExprToC e2
compileExprToC (Mul e1 e2) = compileExprToC e1 ++ " * " ++ compileExprToC e2
compileExprToC (Div e1 e2) = compileExprToC e1 ++ " / " ++ compileExprToC e2
compileExprToC (Target t expr)
    | t == "C" = compileExprToC expr
-- compileExprToC (ExternDec lang fname args)
--     | lang == "c" = do
--         let retType = typeToC $ last args
--         let argTypes = pop args
--         let argsWithTypes = zip (map typeToC argTypes) args
--         "" ++ retType ++ " " ++ fname ++ "(" ++ popcatMap (\(t, a) -> t ++ " " ++ a ++ ",") argsWithTypes ++ ");\n"
compileExprToC (StringLit s) = show s
compileExprToC (IntLit i) = show i
compileExprToC (ModernFunc def dec) = do
    let retType = typeToC $ last $ ftypes dec
    let argTypes = pop $ ftypes dec
    let args = fargs def
    let name = fname dec
    let body = compileExprToC $ fbody def
    let argsWithTypes = zip (map typeToC argTypes) args
    "" ++ retType ++ " " ++ name ++ "(" ++ popcatMap (\(t, a) -> t ++ " " ++ a ++ ",") argsWithTypes ++ ") {\n" ++ body ++ "\n}\n"
compileExprToC (FuncCall name args) = do
    name ++ "(" ++ popcatMap (\a -> compileExprToC a ++ ",") args ++ ")"
compileExprToC (Var name) = name
compileExprToC (Let name expr) = do
    let exprC = compileExprToC expr
    let exprType = typeToC $ Parser.typeOf expr
    exprType ++ " " ++ name ++ " = " ++ exprC ++ ""
compileExprToC (If cond thenExpr elseExpr) = do
    let condC = compileExprToC cond
    let thenC = compileExprToC thenExpr
    let elseC = compileExprToC elseExpr
    "if (" ++ condC ++ ") \n" ++ thenC ++ "\n else \n" ++ elseC ++ "\n"
compileExprToC (DoBlock exprs) = concatMap compileExprToC exprs
-- compileExprToC x = "\n/* UNIMPLEMENTED: " ++ show x ++ " */"
compileExprToC _ = ""