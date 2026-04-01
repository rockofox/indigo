module Compiler.Lists where

import AST qualified as Parser
import Compiler.State (CompileExpr, Compiler, concatMapM, internalError)
import VM (Data (..), Instruction (..))

{- | Compile list and tuple expression forms.
The first argument is the recursive 'compileExpr' callback.
-}
compileLists :: CompileExpr -> Parser.Expr -> Parser.Type -> Compiler [Instruction]
compileLists compileExpr' expr expectedType = case expr of
    Parser.ListConcat{listConcatLhs = a, listConcatRhs = b} -> do
        a' <- compileExpr' a expectedType
        b' <- compileExpr' b expectedType
        let a'' = case a' of
                [Meta "flex", a'''] -> [a'''] ++ b' ++ [Cast]
                _ -> a'
        let b'' = case b' of
                [Meta "flex", b'''] -> [b'''] ++ a' ++ [Cast]
                _ -> b'
        return (b'' ++ a'' ++ [Concat 2])
    Parser.ListAdd{listAddLhs = a, listAddRhs = b} -> do
        a' <- compileExpr' a expectedType
        b' <- compileExpr' b expectedType
        let a'' = case a' of
                [Meta "flex", a'''] -> [a'''] ++ b' ++ [Cast]
                _ -> a'
        let b'' = case b' of
                [Meta "flex", b'''] -> [b'''] ++ a' ++ [Cast]
                _ -> b'
        return (b'' ++ a'' ++ [ListAdd 2])
    Parser.ListLit{listLitExprs} -> do
        elems <- concatMapM (`compileExpr'` expectedType) listLitExprs
        return $ elems ++ [PackList $ length listLitExprs]
    Parser.TupleLit{tupleLitExprs} -> do
        elems <- concatMapM (`compileExpr'` expectedType) tupleLitExprs
        return $ elems ++ [PackList $ length tupleLitExprs]
    Parser.TupleAccess{tupleAccessTuple, tupleAccessIndex} -> do
        tuple' <- compileExpr' tupleAccessTuple expectedType
        return $ tuple' ++ [Push $ DInt tupleAccessIndex, Index]
    _ -> internalError $ "compileLists: unexpected " ++ show expr
