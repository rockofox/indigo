module Compiler.ControlFlow where

import AST (anyPosition)
import AST qualified as Parser
import Compiler.State
    ( CompileExpr
    , Compiler
    , CompilerState (..)
    , Let (..)
    , allocId
    , concatMapM
    , currentContext
    , internalError
    )
import Control.Monad.State (gets)
import Data.List (nub)
import VM (Data (..), Instruction (..))

internalFunctions :: [String]
internalFunctions = ["unsafePrint", "unsafeGetLine", "unsafeGetChar", "unsafeRandom", "abs", "root", "sqrt"]

extractPatternVars :: Parser.Expr -> [String]
extractPatternVars (Parser.Var{varName}) = [varName]
extractPatternVars (Parser.ListLit{listLitExprs}) = concatMap extractPatternVars listLitExprs
extractPatternVars (Parser.ListPattern{listPatternExprs}) = concatMap extractPatternVars listPatternExprs
extractPatternVars (Parser.StructLit{structLitFields}) = concatMap (\(_, e) -> extractPatternVars e) structLitFields
extractPatternVars (Parser.TupleLit{tupleLitExprs}) = concatMap extractPatternVars tupleLitExprs
extractPatternVars _ = []

compilePatternMatch :: CompileExpr -> Parser.Expr -> String -> Parser.Type -> Compiler [Instruction]
compilePatternMatch _ (Parser.Var{varName}) _nextLabel _ = return [LStore varName]
compilePatternMatch _ (Parser.IntLit{intValue = x}) nextLabel _ =
    return [Dup, Push $ DInt $ fromIntegral x, Eq, Jf nextLabel, Pop]
compilePatternMatch _ (Parser.BoolLit{boolValue = b}) nextLabel _ =
    return [Dup, Push $ DBool b, Eq, Jf nextLabel, Pop]
compilePatternMatch _ (Parser.StringLit{stringValue = s}) nextLabel _ =
    return [Dup, Push $ DString s, Eq, Jf nextLabel, Pop]
compilePatternMatch _ (Parser.CharLit{charValue = c}) nextLabel _ =
    return [Dup, Push $ DChar c, Eq, Jf nextLabel, Pop]
compilePatternMatch compileExpr' lex@(Parser.ListLit{listLitExprs}) nextLabel expectedType = do
    if null listLitExprs
        then return [Dup, Push $ DList [], Eq, Jf nextLabel, Pop]
        else do
            let allVars = all (\case Parser.Var{} -> True; _ -> False) listLitExprs
            if allVars
                then do
                    let lengthCheck = [Dup, Length, Push $ DInt $ fromIntegral $ length listLitExprs, Eq, Jf nextLabel]
                    let bindings =
                            concatMap
                                ( \case
                                    (Parser.Var{varName}, index) ->
                                        [Dup, Push $ DInt index, Index, LStore varName]
                                    _ -> internalError "Expected Var in list pattern"
                                )
                                (zip listLitExprs [0 ..])
                    return $ lengthCheck ++ bindings ++ [Pop]
                else do
                    lex' <- compileExpr' lex expectedType
                    return $ lex' ++ [Eq, Jf nextLabel]
compilePatternMatch compileExpr' (Parser.ListPattern{listPatternExprs}) nextLabel expectedType = do
    let lengthCheck = [Dup, Length, Push $ DInt $ fromIntegral $ length listPatternExprs - 1, Lt, Jt nextLabel]
    case last listPatternExprs of
        Parser.ListLit{listLitExprs} -> do
            elements' <- mapM (\p -> compilePatternMatch compileExpr' p nextLabel expectedType) (init listPatternExprs)
            let paramsWithIndex = zip elements' [0 ..]
            let xToY = map (\(x, index) -> [Dup, Push $ DInt index, Index] ++ x) paramsWithIndex
            l' <- compileExpr' (Parser.ListLit{listLitExprs = listLitExprs, listLitPos = anyPosition}) expectedType
            let listThing = [Comment "List thing", Dup, Push $ DInt (length listPatternExprs - 1), Push DNone, Slice] ++ l' ++ [Eq, Jf nextLabel]
            return $ lengthCheck ++ concat xToY ++ listThing
        _ -> do
            elements' <- mapM (\p -> compilePatternMatch compileExpr' p nextLabel expectedType) listPatternExprs
            let paramsWithIndex = zip elements' [0 ..]
            let xToY = map (\(x, index) -> [Dup, Push $ DInt index, Index] ++ x) paramsWithIndex
            let rest = [Push $ DInt (length listPatternExprs - 1), Push DNone, Slice] ++ last elements'
            return $ lengthCheck ++ concat xToY ++ rest
compilePatternMatch compileExpr' (Parser.TupleLit{tupleLitExprs}) nextLabel expectedType = do
    if null tupleLitExprs
        then return [Dup, Push $ DList [], Eq, Jf nextLabel, Pop]
        else do
            let tempVarName = "__tuple_temp_" ++ nextLabel
            let lengthCheck = [Dup, Length, Push $ DInt $ fromIntegral $ length tupleLitExprs, Eq, Jf nextLabel, Dup, LStore tempVarName]
            bindingsList <-
                mapM
                    ( \(pat, index) -> do
                        case pat of
                            Parser.Var{varName} ->
                                return [LLoad tempVarName, Dup, Push $ DInt index, Index, LStore varName, LLoad tempVarName]
                            Parser.TupleLit{} -> do
                                let nestedLabel = nextLabel ++ "_nested_" ++ show index
                                let nestedSuccessLabel = nextLabel ++ "_nested_ok_" ++ show index
                                nestedInstrs <- compilePatternMatch compileExpr' pat nestedLabel expectedType
                                let nestedCleanup = [Jmp nestedSuccessLabel, Label nestedLabel, Pop, LLoad tempVarName, Jmp nextLabel, Label nestedSuccessLabel]
                                return $ [LLoad tempVarName, Dup, Push $ DInt index, Index] ++ nestedInstrs ++ nestedCleanup ++ [LLoad tempVarName]
                            _ -> do
                                let cleanupLabel = "__tuple_cleanup_" ++ nextLabel ++ "_" ++ show index
                                let successLabel = "__tuple_success_" ++ nextLabel ++ "_" ++ show index
                                literalInstrs <- compilePatternMatch compileExpr' pat cleanupLabel expectedType
                                let afterMatch = [Jmp successLabel, Label cleanupLabel, Pop, LLoad tempVarName, Jmp nextLabel, Label successLabel, Pop]
                                return $ [LLoad tempVarName, Dup, Push $ DInt index, Index] ++ literalInstrs ++ afterMatch
                    )
                    (zip tupleLitExprs [0 ..])
            return $ lengthCheck ++ concat bindingsList ++ [Pop]
compilePatternMatch compileExpr' (Parser.StructLit{structLitName, structLitFields}) nextLabel expectedType = do
    let fields' =
            concatMap
                ( \case
                    (sName, Parser.Var{varName = tName}) -> [(sName, tName)]
                    _ -> []
                )
                structLitFields
    let fieldMappings = concatMap (\(sName, tName) -> [Dup, Access sName, LStore tName]) fields'
    let fields'' =
            concatMap
                ( \case
                    (_, Parser.Var{}) -> []
                    (sName, x) -> [(sName, x)]
                )
                structLitFields
    fieldChecks <-
        concatMapM
            ( \(sName, x) -> do
                let a = [Dup, Access sName]
                b <- compileExpr' x expectedType
                return $ a ++ b ++ [Eq, Jf nextLabel]
            )
            fields''
    return $ [Dup, Push $ DTypeQuery structLitName, TypeEq, Jf nextLabel] ++ fieldMappings ++ ([Pop | null fieldChecks]) ++ fieldChecks
compilePatternMatch _ (Parser.Placeholder _) _ _ = return []
compilePatternMatch compileExpr' x nextLabel expectedType = do
    x' <- compileExpr' x expectedType
    return $ x' ++ [Eq, Jf nextLabel]

withShadowedVars :: [String] -> Compiler [Instruction] -> Compiler [Instruction]
withShadowedVars varsToSave body = do
    saveInstrs <- mapM (\v -> return [LLoad v, LStore ("__shadow_" ++ v)]) varsToSave
    bodyInstrs <- body
    let restoreInstrs = concatMap (\v -> [LLoad ("__shadow_" ++ v), LStore v]) varsToSave
    return $ concat saveInstrs ++ bodyInstrs ++ restoreInstrs

compileDoBlock :: CompileExpr -> [Parser.Expr] -> Parser.Type -> Compiler [Instruction]
compileDoBlock compileExpr' exprs expectedType = do
    letsBefore <- gets lets
    curCon <- gets currentContext
    let inScopeLets = filter (\l -> l.context == curCon || l.context == "__outside") letsBefore
    let existingVars = map (\l -> l.name) inScopeLets
    let shadowedVars = filter (`elem` existingVars) $ map (\case Parser.Let{letName} -> letName; _ -> "") $ filter (\case Parser.Let{} -> True; _ -> False) exprs
    let grouped = groupBy' (\a b -> isFuncCall a && isFuncCall b) exprs
    let nestedSequence = concatMap (\case [] -> []; [x] -> if isFuncCall x then [Parser.FuncCall{funcName = "sequence", funcArgs = [x, Parser.FuncCall{funcName = "nop", funcArgs = [], funcPos = anyPosition}], funcPos = anyPosition}] else [x]; xs -> if all isFuncCall xs then [foldl1 (\a b -> Parser.FuncCall{funcName = "sequence", funcArgs = [a, b], funcPos = anyPosition}) xs] else xs) grouped
    withShadowedVars shadowedVars $
        if length (filter isFuncCall exprs) == 1
            then concatMapM (`compileExpr'` expectedType) exprs
            else concatMapM (`compileExpr'` expectedType) nestedSequence
  where
    isFuncCall :: Parser.Expr -> Bool
    isFuncCall (Parser.FuncCall{funcName}) = funcName `notElem` internalFunctions
    isFuncCall _ = False

    -- Group consecutive elements satisfying a predicate together.
    groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
    groupBy' _ [] = []
    groupBy' p (x : xs) =
        let (same, rest) = span (p x) xs
         in (x : same) : groupBy' p rest

compileWhen :: CompileExpr -> Parser.Expr -> [(Parser.Expr, Parser.Expr)] -> Maybe Parser.Expr -> Parser.Type -> Compiler [Instruction]
compileWhen compileExpr' expr branches else_ expectedType = do
    expr' <- compileExpr' expr expectedType
    endLabel <- allocId >>= \x -> return $ "when_end" ++ show x
    branchLabels <- mapM (\_ -> allocId >>= \x -> return $ "when_next" ++ show x) branches
    let allPatternVars = nub $ concatMap (extractPatternVars . fst) branches
    lets' <- gets lets
    let existingVars = map (\l -> l.name) lets'
    let varsToSave = filter (`elem` existingVars) allPatternVars
    saveInstrs <-
        mapM
            ( \varName -> do
                let tempName = "__shadow_" ++ varName
                return [LLoad varName, LStore tempName]
            )
            varsToSave
    let branchPairs = zip branches branchLabels
    branchInstrs <-
        mapM
            ( \((pat, body), nextLabel) -> do
                patternInstrs <- compilePatternMatch compileExpr' pat nextLabel expectedType
                bodyInstrs <- compileExpr' body expectedType
                let restoreInstrs = concatMap (\varName -> [LLoad ("__shadow_" ++ varName), LStore varName]) varsToSave
                return (patternInstrs, bodyInstrs, restoreInstrs)
            )
            branchPairs
    elseInstrs <- case else_ of
        Just elseExpr -> compileExpr' elseExpr expectedType
        Nothing -> return []
    let branchCode =
            concat $
                zipWith
                    ( \(patternInstrs, bodyInstrs, restoreInstrs) nextLabel ->
                        patternInstrs ++ bodyInstrs ++ restoreInstrs ++ [Jmp endLabel, Label nextLabel]
                    )
                    branchInstrs
                    branchLabels
    return $ expr' ++ concat saveInstrs ++ branchCode ++ elseInstrs ++ [Label endLabel]

compileIf :: CompileExpr -> Parser.Expr -> Parser.Expr -> Parser.Expr -> Parser.Type -> Compiler [Instruction]
compileIf compileExpr' ifCond ifThen ifElse expectedType = do
    cond' <- compileExpr' ifCond expectedType
    then' <- compileExpr' ifThen expectedType
    else' <- compileExpr' ifElse expectedType
    elseLabel <- allocId >>= \x -> return $ "else" ++ show x
    endLabel <- allocId >>= \x -> return $ "end" ++ show x
    return $ cond' ++ [Jf elseLabel] ++ then' ++ [Jmp endLabel, Label elseLabel] ++ else' ++ [Label endLabel]

compilePipeline :: CompileExpr -> Parser.Expr -> Parser.Expr -> Parser.Type -> Compiler [Instruction]
compilePipeline compileExpr' a rhs expectedType = case rhs of
    Parser.Var{varName = b} ->
        compileExpr' (Parser.FuncCall{funcName = b, funcArgs = [a], funcPos = anyPosition}) expectedType
    Parser.FuncCall{funcName = f, funcArgs} ->
        compileExpr' (Parser.FuncCall{funcName = f, funcArgs = a : funcArgs, funcPos = anyPosition}) expectedType
    Parser.Then{thenLhs = b, thenRhs = c} ->
        compileThen compileExpr' (Parser.Pipeline{pipelineLhs = a, pipelineRhs = b, pipelinePos = anyPosition}) c expectedType
    _ -> internalError $ "compilePipeline: unexpected rhs " ++ show rhs

compileThen :: CompileExpr -> Parser.Expr -> Parser.Expr -> Parser.Type -> Compiler [Instruction]
compileThen compileExpr' a b expectedType = do
    a' <- compileExpr' a expectedType
    b' <- compileExpr' b expectedType
    return $ a' ++ b'
