module Compiler.Refinements where

import AST (anyPosition, zeroPosition)
import AST qualified as Parser
import Compiler.State
    ( Compiler
    , CompilerError
    , CompilerState (..)
    , Function (..)
    , Let (..)
    , allocId
    , internalError
    )
import Control.Monad.State (MonadIO (liftIO), evalStateT, get, gets)
import Data.List (elemIndex, find, nub)
import Data.Maybe (maybeToList)
import Data.Vector qualified as V
import VM (Data (..), Instruction (..), StackFrame (..), VM (..), initVM, runVMVM)

type CompileProgram = Parser.Program -> Compiler (Either [Instruction] [CompilerError])

locateLabelIn :: [Instruction] -> String -> Int
locateLabelIn program label =
    case elemIndex (Label label) program of
        Just x -> x
        Nothing -> internalError $ "Label not found: " ++ label

transformRefinementExpr :: Parser.Expr -> [(String, Parser.Type)] -> Parser.Expr
transformRefinementExpr expr fieldNames = case expr of
    Parser.Var{varName}
        | varName `elem` map fst fieldNames ->
            Parser.StructAccess
                (Parser.Var "it" zeroPosition)
                (Parser.Var varName zeroPosition)
                anyPosition
    Parser.Add{addLhs = x, addRhs = y, addPos} ->
        Parser.Add (transformRefinementExpr x fieldNames) (transformRefinementExpr y fieldNames) addPos
    Parser.Sub{subLhs = x, subRhs = y, subPos} ->
        Parser.Sub (transformRefinementExpr x fieldNames) (transformRefinementExpr y fieldNames) subPos
    Parser.Mul{mulLhs = x, mulRhs = y, mulPos} ->
        Parser.Mul (transformRefinementExpr x fieldNames) (transformRefinementExpr y fieldNames) mulPos
    Parser.Div{divLhs = x, divRhs = y, divPos} ->
        Parser.Div (transformRefinementExpr x fieldNames) (transformRefinementExpr y fieldNames) divPos
    Parser.Modulo{moduloLhs = x, moduloRhs = y, moduloPos} ->
        Parser.Modulo (transformRefinementExpr x fieldNames) (transformRefinementExpr y fieldNames) moduloPos
    Parser.Gt{gtLhs = x, gtRhs = y, gtPos} ->
        Parser.Gt (transformRefinementExpr x fieldNames) (transformRefinementExpr y fieldNames) gtPos
    Parser.Lt{ltLhs = x, ltRhs = y, ltPos} ->
        Parser.Lt (transformRefinementExpr x fieldNames) (transformRefinementExpr y fieldNames) ltPos
    Parser.Ge{geLhs = x, geRhs = y, gePos} ->
        Parser.Ge (transformRefinementExpr x fieldNames) (transformRefinementExpr y fieldNames) gePos
    Parser.Le{leLhs = x, leRhs = y, lePos} ->
        Parser.Le (transformRefinementExpr x fieldNames) (transformRefinementExpr y fieldNames) lePos
    Parser.Eq{eqLhs = x, eqRhs = y, eqPos} ->
        Parser.Eq (transformRefinementExpr x fieldNames) (transformRefinementExpr y fieldNames) eqPos
    Parser.Neq{neqLhs = x, neqRhs = y, neqPos} ->
        Parser.Neq (transformRefinementExpr x fieldNames) (transformRefinementExpr y fieldNames) neqPos
    Parser.And{andLhs = x, andRhs = y, andPos} ->
        Parser.And (transformRefinementExpr x fieldNames) (transformRefinementExpr y fieldNames) andPos
    Parser.Or{orLhs = x, orRhs = y, orPos} ->
        Parser.Or (transformRefinementExpr x fieldNames) (transformRefinementExpr y fieldNames) orPos
    Parser.Not{notExpr = x, notPos} ->
        Parser.Not (transformRefinementExpr x fieldNames) notPos
    Parser.UnaryMinus{unaryMinusExpr = x, unaryMinusPos} ->
        Parser.UnaryMinus (transformRefinementExpr x fieldNames) unaryMinusPos
    Parser.StructAccess{structAccessStruct = s, structAccessField = f, structAccessPos} ->
        Parser.StructAccess (transformRefinementExpr s fieldNames) f structAccessPos
    Parser.FuncCall{funcName, funcArgs, funcPos} ->
        Parser.FuncCall funcName (map (`transformRefinementExpr` fieldNames) funcArgs) funcPos
    _ -> expr

extractVariables :: Parser.Expr -> [String]
extractVariables expr = case expr of
    Parser.Var{varName} -> [varName]
    Parser.StructLit{structLitFields} -> concatMap (extractVariables . snd) structLitFields
    Parser.FuncCall{funcArgs} -> concatMap extractVariables funcArgs
    Parser.Add{addLhs, addRhs} -> extractVariables addLhs ++ extractVariables addRhs
    Parser.Sub{subLhs, subRhs} -> extractVariables subLhs ++ extractVariables subRhs
    Parser.Mul{mulLhs, mulRhs} -> extractVariables mulLhs ++ extractVariables mulRhs
    Parser.Div{divLhs, divRhs} -> extractVariables divLhs ++ extractVariables divRhs
    Parser.Gt{gtLhs, gtRhs} -> extractVariables gtLhs ++ extractVariables gtRhs
    Parser.Lt{ltLhs, ltRhs} -> extractVariables ltLhs ++ extractVariables ltRhs
    Parser.Ge{geLhs, geRhs} -> extractVariables geLhs ++ extractVariables geRhs
    Parser.Le{leLhs, leRhs} -> extractVariables leLhs ++ extractVariables leRhs
    Parser.Eq{eqLhs, eqRhs} -> extractVariables eqLhs ++ extractVariables eqRhs
    Parser.Neq{neqLhs, neqRhs} -> extractVariables neqLhs ++ extractVariables neqRhs
    Parser.And{andLhs, andRhs} -> extractVariables andLhs ++ extractVariables andRhs
    Parser.Or{orLhs, orRhs} -> extractVariables orLhs ++ extractVariables orRhs
    Parser.Not{notExpr} -> extractVariables notExpr
    Parser.UnaryMinus{unaryMinusExpr} -> extractVariables unaryMinusExpr
    Parser.StructAccess{structAccessStruct, structAccessField} -> extractVariables structAccessStruct ++ extractVariables structAccessField
    Parser.If{ifCond, ifThen, ifElse} -> extractVariables ifCond ++ extractVariables ifThen ++ extractVariables ifElse
    Parser.Let{letValue} -> extractVariables letValue
    Parser.DoBlock{doBlockExprs} -> concatMap extractVariables doBlockExprs
    Parser.ListLit{listLitExprs} -> concatMap extractVariables listLitExprs
    Parser.ParenApply{parenApplyExpr, parenApplyArgs} -> extractVariables parenApplyExpr ++ concatMap extractVariables parenApplyArgs
    _ -> []

extractLets :: [Parser.Expr] -> [Parser.Expr]
extractLets = concatMap extractLet
  where
    extractLet expr@(Parser.Let{letValue}) = expr : extractLets [letValue]
    extractLet (Parser.FuncDef{body}) = extractLets [body]
    extractLet (Parser.DoBlock{doBlockExprs}) = extractLets doBlockExprs
    extractLet (Parser.If{ifThen, ifElse}) = extractLets [ifThen, ifElse]
    extractLet (Parser.Function{def}) = extractLets def
    extractLet _ = []

extractFunctions :: [Parser.Expr] -> [Parser.Expr]
extractFunctions = concatMap extractFunction
  where
    extractFunction expr@(Parser.Function{}) = [expr]
    extractFunction expr@(Parser.FuncDef{body}) = expr : extractFunctions [body]
    extractFunction expr@(Parser.FuncDec{}) = [expr]
    extractFunction (Parser.Let{letName, letValue}) = case letValue of
        Parser.Function{def, dec} ->
            let renamedDec = case dec of
                    Parser.FuncDec{} -> dec{Parser.name = letName}
                    _ -> dec
                renamedDef =
                    map
                        ( \d -> case d of
                            Parser.FuncDef{args, body, funcDefPos} -> Parser.FuncDef{name = letName, args = args, body = body, funcDefPos = funcDefPos}
                            _ -> d
                        )
                        def
             in [Parser.Function{def = renamedDef, dec = renamedDec, functionPos = anyPosition}]
        _ -> extractFunctions [letValue]
    extractFunction (Parser.DoBlock{doBlockExprs}) = extractFunctions doBlockExprs
    extractFunction _ = []

{- | Run a refinement check against a value expression.
Returns 'Nothing' if the check cannot be performed at compile time,
'Just True' if it passes, and 'Just False' if it fails.
-}
runRefinement :: CompileProgram -> Parser.Expr -> Parser.Expr -> Compiler (Maybe Bool)
runRefinement compileProgram' refinement value = do
    currentState <- get
    let structDef = case value of
            Parser.StructLit{structLitName} ->
                find (\case Parser.Struct{name = name'} -> name' == structLitName; _ -> False) currentState.structDecs
            _ -> Nothing
    let fieldNames = case structDef of
            Just (Parser.Struct{fields}) -> fields
            _ -> []
    let transformedRefinement = if null fieldNames then refinement else transformRefinementExpr refinement fieldNames
    let currentProgramExprs = Parser.exprs currentState.program
    let allFunctionDefs = extractFunctions currentProgramExprs
    let functionDefs = filter (\case Parser.FuncDec{name} -> name /= "main"; Parser.FuncDef{name} -> name /= "main"; Parser.Function{dec = Parser.FuncDec{name}} -> name /= "main"; _ -> True) allFunctionDefs
    let referencedVars = nub $ extractVariables value
    let allLets = extractLets currentProgramExprs
    let neededLets = filter (\case Parser.Let{letName} -> letName `elem` referencedVars; _ -> False) allLets
    let letNames = map (\case Parser.Let{letName} -> letName; _ -> "") neededLets
    let unresolvedVars = filter (`notElem` letNames) referencedVars
    if not (null unresolvedVars)
        then return Nothing
        else do
            let mainBody =
                    if null neededLets
                        then [Parser.FuncCall "__refinement" [value] anyPosition]
                        else neededLets ++ [Parser.FuncCall "__refinement" [value] anyPosition]
            let structDefWithoutIs = case structDef of
                    Just (Parser.Struct{name, fields, refinement = ref, refinementSrc, isValueStruct, generics, structPos}) ->
                        Just (Parser.Struct{name = name, fields = fields, refinement = ref, refinementSrc = refinementSrc, is = [], isValueStruct = isValueStruct, generics = generics, structPos = structPos})
                    _ -> structDef
            let progExprs =
                    maybeToList structDefWithoutIs
                        ++ functionDefs
                        ++ [ Parser.FuncDec "__refinement" [Parser.Any, Parser.StructT "Bool" []] [] anyPosition
                           , Parser.FuncDef "__refinement" [Parser.Var "it" zeroPosition] transformedRefinement anyPosition
                           , Parser.FuncDec "main" [Parser.StructT "IO" []] [] anyPosition
                           , Parser.FuncDef
                                "main"
                                []
                                (Parser.DoBlock mainBody anyPosition)
                                anyPosition
                           ]
            let prog = Parser.Program progExprs Nothing
            let freshState =
                    currentState
                        { program = prog
                        , functions = []
                        , funcDefs = []
                        , funcDecs = currentState.funcDecs ++ filter (\case Parser.FuncDec{} -> True; _ -> False) functionDefs ++ [Parser.FuncDec "__refinement" [Parser.Any, Parser.StructT "Bool" []] [] anyPosition]
                        , structDecs = currentState.structDecs
                        , lastLabel = 0
                        , lets = []
                        , traits = []
                        , impls = []
                        , contextStack = ["__outside"]
                        , externals = []
                        , functionsByTrait = []
                        , errors = []
                        , skipRefinementCheck = True
                        , modules = currentState.modules
                        , currentModule = Nothing
                        , currentSourceFile = currentState.currentSourceFile
                        }
            compiled <- liftIO $ evalStateT (compileProgram' prog) freshState
            case compiled of
                Right _ -> return $ Just False
                Left bytecode -> do
                    let mainPc = locateLabelIn bytecode "main"
                    let vm = (initVM (V.fromList bytecode)){pc = mainPc, callStack = [StackFrame{returnAddress = 0, locals = []}, StackFrame{returnAddress = mainPc, locals = []}], shouldExit = False, running = True}
                    result <- liftIO $ runVMVM vm
                    let finalStack = stack result
                    let isRunning = running result
                    if isRunning
                        then return $ Just False
                        else return $ Just $ case find (\case DBool _ -> True; _ -> False) finalStack of
                            Just (DBool b) -> b
                            _ -> False
