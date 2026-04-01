{-# LANGUAGE CPP #-}

module BytecodeCompiler
    ( -- Re-export submodule namespaces for backward compatibility
      module Compiler.State
    , module Compiler.Types
    , module Compiler.Literals
    , module Compiler.Functions
    , module Compiler.Structs
    , module Compiler.Traits
    , module Compiler.ControlFlow
    , module Compiler.Lists
    , module Compiler.Modules
    , module Compiler.Refinements
    -- Orchestrator-level definitions
    , compileExpr
    , compileProgram
    , compileProgramBare
    , compileDry
    , locateLabel
    , preludeExpr
    , groupErrorsByFile
    , compilerErrorToSourceError
    , renderErrorsForFile
    , renderCompilerErrors
    , readMissingFileContents
    , compileFail
    )
where

import AST (anyPosition)
import AST qualified as Parser
import Compiler.ControlFlow
import Compiler.Functions
import Compiler.Lists
import Compiler.Literals
import Compiler.Modules
import Compiler.Refinements
import Compiler.State
import Compiler.Structs
import Compiler.Traits
import Compiler.Types
import Control.Exception (SomeException, try)
import Control.Monad.State (MonadIO (liftIO), evalStateT, gets)
import Data.List (nub)
import Data.Map qualified as Map
import Data.Text qualified as T
import ErrorRenderer (SourceError (..), parseErrorBundleToSourceErrors, renderErrors)
import Parser (parseProgram)
import Parser qualified
import VM (Data (..), Instruction (..))

compileBinOpSeq
    :: CompileExpr
    -> String
    -> Parser.Expr
    -> Parser.Expr
    -> Parser.Type
    -> [Instruction]
    -> Compiler [Instruction]
compileBinOpSeq compileExpr' opName x y expectedType ops = do
    errorCountBefore <- getErrorCount
    let pos = getBinOpPosition x y
    _ <- compileExpr' (Parser.FuncCall{funcName = opName, funcArgs = [x, y], funcPos = pos}) expectedType
    errorCountAfter <- getErrorCount
    if errorCountAfter > errorCountBefore
        then return []
        else do
            id' <- allocId
            let aName = "__op_a_" ++ show id'
            let bName = "__op_b_" ++ show id'
            x' <- compileExpr' x expectedType
            y' <- compileExpr' y expectedType
            let cast = case (x, y) of
                    (Parser.Flexible{}, Parser.Flexible{}) -> error "Double cast"
                    (Parser.Flexible{}, _) -> Cast : y'
                    (_, Parser.Flexible{}) -> [Swp, Cast] ++ x'
                    _ -> []
            return (x' ++ LStore aName : y' ++ [LStore bName, LLoad aName, LLoad bName] ++ cast ++ ops)

compileExpr :: Parser.Expr -> Parser.Type -> Compiler [Instruction]
compileExpr expr typ = do
    litResult <- compileLiterals expr typ
    case litResult of
        Just instrs -> return instrs
        Nothing -> dispatch expr typ

dispatch :: Parser.Expr -> Parser.Type -> Compiler [Instruction]
-- Binary operators
dispatch (Parser.Add{addLhs = x, addRhs = y}) t = compileBinOp compileExpr "+" x y t Add
dispatch (Parser.Sub{subLhs = x, subRhs = y}) t = compileBinOp compileExpr "-" x y t Sub
dispatch (Parser.Mul{mulLhs = x, mulRhs = y}) t = compileBinOp compileExpr "*" x y t Mul
dispatch (Parser.Div{divLhs = x, divRhs = y}) t = compileBinOp compileExpr "/" x y t Div
dispatch (Parser.Modulo{moduloLhs = x, moduloRhs = y}) t = compileBinOp compileExpr "%" x y t Mod
dispatch (Parser.Power{powerBase = x, powerExponent = y}) t = compileBinOp compileExpr "^" x y t Pow
dispatch (Parser.Gt{gtLhs = x, gtRhs = y}) t = compileBinOp compileExpr ">" x y t Gt
dispatch (Parser.Lt{ltLhs = x, ltRhs = y}) t = compileBinOp compileExpr "<" x y t Lt
dispatch (Parser.Ge{geLhs = x, geRhs = y}) t = compileBinOpSeq compileExpr ">=" x y t [Lt, Not]
dispatch (Parser.Le{leLhs = x, leRhs = y}) t = compileBinOpSeq compileExpr "<=" x y t [Gt, Not]
dispatch (Parser.Eq{eqLhs = x, eqRhs = y}) t = compileBinOp compileExpr "==" x y t Eq
dispatch (Parser.Neq{neqLhs = x, neqRhs = y}) t = compileBinOp compileExpr "!=" x y t Neq
dispatch (Parser.And{andLhs = x, andRhs = y}) t = compileBinOp compileExpr "&&" x y t And
dispatch (Parser.Or{orLhs = x, orRhs = y}) t = compileBinOp compileExpr "||" x y t Or
dispatch (Parser.Not{notExpr = x}) t = compileExpr x t >>= \x' -> return (x' ++ [Not])
dispatch (Parser.UnaryMinus{unaryMinusExpr = x}) t = compileExpr x t >>= \x' -> return (x' ++ [Push $ DInt (-1), Mul])
-- Simple pass-throughs
dispatch (Parser.Placeholder _) _ = return []
dispatch (Parser.StrictEval{strictEvalExpr = e}) t = compileExpr e t
dispatch (Parser.Flexible{flexibleExpr = a}) t = compileExpr a t >>= \a' -> return $ Meta "flex" : a'
-- Control flow
dispatch (Parser.DoBlock{doBlockExprs = exprs}) t = compileDoBlock compileExpr exprs t
dispatch (Parser.If{ifCond, ifThen, ifElse}) t = compileIf compileExpr ifCond ifThen ifElse t
dispatch (Parser.When{whenExpr, whenBranches, whenElse}) t = compileWhen compileExpr whenExpr whenBranches whenElse t
dispatch (Parser.Pipeline{pipelineLhs = a, pipelineRhs = rhs}) t = compilePipeline compileExpr a rhs t
dispatch (Parser.Then{thenLhs = a, thenRhs = b}) t = compileThen compileExpr a b t
-- Functions
dispatch expr@(Parser.FuncCall{}) t = compileFuncCall compileExpr expr t
dispatch expr@(Parser.FuncDec{}) t = compileFuncDec compileExpr expr t
dispatch expr@(Parser.FuncDef{}) t = compileFuncDef compileExpr expr t
dispatch expr@(Parser.Function{}) t = compileFunction compileExpr expr t
dispatch expr@(Parser.Lambda{}) t = compileLambda compileExpr expr t
dispatch expr@(Parser.Let{}) t = compileLet compileExpr expr t
dispatch expr@(Parser.Var{}) t = compileVar compileExpr expr t
dispatch expr@(Parser.ParenApply{}) t = compileParenApply compileExpr expr t
dispatch expr@(Parser.External{}) t = compileExternal compileExpr expr t
-- Structs / types
dispatch expr@(Parser.Struct{}) t = compileStruct compileExpr compileProgram expr t
dispatch expr@(Parser.StructLit{}) t = compileStructLit compileExpr compileProgram expr t
dispatch expr@(Parser.StructAccess{}) t = compileStructAccess compileExpr expr t
dispatch expr@(Parser.Cast{}) t = compileCast compileExpr compileProgram expr t
-- Traits / impls
dispatch expr@(Parser.Trait{}) t = compileTrait compileExpr expr t
dispatch expr@(Parser.Impl{}) t = compileImpl compileExpr expr t
-- Lists / tuples
dispatch expr@(Parser.ListConcat{}) t = compileLists compileExpr expr t
dispatch expr@(Parser.ListAdd{}) t = compileLists compileExpr expr t
dispatch expr@(Parser.ListLit{}) t = compileLists compileExpr expr t
dispatch expr@(Parser.TupleLit{}) t = compileLists compileExpr expr t
dispatch expr@(Parser.TupleAccess{}) t = compileLists compileExpr expr t
-- Imports
dispatch expr@(Parser.Import{}) t = compileImport compileExpr expr t
-- Misc / no-ops
dispatch (Parser.TypeLit{}) _ = return []
dispatch (Parser.ArrayAccess{}) _ = return []
dispatch (Parser.Ref{}) _ = return []
dispatch (Parser.Discard{}) _ = return []
dispatch (Parser.Target{}) _ = return []
dispatch (Parser.ExternDec{}) _ = return []
dispatch (Parser.ListPattern{}) _ = return []
dispatch x _ = error $ show x ++ " is not implemented"

compileProgram :: CompileProgram
compileProgram (Parser.Program expr _) = do
    prelude <- liftIO preludeExpr
    prelude' <- concatMapM (`compileExpr` Parser.Any) prelude
    freePart <- concatMapM (`compileExpr` Parser.Any) expr
    createVirtualFunctions
    funcs <- gets functions
    let functions' = concatMap function (reverse funcs)
    errors' <- gets errors
    let hasTopLevelLets = any (\case Parser.Let{} -> True; _ -> False) expr
    if null errors'
        then
            return $ Left $ prelude' ++ freePart ++ ([Jmp "main" | hasTopLevelLets]) ++ functions' ++ [Push $ DInt 0, Exit]
        else
            return $ Right errors'

compileProgramBare :: CompileProgram
compileProgramBare (Parser.Program expr _) = do
    freePart <- concatMapM (`compileExpr` Parser.Any) expr
    functions' <- gets functions >>= \x -> return $ concatMap function (reverse x)
    errors' <- gets errors
    if null errors'
        then
            return $ Left $ functions' ++ freePart ++ [Push $ DInt 0, Exit]
        else
            return $ Right errors'

preludeExpr :: IO [Parser.Expr]
preludeExpr = do
    i <- preludeFile
    case parseProgram (T.pack i) Parser.initCompilerFlags{Parser.needsMain = False} of
        Left err ->
            error $
                "Parse error:\n"
                    ++ renderErrors (parseErrorBundleToSourceErrors err (T.pack i)) i
        Right prog@(Parser.Program progExpr _) -> do
            _ <-
                compileDry prog "<prelude>" >>= \case
                    Right err -> compileFail "<prelude>" err (Map.singleton "<prelude>" i) >> error ""
                    Left p' -> return p'
            return $ progExpr ++ [Parser.FuncDef "__sep" [] (Parser.Placeholder anyPosition) anyPosition]

locateLabel :: [Instruction] -> String -> Int
locateLabel = locateLabelIn

compileDry :: Parser.Program -> String -> IO (Either [Instruction] [CompilerError])
compileDry prog sourcePath' =
    evalStateT
        (compileProgramBare prog)
        (initCompilerStateWithFile prog sourcePath')

groupErrorsByFile :: [CompilerError] -> Map.Map String [CompilerError]
groupErrorsByFile errs =
    Map.fromListWith (++) $
        map (\e@(CompilerError _ _ file) -> (file, [e])) errs

compilerErrorToSourceError :: CompilerError -> SourceError
compilerErrorToSourceError (CompilerError msg pos _) =
    SourceError{errorMessage = msg, errorPosition = pos}

renderErrorsForFile :: String -> [CompilerError] -> String -> Bool -> String
renderErrorsForFile filePath fileErrors content hasMultipleFiles
    | null content = renderPlainErrors filePath fileErrors hasMultipleFiles
    | otherwise = renderFormattedErrors filePath fileErrors content hasMultipleFiles
  where
    renderPlainErrors :: String -> [CompilerError] -> Bool -> String
    renderPlainErrors path errs multiple
        | multiple = path ++ ":\n" ++ unlines (map (\(CompilerError msg _ _) -> "  " ++ msg) errs)
        | otherwise = unlines (map (\(CompilerError msg _ _) -> msg) errs)

    renderFormattedErrors :: String -> [CompilerError] -> String -> Bool -> String
    renderFormattedErrors path errs content' multiple =
        let sourceErrors = map compilerErrorToSourceError errs
            rendered = renderErrors sourceErrors content'
         in if multiple then path ++ ":\n" ++ rendered else rendered

renderCompilerErrors :: [CompilerError] -> Map.Map String String -> String
renderCompilerErrors errs fileContents =
    let errorsByFile = groupErrorsByFile errs
        hasMultipleFiles = Map.size errorsByFile > 1
     in unlines $
            Map.elems $
                Map.mapWithKey
                    ( \filePath fileErrors ->
                        renderErrorsForFile
                            filePath
                            fileErrors
                            (Map.findWithDefault "" filePath fileContents)
                            hasMultipleFiles
                    )
                    errorsByFile

readMissingFileContents :: [CompilerError] -> Map.Map String String -> IO (Map.Map String String)
readMissingFileContents errs existingContents = do
    let errorFiles = nub $ map (\(CompilerError _ _ file) -> file) errs
    let missingFiles =
            filter
                ( \file ->
                    not (Map.member file existingContents)
                        && file /= ""
                        && file /= "no file"
                )
                errorFiles
    additionalContents <-
        Map.fromList
            <$> mapM
                ( \file -> do
                    content <- try (readFile file) :: IO (Either SomeException String)
                    case content of
                        Right c -> return (file, c)
                        Left _ -> return (file, "")
                )
                missingFiles
    return $ Map.union existingContents additionalContents

compileFail :: String -> [CompilerError] -> Map.Map String String -> IO ()
compileFail fileName errs fileContents = do
    allFileContents <- readMissingFileContents errs fileContents
    let rendered = renderCompilerErrors errs allFileContents
    let hasMultipleFiles = Map.size (groupErrorsByFile errs) > 1
    let output =
            if hasMultipleFiles
                then rendered
                else "\x1b[1m\x1b[36m" ++ fileName ++ "\x1b[0m \n" ++ rendered
    putStrLn output
