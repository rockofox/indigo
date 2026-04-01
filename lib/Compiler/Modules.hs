module Compiler.Modules where

import AST qualified as Parser
import Compiler.State
    ( CompileExpr
    , Compiler
    , CompilerState (..)
    , cerror
    , concatMapM
    , internalError
    )
import Control.Exception (SomeException, try)
import Control.Monad.State (MonadIO (liftIO), gets, modify)
import Data.Bifunctor (second)
import Data.Either (lefts, rights)
import Data.List (intercalate, isSuffixOf, nub, partition)
import Data.Map qualified as Map
import Data.Maybe (fromJust, fromMaybe, isJust, maybeToList)
import Data.Text qualified as T
import Parser (CompilerFlags (CompilerFlags), parseProgram)
import Parser qualified
import Paths_indigo qualified
import System.Directory (doesFileExist)
import System.Environment (getExecutablePath)
import System.FilePath
import VM (Instruction)

findSourceFile :: String -> [String] -> IO String
findSourceFile fileName paths = do
    dataFile <- Paths_indigo.getDataFileName fileName
    executablePathFile <- getExecutablePath >>= \x -> return $ takeDirectory x </> fileName
    let pathsToTry = map (</> fileName) (["/usr/local/lib/indigo/", "/usr/lib/indigo/"] ++ paths) ++ [executablePathFile, dataFile]
    firstThatExists <- firstM doesFileExist pathsToTry
    case firstThatExists of
        Just x -> return x
        Nothing -> error $ "Source file " ++ fileName ++ " not found. Tried:\n* " ++ intercalate "\n* " pathsToTry
  where
    firstM :: (Monad m) => (a -> m Bool) -> [a] -> m (Maybe a)
    firstM _ [] = return Nothing
    firstM p (x : xs) = do
        b <- p x
        if b then return (Just x) else firstM p xs

findSourceFile' :: String -> IO String
findSourceFile' fileName = findSourceFile fileName []

findFirstModuleFile :: [String] -> String -> IO (Maybe (String, String))
findFirstModuleFile [] _ = return Nothing
findFirstModuleFile (fileName : rest) sourcePath = do
    result <- try (findSourceFile fileName [sourcePath] >>= readFile) :: IO (Either SomeException String)
    case result of
        Right content -> return $ Just (fileName, content)
        Left _ -> findFirstModuleFile rest sourcePath

preludeFile :: IO String
preludeFile = findSourceFile' "std/prelude.in" >>= readFile

extractModuleName :: FilePath -> Parser.Program -> String
extractModuleName _ (Parser.Program _ (Just name)) = name
extractModuleName filePath _ =
    let baseName' = takeBaseName filePath
        withoutExt = if ".in" `isSuffixOf` baseName' then take (length baseName' - 3) baseName' else baseName'
        dirPath = takeDirectory filePath
        pathParts = splitDirectories dirPath
        modulePath = filter (not . null) pathParts
     in if null modulePath then withoutExt else intercalate "." (modulePath ++ [withoutExt])

buildModuleMap :: [FilePath] -> IO (Either String (Map.Map String (Parser.Program, FilePath)))
buildModuleMap filePaths = do
    results <- mapM parseFile filePaths
    let errors' = lefts results
    if not (null errors')
        then return $ Left (unlines errors')
        else do
            let programs = rights results
            let moduleNames = map (\(prog, path) -> extractModuleName path prog) programs
            let duplicates = findDuplicates moduleNames
            if not (null duplicates)
                then return $ Left $ "Duplicate module names: " ++ intercalate ", " duplicates
                else return $ Right $ Map.fromList $ zip moduleNames programs
  where
    parseFile :: FilePath -> IO (Either String (Parser.Program, FilePath))
    parseFile path = do
        content <- readFile path
        case parseProgram (T.pack content) Parser.initCompilerFlags{Parser.needsMain = False} of
            Left err -> return $ Left $ "Parse error in " ++ path ++ ":\n" ++ show err
            Right prog -> return $ Right (prog, path)

    findDuplicates :: [String] -> [String]
    findDuplicates xs = [x | (x, count) <- map (\x -> (x, length (filter (== x) xs))) (nub xs), count > 1]

compileImport :: CompileExpr -> Parser.Expr -> Parser.Type -> Compiler [Instruction]
compileImport compileExpr' (Parser.Import{objects = o, from = from, as = as, qualified = qualified, importPos}) expectedType = do
    if o /= ["*"]
        then do
            cerror "Only * imports are supported right now" importPos
            return []
        else do
            modules' <- gets modules
            currentModule' <- gets currentModule
            if from `elem` maybeToList currentModule'
                then do
                    cerror ("Circular import detected: module '" ++ from ++ "' imports itself") importPos
                    return []
                else do
                    (importedProg, importedPath) <- case Map.lookup from modules' of
                        Just (prog, path) -> return (prog, path)
                        Nothing -> do
                            sourcePath' <- gets sourcePath
                            let sourceDir = takeDirectory sourcePath'
                            let fileNamesToTry = [from ++ ".in", "std/" ++ from ++ ".in"]
                            fileResult <- liftIO $ findFirstModuleFile fileNamesToTry sourceDir
                            case fileResult of
                                Nothing -> do
                                    cerror ("Module '" ++ from ++ "' not found. Tried: " ++ intercalate ", " fileNamesToTry) importPos
                                    return (Parser.Program [] Nothing, "")
                                Just (fileName, i) -> do
                                    case parseProgram (T.pack i) Parser.initCompilerFlags{Parser.needsMain = False} of
                                        Left err -> do
                                            cerror ("Parse error in imported module '" ++ from ++ "':\n" ++ show err) importPos
                                            return (Parser.Program [] Nothing, "")
                                        Right p -> return (p, fileName)
                    let (Parser.Program exprs _) = importedProg
                    let moduleNameToUse = fromMaybe from (Parser.moduleName importedProg)
                    let prefix =
                            if qualified || isJust as
                                then if qualified then moduleNameToUse else fromJust as
                                else ""
                    let definedFuncs = concatMap extractDefinedFuncNames exprs
                    let definedStructs = concatMap extractDefinedStructNames exprs
                    let exprsToCompile = if null prefix then exprs else map (\e -> mangleExprForImport e prefix definedFuncs definedStructs) exprs
                    let (traitsAndStructsToCompile, otherExprsToCompile) = partition (\case Parser.Trait{} -> True; Parser.Struct{} -> True; _ -> False) exprsToCompile
                    previousSourceFile <- gets currentSourceFile
                    modify (\s -> s{currentSourceFile = importedPath})
                    _ <- concatMapM (`compileExpr'` expectedType) traitsAndStructsToCompile
                    result <- concatMapM (`compileExpr'` expectedType) otherExprsToCompile
                    modify (\s -> s{currentSourceFile = previousSourceFile})
                    return result
compileImport _ expr _ = internalError $ "compileImport: expected Import, got " ++ show expr

extractDefinedFuncNames :: Parser.Expr -> [String]
extractDefinedFuncNames (Parser.FuncDec{name}) = [name]
extractDefinedFuncNames (Parser.FuncDef{name}) = [name]
extractDefinedFuncNames (Parser.Function{dec = Parser.FuncDec{name}}) = [name]
extractDefinedFuncNames (Parser.Let{letName}) = [letName]
extractDefinedFuncNames _ = []

extractDefinedStructNames :: Parser.Expr -> [String]
extractDefinedStructNames (Parser.Struct{name}) = [name]
extractDefinedStructNames _ = []

mangleExprForImport :: Parser.Expr -> String -> [String] -> [String] -> Parser.Expr
mangleExprForImport (Parser.FuncDec{name, types, generics, funcDecPos}) prefix _ structs =
    Parser.FuncDec{name = prefix ++ "." ++ name, types = map (\t -> mangleTypeForImport t prefix structs) types, generics = generics, funcDecPos = funcDecPos}
mangleExprForImport (Parser.Function{def = fdef, dec, functionPos}) prefix funcs structs =
    Parser.Function{def = map (\e -> mangleExprForImport e prefix funcs structs) fdef, dec = mangleExprForImport dec prefix funcs structs, functionPos = functionPos}
mangleExprForImport (Parser.FuncDef{name, args, body, funcDefPos}) prefix funcs structs =
    Parser.FuncDef{name = prefix ++ "." ++ name, args = args, body = mangleExprForImport body prefix funcs structs, funcDefPos = funcDefPos}
mangleExprForImport (Parser.Struct{name, fields, refinement, refinementSrc, is, isValueStruct, generics, structPos}) prefix funcs structs =
    Parser.Struct{name = prefix ++ "." ++ name, fields = map (\(n, t) -> (n, mangleTypeForImport t prefix structs)) fields, refinement = fmap (\e -> mangleExprForImport e prefix funcs structs) refinement, refinementSrc = refinementSrc, is = is, isValueStruct = isValueStruct, generics = generics, structPos = structPos}
mangleExprForImport (Parser.StructLit{structLitName, structLitFields, structLitTypeArgs, structLitPos}) prefix funcs structs =
    let mangledName = if structLitName `elem` structs then prefix ++ "." ++ structLitName else structLitName
     in Parser.StructLit{structLitName = mangledName, structLitFields = map (second (\e -> mangleExprForImport e prefix funcs structs)) structLitFields, structLitTypeArgs = structLitTypeArgs, structLitPos = structLitPos}
mangleExprForImport (Parser.Trait{name, methods, generics, requiredProperties, refinement, refinementSrc, traitPos}) prefix funcs structs =
    Parser.Trait{name = prefix ++ "." ++ name, methods = map (\e -> mangleExprForImport e prefix funcs structs) methods, generics = generics, requiredProperties = map (\(n, t) -> (n, mangleTypeForImport t prefix structs)) requiredProperties, refinement = fmap (\e -> mangleExprForImport e prefix funcs structs) refinement, refinementSrc = refinementSrc, traitPos = traitPos}
mangleExprForImport (Parser.Impl{trait, traitTypeArgs, for, methods, implPos}) prefix funcs structs =
    Parser.Impl{trait = trait, traitTypeArgs = traitTypeArgs, for = mangleTypeForImport for prefix structs, methods = map (mangleImplMethod prefix funcs structs) methods, implPos = implPos}
mangleExprForImport (Parser.FuncCall{funcName, funcArgs, funcPos}) prefix funcs structs =
    let mangledName = if funcName `elem` funcs then prefix ++ "." ++ funcName else funcName
     in Parser.FuncCall{funcName = mangledName, funcArgs = map (\e -> mangleExprForImport e prefix funcs structs) funcArgs, funcPos = funcPos}
mangleExprForImport (Parser.Var{varName, varPos}) prefix _ _ =
    Parser.Var{varName = varName, varPos = varPos}
mangleExprForImport (Parser.StructAccess{structAccessStruct, structAccessField, structAccessPos}) prefix funcs structs =
    Parser.StructAccess{structAccessStruct = mangleExprForImport structAccessStruct prefix funcs structs, structAccessField = mangleExprForImport structAccessField prefix funcs structs, structAccessPos = structAccessPos}
mangleExprForImport (Parser.Let{letName, letValue, letPos}) prefix funcs structs =
    Parser.Let{letName = letName, letValue = mangleExprForImport letValue prefix funcs structs, letPos = letPos}
mangleExprForImport (Parser.If{ifCond, ifThen, ifElse, ifPos}) prefix funcs structs =
    Parser.If{ifCond = mangleExprForImport ifCond prefix funcs structs, ifThen = mangleExprForImport ifThen prefix funcs structs, ifElse = mangleExprForImport ifElse prefix funcs structs, ifPos = ifPos}
mangleExprForImport (Parser.DoBlock{doBlockExprs, doBlockPos}) prefix funcs structs =
    Parser.DoBlock{doBlockExprs = map (\e -> mangleExprForImport e prefix funcs structs) doBlockExprs, doBlockPos = doBlockPos}
mangleExprForImport (Parser.When{whenExpr, whenBranches, whenElse, whenPos}) prefix funcs structs =
    Parser.When{whenExpr = mangleExprForImport whenExpr prefix funcs structs, whenBranches = map (\(p, b) -> (mangleExprForImport p prefix funcs structs, mangleExprForImport b prefix funcs structs)) whenBranches, whenElse = fmap (\e -> mangleExprForImport e prefix funcs structs) whenElse, whenPos = whenPos}
mangleExprForImport (Parser.ListLit{listLitExprs, listLitPos}) prefix funcs structs =
    Parser.ListLit{listLitExprs = map (\e -> mangleExprForImport e prefix funcs structs) listLitExprs, listLitPos = listLitPos}
mangleExprForImport (Parser.TupleLit{tupleLitExprs, tupleLitPos}) prefix funcs structs =
    Parser.TupleLit{tupleLitExprs = map (\e -> mangleExprForImport e prefix funcs structs) tupleLitExprs, tupleLitPos = tupleLitPos}
mangleExprForImport (Parser.Lambda{lambdaArgs, lambdaBody, lambdaPos}) prefix funcs structs =
    Parser.Lambda{lambdaArgs = lambdaArgs, lambdaBody = mangleExprForImport lambdaBody prefix funcs structs, lambdaPos = lambdaPos}
mangleExprForImport (Parser.External{externalName, externalArgs, externalPos}) prefix funcs structs =
    Parser.External{externalName = prefix ++ "." ++ externalName, externalArgs = map (\e -> mangleExprForImport e prefix funcs structs) externalArgs, externalPos = externalPos}
mangleExprForImport (Parser.ParenApply{parenApplyExpr, parenApplyArgs, parenApplyPos}) prefix funcs structs =
    case parenApplyExpr of
        Parser.Var{varName}
            | varName `elem` funcs ->
                Parser.ParenApply{parenApplyExpr = Parser.Var{varName = prefix ++ "." ++ varName, varPos = Parser.anyPosition}, parenApplyArgs = map (\e -> mangleExprForImport e prefix funcs structs) parenApplyArgs, parenApplyPos = parenApplyPos}
        _ -> Parser.ParenApply{parenApplyExpr = mangleExprForImport parenApplyExpr prefix funcs structs, parenApplyArgs = map (\e -> mangleExprForImport e prefix funcs structs) parenApplyArgs, parenApplyPos = parenApplyPos}
mangleExprForImport (Parser.Add{addLhs, addRhs, addPos}) prefix funcs structs =
    Parser.Add{addLhs = mangleExprForImport addLhs prefix funcs structs, addRhs = mangleExprForImport addRhs prefix funcs structs, addPos = addPos}
mangleExprForImport (Parser.Sub{subLhs, subRhs, subPos}) prefix funcs structs =
    Parser.Sub{subLhs = mangleExprForImport subLhs prefix funcs structs, subRhs = mangleExprForImport subRhs prefix funcs structs, subPos = subPos}
mangleExprForImport (Parser.Mul{mulLhs, mulRhs, mulPos}) prefix funcs structs =
    Parser.Mul{mulLhs = mangleExprForImport mulLhs prefix funcs structs, mulRhs = mangleExprForImport mulRhs prefix funcs structs, mulPos = mulPos}
mangleExprForImport (Parser.Div{divLhs, divRhs, divPos}) prefix funcs structs =
    Parser.Div{divLhs = mangleExprForImport divLhs prefix funcs structs, divRhs = mangleExprForImport divRhs prefix funcs structs, divPos = divPos}
mangleExprForImport (Parser.ListAdd{listAddLhs, listAddRhs, listAddPos}) prefix funcs structs =
    Parser.ListAdd{listAddLhs = mangleExprForImport listAddLhs prefix funcs structs, listAddRhs = mangleExprForImport listAddRhs prefix funcs structs, listAddPos = listAddPos}
mangleExprForImport (Parser.UnaryMinus{unaryMinusExpr, unaryMinusPos}) prefix funcs structs =
    Parser.UnaryMinus{unaryMinusExpr = mangleExprForImport unaryMinusExpr prefix funcs structs, unaryMinusPos = unaryMinusPos}
mangleExprForImport (Parser.Cast{castExpr, castType, castPos}) prefix funcs structs =
    Parser.Cast{castExpr = mangleExprForImport castExpr prefix funcs structs, castType = castType, castPos = castPos}
mangleExprForImport (Parser.TupleAccess{tupleAccessTuple, tupleAccessIndex, tupleAccessPos}) prefix funcs structs =
    Parser.TupleAccess{tupleAccessTuple = mangleExprForImport tupleAccessTuple prefix funcs structs, tupleAccessIndex = tupleAccessIndex, tupleAccessPos = tupleAccessPos}
mangleExprForImport x _ _ _ = x

mangleImplMethod :: String -> [String] -> [String] -> Parser.Expr -> Parser.Expr
mangleImplMethod prefix funcs structs (Parser.FuncDef{name, args, body, funcDefPos}) =
    Parser.FuncDef{name = name, args = args, body = mangleExprForImport body prefix funcs structs, funcDefPos = funcDefPos}
mangleImplMethod prefix funcs structs expr = mangleExprForImport expr prefix funcs structs

mangleTypeForImport :: Parser.Type -> String -> [String] -> Parser.Type
mangleTypeForImport (Parser.StructT name typeArgs) prefix structs =
    let mangledName = if name `elem` structs then prefix ++ "." ++ name else name
     in Parser.StructT mangledName (map (\t -> mangleTypeForImport t prefix structs) typeArgs)
mangleTypeForImport (Parser.List t) prefix structs = Parser.List (mangleTypeForImport t prefix structs)
mangleTypeForImport (Parser.Tuple ts) prefix structs = Parser.Tuple (map (\t -> mangleTypeForImport t prefix structs) ts)
mangleTypeForImport (Parser.Fn args ret) prefix structs = Parser.Fn (map (\t -> mangleTypeForImport t prefix structs) args) (mangleTypeForImport ret prefix structs)
mangleTypeForImport t _ _ = t
