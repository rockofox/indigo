{-# LANGUAGE CPP #-}

module BytecodeCompiler where

import AST (Position (..), anyPosition, zeroPosition)
import AST qualified as Parser
import AST qualified as Parser.Type (Type (Unknown))
import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless, when, zipWithM, (>=>))
import Control.Monad.Loops (allM, firstM)
import Control.Monad.State (MonadIO (liftIO), StateT, evalStateT, get, gets, modify)
import Data.Bifunctor (second)
import Data.Either (lefts, rights)
import Data.Functor ((<&>))
import Data.List (elemIndex, find, groupBy, inits, intercalate, isInfixOf, isSuffixOf, nub)
import Data.List.Split qualified
import Data.Map qualified
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, isNothing, mapMaybe, maybeToList)
import Data.String
import Data.Text (isPrefixOf, splitOn)
import Data.Text qualified
import Data.Text qualified as T
import Data.Vector qualified as V
import ErrorRenderer (SourceError (..), parseErrorBundleToSourceErrors, renderErrors)
import Foreign (nullPtr, ptrToWordPtr)
import Foreign.C.Types ()
import GHC.Generics (Generic)
import Parser (CompilerFlags (CompilerFlags), name, parseProgram, types)
import Parser qualified
import Paths_indigo qualified
import System.Directory (doesFileExist)
import System.Environment
import System.FilePath
import Util
import VM
import Prelude hiding (FilePath, lex)

data Function = Function
    { baseName :: String
    , funame :: String
    , function :: [Instruction]
    , types :: [Parser.Type]
    , context :: String
    }
    deriving (Show, Generic)

data External = External
    { name :: String
    , returnType :: Parser.Type
    , args :: [Parser.Type]
    , from :: String
    }
    deriving (Show)

data CompilerError = CompilerError
    { errorMessage :: String
    , errorPosition :: AST.Position
    }
    deriving (Show, Eq)

data CompilerState a = CompilerState
    { program :: Parser.Program
    , functions :: [Function]
    , funcDefs :: [Parser.Expr]
    , funcDecs :: [Parser.Expr]
    , structDecs :: [Parser.Expr]
    , lastLabel :: Int
    , lets :: [Let]
    , traits :: [Parser.Expr]
    , impls :: [Parser.Expr]
    , currentContext :: String -- TODO: Nested contexts
    , contextPath :: [String]
    , externals :: [External]
    , functionsByTrait :: [(String, String, String, Parser.Expr)]
    , errors :: [CompilerError]
    , sourcePath :: String
    , skipRefinementCheck :: Bool
    , modules :: Map.Map String (Parser.Program, FilePath)
    , currentModule :: Maybe String
    }

data Let = Let
    { name :: String
    , vtype :: Parser.Type
    , context :: String
    }
    deriving (Show)

initCompilerState :: Parser.Program -> CompilerState a
initCompilerState prog =
    CompilerState
        prog
        []
        []
        []
        []
        0
        []
        []
        []
        "__outside"
        ["__outside"]
        []
        []
        []
        "no file"
        False
        Map.empty
        Nothing

initCompilerStateWithFile :: Parser.Program -> String -> CompilerState a
initCompilerStateWithFile prog sourcePath' =
    CompilerState
        prog
        []
        []
        []
        []
        0
        []
        []
        []
        "__outside"
        ["__outside"]
        []
        []
        []
        sourcePath'
        False
        Map.empty
        Nothing

initCompilerStateWithModules :: Map.Map String (Parser.Program, FilePath) -> Parser.Program -> String -> CompilerState a
initCompilerStateWithModules modules' prog sourcePath' =
    CompilerState
        prog
        []
        []
        []
        []
        0
        []
        []
        []
        "__outside"
        ["__outside"]
        []
        []
        []
        sourcePath'
        False
        modules'
        (Parser.moduleName prog)

cerror :: String -> AST.Position -> StateT (CompilerState a) IO ()
cerror msg pos = do
    unless (pos == AST.zeroPosition) $ modify (\s -> s{errors = errors s ++ [CompilerError msg pos]})

extractPosition :: Parser.Expr -> AST.Position
extractPosition (Parser.Var{varPos}) = varPos
extractPosition (Parser.FuncCall{funcPos}) = funcPos
extractPosition (Parser.StructLit{structLitPos}) = structLitPos
extractPosition (Parser.ParenApply{parenApplyPos}) = parenApplyPos
extractPosition (Parser.BoolLit{boolPos}) = boolPos
extractPosition (Parser.IntLit{intPos}) = intPos
extractPosition (Parser.StringLit{stringPos}) = stringPos
extractPosition (Parser.FloatLit{floatPos}) = floatPos
extractPosition (Parser.DoubleLit{doublePos}) = doublePos
extractPosition (Parser.CharLit{charPos}) = charPos
extractPosition (Parser.If{ifPos}) = ifPos
extractPosition (Parser.Let{letPos}) = letPos
extractPosition (Parser.FuncDef{funcDefPos}) = funcDefPos
extractPosition (Parser.FuncDec{funcDecPos}) = funcDecPos
extractPosition (Parser.Function{functionPos}) = functionPos
extractPosition (Parser.DoBlock{doBlockPos}) = doBlockPos
extractPosition (Parser.ExternDec{externDecPos}) = externDecPos
extractPosition (Parser.Add{addPos}) = addPos
extractPosition (Parser.Sub{subPos}) = subPos
extractPosition (Parser.Mul{mulPos}) = mulPos
extractPosition (Parser.Div{divPos}) = divPos
extractPosition (Parser.Eq{eqPos}) = eqPos
extractPosition (Parser.Neq{neqPos}) = neqPos
extractPosition (Parser.Lt{ltPos}) = ltPos
extractPosition (Parser.Gt{gtPos}) = gtPos
extractPosition (Parser.Le{lePos}) = lePos
extractPosition (Parser.Ge{gePos}) = gePos
extractPosition (Parser.And{andPos}) = andPos
extractPosition (Parser.Or{orPos}) = orPos
extractPosition (Parser.Not{notPos}) = notPos
extractPosition (Parser.UnaryMinus{unaryMinusPos}) = unaryMinusPos
extractPosition (Parser.Placeholder{placeholderPos}) = placeholderPos
extractPosition (Parser.Discard{discardPos}) = discardPos
extractPosition (Parser.Import{importPos}) = importPos
extractPosition (Parser.Ref{refPos}) = refPos
extractPosition (Parser.Struct{structPos}) = structPos
extractPosition (Parser.StructAccess{structAccessPos}) = structAccessPos
extractPosition (Parser.ListLit{listLitPos}) = listLitPos
extractPosition (Parser.ListPattern{listPatternPos}) = listPatternPos
extractPosition (Parser.ListConcat{listConcatPos}) = listConcatPos
extractPosition (Parser.ListAdd{listAddPos}) = listAddPos
extractPosition (Parser.ArrayAccess{arrayAccessPos}) = arrayAccessPos
extractPosition (Parser.Modulo{moduloPos}) = moduloPos
extractPosition (Parser.Power{powerPos}) = powerPos
extractPosition (Parser.Target{targetPos}) = targetPos
extractPosition (Parser.Then{thenPos}) = thenPos
extractPosition (Parser.Pipeline{pipelinePos}) = pipelinePos
extractPosition (Parser.Lambda{lambdaPos}) = lambdaPos
extractPosition (Parser.Cast{castPos}) = castPos
extractPosition (Parser.TypeLit{typeLitPos}) = typeLitPos
extractPosition (Parser.Flexible{flexiblePos}) = flexiblePos
extractPosition (Parser.Trait{traitPos}) = traitPos
extractPosition (Parser.Impl{implPos = pos}) = pos
extractPosition (Parser.StrictEval{strictEvalPos}) = strictEvalPos
extractPosition (Parser.External{externalPos}) = externalPos
extractPosition (Parser.When{whenPos}) = whenPos
extractPosition (Parser.TupleLit{tupleLitPos}) = tupleLitPos
extractPosition (Parser.TupleAccess{tupleAccessPos}) = tupleAccessPos

getErrorCount :: StateT (CompilerState a) IO Int
getErrorCount = gets (length . errors)

getBinOpPosition :: Parser.Expr -> Parser.Expr -> AST.Position
getBinOpPosition x y =
    let px = extractPosition x
        py = extractPosition y
     in case (px, py) of
            (AST.Position (x1, x2), AST.Position (y1, y2))
                | x1 >= 0 && y1 >= 0 ->
                    AST.Position (min x1 y1, max x2 y2)
            (AST.Position (p1, _), _) | p1 >= 0 -> px
            (_, AST.Position (p2, _)) | p2 >= 0 -> py
            _ -> AST.anyPosition

compileBinOp :: String -> Parser.Expr -> Parser.Expr -> Parser.Type -> Instruction -> StateT (CompilerState a) IO [Instruction]
compileBinOp opName x y expectedType op = do
    case (x, y) of
        (Parser.Flexible{}, _) -> doBinOp opName x y expectedType op
        (_, Parser.Flexible{}) -> doBinOp opName x y expectedType op
        _ -> do
            errorCountBefore <- getErrorCount
            let pos = getBinOpPosition x y
            _ <- compileExpr (Parser.FuncCall{funcName = opName, funcArgs = [x, y], funcPos = pos}) expectedType
            errorCountAfter <- getErrorCount
            if errorCountAfter > errorCountBefore
                then return []
                else doBinOp opName x y expectedType op

compileBinOpSeq :: String -> Parser.Expr -> Parser.Expr -> Parser.Type -> [Instruction] -> StateT (CompilerState a) IO [Instruction]
compileBinOpSeq opName x y expectedType ops = do
    errorCountBefore <- getErrorCount
    let pos = getBinOpPosition x y
    _ <- compileExpr (Parser.FuncCall{funcName = opName, funcArgs = [x, y], funcPos = pos}) expectedType
    errorCountAfter <- getErrorCount
    if errorCountAfter > errorCountBefore
        then return []
        else do
            id' <- allocId
            let aName = "__op_a_" ++ show id'
            let bName = "__op_b_" ++ show id'
            x' <- compileExpr x expectedType
            y' <- compileExpr y expectedType
            cast <- case (x, y) of
                (Parser.Flexible{}, Parser.Flexible{}) -> error "Double cast"
                (Parser.Flexible{}, _) -> return $ Cast : y'
                (_, Parser.Flexible{}) -> return $ [Swp, Cast] ++ x'
                _ -> return []
            return (x' ++ LStore aName : y' ++ [LStore bName, LLoad aName, LLoad bName] ++ cast ++ ops)

allocId :: StateT (CompilerState a) IO Int
allocId = do
    s <- gets lastLabel
    modify (\s' -> s'{lastLabel = s + 1})
    return s

preludeExpr :: IO [Parser.Expr]
preludeExpr = do
    i <- liftIO preludeFile
    case parseProgram (T.pack i) CompilerFlags{verboseMode = False, needsMain = False} of -- FIXME: pass on flags
        Left err -> error $ "Parse error:\n" ++ renderErrors (parseErrorBundleToSourceErrors err (T.pack i)) i
        Right prog@(Parser.Program progExpr _) -> do
            _ <-
                liftIO $
                    compileDry prog "<prelude>" >>= \case
                        Right err -> compileFail "<prelude>" err i >> error ""
                        Left p' -> return p'
            return $ progExpr ++ [Parser.FuncDef "__sep" [] (Parser.Placeholder anyPosition) anyPosition]

compileProgram :: Parser.Program -> StateT (CompilerState a) IO (Either [Instruction] [CompilerError])
compileProgram (Parser.Program expr _) = do
    prelude <- liftIO preludeExpr
    prelude' <- concatMapM (`compileExpr` Parser.Any) prelude
    freePart <- concatMapM (`compileExpr` Parser.Any) expr
    createVirtualFunctions
    functions' <- gets functions >>= \x -> return $ concatMap function (reverse x)
    errors' <- gets errors
    let hasTopLevelLets = any (\case Parser.Let{} -> True; _ -> False) expr
    if null errors'
        then
            return $ Left $ prelude' ++ freePart ++ ([Jmp "main" | hasTopLevelLets]) ++ functions' ++ [Push $ DInt 0, Exit]
        else
            return $ Right errors'

compileProgramBare :: Parser.Program -> StateT (CompilerState a) IO (Either [Instruction] [CompilerError])
compileProgramBare (Parser.Program expr _) = do
    freePart <- concatMapM (`compileExpr` Parser.Any) expr
    functions' <- gets functions >>= \x -> return $ concatMap function (reverse x)
    errors' <- gets errors
    if null errors'
        then
            return $ Left $ functions' ++ freePart ++ [Push $ DInt 0, Exit]
        else
            return $ Right errors'

findAnyFunction :: String -> [Function] -> Maybe Function
findAnyFunction funcName xs = do
    -- TODO: I don't like this function
    let candidates = filter (\y -> baseName y == funcName && funame y /= "main") xs

    let ids = map ((!! 1) . splitOn "#" . Data.Text.pack . funame) candidates
    let ids' = map (read . Data.Text.unpack) ids :: [Int]
    let minId = minimum ids'
    find (\y -> funame y == funcName ++ "#" ++ show minId) candidates

findFunction :: String -> [Function] -> [Parser.Type] -> Maybe Function
findFunction funcName xs typess = do
    -- TODO: I don't like this function
    let candidates = filter (\y -> baseName y == funcName && funame y /= "main" && typesMatch y typess) xs
    let candidates' = case filter (`typesMatchExactly` typess) candidates of
            [] -> candidates
            candidates'' -> candidates''
    let ids = map ((!! 1) . splitOn "#" . Data.Text.pack . funame) candidates'
    let ids' = map (read . Data.Text.unpack) ids :: [Int]
    let minId = minimum ids'

    case find (\y -> funame y == funcName ++ "#" ++ show minId) candidates' of
        Just x -> Just x
        Nothing -> findAnyFunction funcName xs

internalFunctions :: [String]
internalFunctions = ["unsafePrint", "unsafeGetLine", "unsafeGetChar", "unsafeRandom", "abs", "root", "sqrt"]
#ifdef POSIX_IO
    ++ ["unsafeOpenFile", "unsafeReadFile", "unsafeWriteFile", "unsafeCloseFile", "unsafeSocket", "unsafeBind", "unsafeListen", "unsafeAccept", "unsafeConnect", "unsafeSend", "unsafeRecv", "unsafeCloseSocket"]
#endif

typesMatch :: Function -> [Parser.Type] -> Bool
typesMatch fun typess = all (uncurry Parser.compareTypes) (zip fun.types typess) && length typess <= length fun.types

typesMatchExactly :: Function -> [Parser.Type] -> Bool
typesMatchExactly fun typess = all (uncurry (==)) (zip fun.types typess) && length typess <= length fun.types

typeCompatible :: Parser.Type -> Parser.Type -> StateT (CompilerState a) IO Bool
typeCompatible x y = do
    let x' = typeToString x
    let y' = typeToString y
    impls <- implsFor x'
    let equal = Parser.compareTypes x y
    if equal
        then return True
        else do
            let numericTypes = ["Int", "Float", "Double"]
            let bothNumeric = x' `elem` numericTypes && y' `elem` numericTypes
            let boolAliases = ["Bool", "Boolean"]
            let bothBool = x' `elem` boolAliases && y' `elem` boolAliases
            if bothNumeric || bothBool
                then return True
                else return $ y' `elem` impls

compareTypes' :: Parser.Type -> Parser.Type -> [Parser.GenericExpr] -> StateT (CompilerState a) IO Bool
compareTypes' (Parser.List x) (Parser.List y) generics = compareTypes' x y generics
compareTypes' (Parser.List Parser.Any) (Parser.StructT "String" []) _ = return True
compareTypes' (Parser.List (Parser.StructT "Char" [])) (Parser.StructT "String" []) _ = return True
compareTypes' (Parser.StructT "String" []) (Parser.List (Parser.StructT "Char" [])) _ = return True
compareTypes' (Parser.StructT "String" []) (Parser.List Parser.Any) _ = return True
compareTypes' aT (Parser.StructT b bArgs) generics = case aT of
    Parser.StructT a aArgs -> do
        if length aArgs == length bArgs && all (uncurry (==)) (zip aArgs bArgs)
            then do
                let gen = find (\(Parser.GenericExpr name _) -> name == b) generics
                case gen of
                    Just (Parser.GenericExpr _ (Just (Parser.StructT t _))) -> compStructs a t
                    Just (Parser.GenericExpr _ _) -> return True
                    Nothing -> compStructs a b
            else return False
    Parser.Unknown -> return True
    _ -> do
        let gen = find (\(Parser.GenericExpr name _) -> name == b) generics
        case gen of
            Just (Parser.GenericExpr _ (Just t)) -> return $ Parser.compareTypes aT t
            Just (Parser.GenericExpr _ Nothing) -> return True
            Nothing -> return False
  where
    compStructs :: String -> String -> StateT (CompilerState a) IO Bool
    compStructs structA structB = do
        implsA <- implsFor structA
        return $ structB `elem` implsA || structA == structB
compareTypes' a b _ = return $ Parser.compareTypes a b

allTheSame :: [Parser.Type] -> Bool
allTheSame [] = True
allTheSame [_] = True
allTheSame xs = all (compareSame $ head xs) (tail xs)
  where
    compareSame :: Parser.Type -> Parser.Type -> Bool
    compareSame (Parser.List x) (Parser.List y) = compareSame x y
    compareSame x y = x == y

functionTypesAcceptable :: [Parser.Type] -> [Parser.Type] -> [Parser.GenericExpr] -> StateT (CompilerState a) IO Bool
functionTypesAcceptable use def generics = do
    let genericNames = map (\(Parser.GenericExpr name _) -> name) generics
    let useAndDef = zip use def
    let udStructs = concatMap (\(a, b) -> case (a, b) of (_, Parser.StructT _ _) -> [(a, b)]; (Parser.List c'@(Parser.StructT _ _), Parser.List c@(Parser.StructT _ _)) -> [(c', c)]; _ -> []) useAndDef
    let udStructsGeneric = filter (\case (_, Parser.StructT b _) -> b `elem` genericNames; (_, Parser.List (Parser.StructT b _)) -> b `elem` genericNames; _ -> error "Impossible") udStructs
    let groupedUdStructsGeneric = map (\x -> (x, fst <$> filter (\case (_, Parser.StructT b _) -> b == x; (_, Parser.List (Parser.StructT b _)) -> b == x; _ -> error "Impossible") udStructsGeneric)) genericNames
    let genericsMatch = all (\(_, types) -> allTheSame types) groupedUdStructsGeneric
    typesMatch' <- allM (uncurry $ uncurry compareTypes') $ zip (zip use def) [generics]
    return $ typesMatch' && genericsMatch

evaluateFunction :: [Parser.Type] -> Int -> Parser.Type
evaluateFunction types taken = case drop taken types of
    [] -> Parser.Unknown
    [x] -> x
    xs -> Parser.Fn xs (last xs)

unmangleFunctionName :: String -> String
unmangleFunctionName = takeWhile (/= '#')

findSourceFile :: String -> [String] -> IO String
findSourceFile fileName paths = do
    dataFile <- Paths_indigo.getDataFileName fileName
    executablePathFile <- getExecutablePath >>= \x -> return $ takeDirectory x </> fileName
    let pathsToTry = map (</> fileName) (["/usr/local/lib/indigo/", "/usr/lib/indigo/"] ++ paths) ++ [executablePathFile, dataFile]
    firstThatExists <- firstM doesFileExist pathsToTry
    case firstThatExists of
        Just x -> return x
        Nothing -> error $ "Source file " ++ fileName ++ " not found. Tried:\n* " ++ intercalate "\n* " pathsToTry

findSourceFile' :: String -> IO String
findSourceFile' fileName = findSourceFile fileName []

preludeFile :: IO String
preludeFile = findSourceFile' "std/prelude.in" >>= readFile

extractModuleName :: FilePath -> Parser.Program -> String
extractModuleName filePath (Parser.Program _ (Just name)) = name
extractModuleName filePath _ =
    let baseName = takeBaseName filePath
        withoutExt = if ".in" `isSuffixOf` baseName then take (length baseName - 3) baseName else baseName
        dirPath = takeDirectory filePath
        pathParts = splitDirectories dirPath
        modulePath = filter (not . null) pathParts
     in if null modulePath then withoutExt else intercalate "." (modulePath ++ [withoutExt])

buildModuleMap :: [FilePath] -> IO (Either String (Map.Map String (Parser.Program, FilePath)))
buildModuleMap filePaths = do
    results <- mapM parseFile filePaths
    let errors = lefts results
    if not (null errors)
        then return $ Left (unlines errors)
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

doBinOp :: String -> Parser.Expr -> Parser.Expr -> Parser.Type -> Instruction -> StateT (CompilerState a) IO [Instruction]
doBinOp opName x y expectedType op = do
    id' <- allocId
    functions' <- gets functions
    funcDecs' <- gets funcDecs
    let f = findAnyFunction (Data.Text.unpack $ Data.Text.toLower $ Data.Text.pack $ show op) functions'
    xType <- typeOf x
    yType <- typeOf y
    let argTypes = [xType, yType]
    let foundFunc = findFunction opName functions' argTypes
    let hasFuncDec = isJust $ find (\case Parser.FuncDec{name} -> name == opName; _ -> False) funcDecs'
    let aName = "__op_a_" ++ show id'
    let bName = "__op_b_" ++ show id'
    x' <- compileExpr x expectedType
    y' <- compileExpr y expectedType

    let hasFlexible = case (x, y) of
            (Parser.Flexible{}, _) -> True
            (_, Parser.Flexible{}) -> True
            _ -> False

    castInstrs <- case (x, y) of
        (Parser.Flexible{}, Parser.Flexible{}) -> error "Double cast"
        (Parser.Flexible{}, _) -> do
            let targetType = compileTypeFromType yType
            return ([LLoad aName] ++ targetType ++ [Cast, LLoad bName])
        (_, Parser.Flexible{}) -> do
            let targetType = compileTypeFromType xType
            return ([LLoad bName] ++ targetType ++ [Cast, LLoad aName, Swp])
        _ -> return [LLoad aName, LLoad bName]
    let finalOp = case (foundFunc, hasFuncDec, hasFlexible) of
            (Just fun, _, False) -> Call (funame fun)
            (Nothing, True, False) -> Call opName
            _ -> op
    case (x, y) of
        (Parser.Placeholder _, Parser.Placeholder _) -> return [PushPf (funame $ fromJust f) 0]
        (Parser.Placeholder _, _) -> return $ y' ++ [PushPf (funame $ fromJust f) 1]
        (_, Parser.Placeholder _) -> return $ x' ++ [PushPf (funame $ fromJust f) 1]
        _ -> return (x' ++ LStore aName : y' ++ [LStore bName] ++ castInstrs ++ [finalOp])
  where
    compileTypeFromType :: Parser.Type -> [Instruction]
    compileTypeFromType (Parser.StructT "Int" []) = [Push $ DInt 0]
    compileTypeFromType (Parser.StructT "Float" []) = [Push $ DFloat 0.0]
    compileTypeFromType (Parser.StructT "Double" []) = [Push $ DDouble 0.0]
    compileTypeFromType (Parser.StructT "Bool" []) = [Push $ DBool False]
    compileTypeFromType (Parser.StructT "Char" []) = [Push $ DChar '\0']
    compileTypeFromType (Parser.StructT "String" []) = [Push $ DString ""]
    compileTypeFromType (Parser.StructT "CPtr" []) = [Push $ DCPtr $ ptrToWordPtr nullPtr]
    compileTypeFromType _ = [Push DNone]

safeTypeOf :: Parser.Expr -> StateT (CompilerState a) IO Parser.Type
safeTypeOf (Parser.Let{}) = return Parser.Unknown
safeTypeOf (Parser.FuncDef{}) = return Parser.Unknown
safeTypeOf (Parser.FuncDec{}) = return Parser.Unknown
safeTypeOf (Parser.ExternDec{}) = return Parser.Unknown
safeTypeOf (Parser.Discard{}) = return Parser.Unknown
safeTypeOf (Parser.Import{}) = return Parser.Unknown
safeTypeOf (Parser.Ref{}) = return Parser.Unknown
safeTypeOf (Parser.Struct{}) = return Parser.Unknown
safeTypeOf (Parser.ArrayAccess{}) = return Parser.Unknown
safeTypeOf (Parser.Target{}) = return Parser.Unknown
safeTypeOf (Parser.Trait{}) = return Parser.Unknown
safeTypeOf (Parser.Impl{}) = return Parser.Unknown
safeTypeOf (Parser.External{}) = return Parser.Unknown
safeTypeOf (Parser.DoBlock exprs _) = if null exprs then return Parser.None else safeTypeOf $ last exprs
safeTypeOf (Parser.If _ b _ _) = safeTypeOf b
safeTypeOf (Parser.When _ branches else_ _) = case branches of
    [] -> maybe (return Parser.Unknown) safeTypeOf else_
    ((_, body) : _) -> safeTypeOf body
safeTypeOf expr = typeOf expr

typeOf :: Parser.Expr -> StateT (CompilerState a) IO Parser.Type
typeOf Parser.FuncCall{funcName, funcArgs} = do
    funcDecs' <- gets funcDecs
    let a =
            maybe
                [Parser.Type.Unknown]
                Parser.types
                (find (\x -> x.name == funcName) funcDecs')
    return $ evaluateFunction a (length funcArgs)
typeOf Parser.Var{varName = varName} = do
    lets' <- gets lets
    let a = maybe Parser.Unknown vtype (find (\x -> x.name == varName) lets')
    return a
typeOf (Parser.IntLit _ _) = return $ Parser.StructT "Int" []
typeOf (Parser.FloatLit _ _) = return $ Parser.StructT "Float" []
typeOf (Parser.BoolLit _ _) = return $ Parser.StructT "Bool" []
typeOf (Parser.StringLit _ _) = return $ Parser.StructT "String" []
typeOf (Parser.Add x _ _) = typeOf x
typeOf (Parser.Sub x _ _) = typeOf x
typeOf (Parser.Mul x _ _) = typeOf x
typeOf (Parser.Div x _ _) = typeOf x
typeOf (Parser.Power x _ _) = typeOf x
typeOf (Parser.UnaryMinus x _) = typeOf x
typeOf (Parser.Eq{}) = return $ Parser.StructT "Bool" []
typeOf (Parser.Neq{}) = return $ Parser.StructT "Bool" []
typeOf (Parser.Lt{}) = return $ Parser.StructT "Bool" []
typeOf (Parser.Gt{}) = return $ Parser.StructT "Bool" []
typeOf (Parser.Le{}) = return $ Parser.StructT "Bool" []
typeOf (Parser.Ge{}) = return $ Parser.StructT "Bool" []
typeOf (Parser.And{}) = return $ Parser.StructT "Bool" []
typeOf (Parser.Or{}) = return $ Parser.StructT "Bool" []
typeOf (Parser.Not _ _) = return $ Parser.StructT "Bool" []
typeOf (Parser.Placeholder _) = return Parser.Any
typeOf (Parser.Let{}) = error "Cannot infer type of let"
typeOf (Parser.If _ b _ _) = typeOf b
typeOf (Parser.FuncDef{}) = error "Cannot infer type of function definition"
typeOf x@(Parser.FuncDec{}) = error $ "Cannot infer type of function declaration " ++ show x
typeOf (Parser.Function{}) = return Parser.Unknown -- error "Cannot infer type of modern function"
typeOf (Parser.DoBlock x _) = if null x then return Parser.None else typeOf $ last x
typeOf (Parser.ExternDec{}) = error "Cannot infer type of extern declaration"
typeOf (Parser.Discard _ _) = error "Cannot infer type of discard"
typeOf (Parser.Import{}) = error "Cannot infer type of import"
typeOf (Parser.Ref _ _) = error "Cannot infer type of ref"
typeOf (Parser.Struct{}) = error "Cannot infer type of struct"
typeOf (Parser.StructLit x _ typeArgs _) = return $ Parser.StructT x typeArgs
typeOf (Parser.ListLit [Parser.Var{varName}] _) = return $ Parser.List $ Parser.StructT varName []
typeOf (Parser.ListLit x _) = case x of
    [] -> return $ Parser.List Parser.Any
    (y : _) -> typeOf y <&> Parser.List
typeOf (Parser.ArrayAccess{}) = error "Cannot infer type of array access"
typeOf (Parser.Modulo x _ _) = typeOf x
typeOf (Parser.Target{}) = error "Cannot infer type of target"
typeOf (Parser.ListConcat x _ _) = typeOf x
typeOf (Parser.ListPattern _ _) = return $ Parser.List Parser.Any
typeOf (Parser.StructAccess _ s _) = typeOf s
typeOf (Parser.Pipeline _ b _) = typeOf b
typeOf (Parser.Lambda{lambdaArgs, lambdaBody}) = do
    returnType <- typeOf lambdaBody
    let argTypes = replicate (length lambdaArgs) Parser.Any
    return $ Parser.Fn argTypes returnType
typeOf (Parser.Cast _ (Parser.Var to _) _) = return $ Parser.StructT to []
typeOf (Parser.Cast _ b _) = typeOf b
typeOf (Parser.TypeLit x _) = return x
typeOf (Parser.Flexible x _) = typeOf x
typeOf (Parser.Trait{}) = error "Cannot infer type of trait"
typeOf (Parser.Impl{}) = error "Cannot infer type of impl"
typeOf (Parser.Then _ b _) = typeOf b
typeOf (Parser.StrictEval x _) = typeOf x
typeOf (Parser.External{}) = error "Cannot infer type of external"
typeOf (Parser.CharLit _ _) = return $ Parser.StructT "Char" []
typeOf (Parser.DoubleLit _ _) = return $ Parser.StructT "Double" []
typeOf (Parser.ParenApply a _ _) = typeOf a
typeOf (Parser.ListAdd x _ _) = typeOf x
typeOf (Parser.When _ branches else_ _) =
    case branches of
        [] -> maybe (return Parser.Unknown) typeOf else_
        ((_, body) : _) -> typeOf body
typeOf (Parser.TupleLit exprs _) = mapM typeOf exprs <&> Parser.Tuple
typeOf (Parser.TupleAccess tupleExpr index _) = do
    tupleType <- typeOf tupleExpr
    case tupleType of
        Parser.Tuple types -> if index >= 0 && index < length types then return $ types !! index else return Parser.Unknown
        _ -> return Parser.Unknown

getStructFields :: String -> StateT (CompilerState a) IO [(String, Parser.Type)]
getStructFields structName = do
    structDecs' <- gets structDecs
    let structDec = fromJust $ find (\x -> Parser.name x == structName) structDecs'
    let fields = case structDec of
            Parser.Struct{fields = fields'} -> fields'
            _ -> error "Not a struct"
    return fields

constructFQName :: String -> [Parser.Expr] -> StateT (CompilerState a) IO String
constructFQName "main" _ = return "main"
constructFQName funcName args = mapM (typeOf >=> return . show) args <&> \x -> funcName ++ ":" ++ intercalate "," x

constructFQName' :: String -> [Parser.Type] -> StateT (CompilerState a) IO String
constructFQName' "main" _ = return "main"
constructFQName' funcName args = return $ funcName ++ ":" ++ intercalate "," (map show args)

implsFor :: String -> StateT (CompilerState a) IO [String]
implsFor structName = do
    impls' <- gets impls
    let impls'' = filter (\x -> structNameFromType (Parser.for x) == structName) impls'
    return $ map Parser.trait impls''

methodsForTrait :: String -> StateT (CompilerState a) IO [String]
methodsForTrait traitName = do
    traits' <- gets traits
    case find (\x -> Parser.name x == traitName) traits' of
        Just trait -> return $ map Parser.name $ Parser.methods trait
        Nothing -> return []

methodsForStruct :: String -> StateT (CompilerState a) IO [String]
methodsForStruct structName = do
    impls' <- gets impls
    let impls'' = filter (\x -> structNameFromType (Parser.for x) == structName) impls'
    let methods = concatMap Parser.methods impls''
    return $ map Parser.name methods

findBaseDecInTraits :: String -> StateT (CompilerState a) IO (Maybe Parser.Expr)
findBaseDecInTraits funcName = do
    traits' <- gets traits
    let baseDecs = map (find (\y -> Parser.name y == funcName) . Parser.methods) traits'
    return $ firstJust id baseDecs

typeToData :: Parser.Type -> VM.Data
typeToData (Parser.StructT "Int" []) = VM.DInt 0
typeToData (Parser.StructT "Float" []) = VM.DFloat 0
typeToData (Parser.StructT "Double" []) = VM.DDouble 0
typeToData (Parser.StructT "Bool" []) = VM.DBool False
typeToData (Parser.StructT "String" []) = VM.DString ""
typeToData (Parser.StructT "IO" []) = VM.DNone -- Hmmm...
typeToData (Parser.StructT "Char" []) = VM.DChar ' '
typeToData (Parser.StructT "CPtr" []) = VM.DCPtr 0
typeToData (Parser.StructT _ _) = VM.DMap Data.Map.empty
typeToData Parser.Any = VM.DNone
typeToData x = error $ "Cannot convert type " ++ show x ++ " to data"

typeToString :: Parser.Type -> String
typeToString (Parser.StructT x typeArgs)
    | null typeArgs = x
    | otherwise = x ++ "<" ++ intercalate ", " (map typeToString typeArgs) ++ ">"
typeToString x = show x

structNameFromType :: Parser.Type -> String
structNameFromType (Parser.StructT name _) = name
structNameFromType _ = error "structNameFromType: expected StructT"

validateTypeParameters :: [String] -> Parser.Type -> Parser.Position -> String -> StateT (CompilerState a) IO ()
validateTypeParameters validGenericNames typeArg pos context = do
    structDecs' <- gets structDecs
    traits' <- gets traits
    let knownStructNames = map (\case Parser.Struct{name = n} -> n; _ -> "") structDecs'
    let knownTraitNames = map (\case Parser.Trait{name = n} -> n; _ -> "") traits'
    let knownTypeNames = knownStructNames ++ knownTraitNames
    let builtInTypes = ["Int", "Float", "Double", "Bool", "String", "Char", "CPtr", "IO", "None"]
    let validateTypeArg typeArg' = case typeArg' of
            Parser.Any -> True
            Parser.Unknown -> True
            Parser.None -> True
            Parser.Self -> True
            Parser.StructT argName [] -> argName `elem` validGenericNames || argName `elem` builtInTypes || argName `elem` knownTypeNames
            Parser.StructT _ nestedArgs -> all validateTypeArg nestedArgs
            Parser.List nested -> validateTypeArg nested
            Parser.Tuple nested -> all validateTypeArg nested
            Parser.Fn args ret -> all validateTypeArg (args ++ [ret])
    unless (validateTypeArg typeArg) $ do
        let typeArgStr = case typeArg of
                Parser.StructT name' [] -> name'
                _ -> typeToString typeArg
        cerror ("Invalid type parameter '" ++ typeArgStr ++ "' in " ++ context ++ ". Type parameters must be either concrete types or match valid generic parameters: " ++ show validGenericNames) pos

typesEqual :: Parser.Type -> Parser.Type -> Bool
typesEqual (Parser.StructT x xArgs) (Parser.StructT y yArgs) = x == y && xArgs == yArgs
typesEqual x y = x == y

extractPatternVars :: Parser.Expr -> [String]
extractPatternVars (Parser.Var{varName}) = [varName]
extractPatternVars (Parser.ListLit{listLitExprs}) = concatMap extractPatternVars listLitExprs
extractPatternVars (Parser.ListPattern{listPatternExprs}) = concatMap extractPatternVars listPatternExprs
extractPatternVars (Parser.StructLit{structLitFields}) = concatMap (\(_, e) -> extractPatternVars e) structLitFields
extractPatternVars (Parser.TupleLit{tupleLitExprs}) = concatMap extractPatternVars tupleLitExprs
extractPatternVars _ = []

compilePatternMatch :: Parser.Expr -> String -> Parser.Type -> StateT (CompilerState a) IO [Instruction]
compilePatternMatch (Parser.Var{varName}) _nextLabel _ = return [LStore varName]
compilePatternMatch (Parser.IntLit{intValue = x}) nextLabel _ = do
    return [Dup, Push $ DInt $ fromIntegral x, Eq, Jf nextLabel, Pop]
compilePatternMatch (Parser.BoolLit{boolValue = b}) nextLabel _ = do
    return [Dup, Push $ DBool b, Eq, Jf nextLabel, Pop]
compilePatternMatch (Parser.StringLit{stringValue = s}) nextLabel _ = do
    return [Dup, Push $ DString s, Eq, Jf nextLabel, Pop]
compilePatternMatch (Parser.CharLit{charValue = c}) nextLabel _ = do
    return [Dup, Push $ DChar c, Eq, Jf nextLabel, Pop]
compilePatternMatch lex@(Parser.ListLit{listLitExprs}) nextLabel expectedType = do
    if null listLitExprs
        then return [Dup, Push $ DList [], Eq, Jf nextLabel, Pop]
        else do
            -- Check if all elements are variables (for binding) or if we need to match literals
            let allVars = all (\case Parser.Var{} -> True; _ -> False) listLitExprs
            if allVars
                then do
                    -- Pattern like [x, y] - bind variables
                    let lengthCheck = [Dup, Length, Push $ DInt $ fromIntegral $ length listLitExprs, Eq, Jf nextLabel]
                    let bindings =
                            concatMap
                                ( \case
                                    (Parser.Var{varName}, index) ->
                                        [Dup, Push $ DInt index, Index, LStore varName]
                                    _ -> error "Expected Var in list pattern"
                                )
                                (zip listLitExprs [0 ..])
                    return $ lengthCheck ++ bindings ++ [Pop]
                else do
                    -- Pattern like [1, 2] - match literals
                    lex' <- compileExpr lex expectedType
                    return $ lex' ++ [Eq, Jf nextLabel]
compilePatternMatch (Parser.ListPattern{listPatternExprs}) nextLabel expectedType = do
    let lengthCheck = [Dup, Length, Push $ DInt $ fromIntegral $ length listPatternExprs - 1, Lt, StackLength, Push $ DInt 1, Neq, And, Jt nextLabel]
    case last listPatternExprs of
        Parser.ListLit{listLitExprs} -> do
            elements' <- mapM (\p -> compilePatternMatch p nextLabel expectedType) (init listPatternExprs)
            let paramsWithIndex = zip elements' [0 ..]
            let xToY = map (\(x, index) -> [Dup, Push $ DInt index, Index] ++ x) paramsWithIndex
            l' <- compileExpr (Parser.ListLit{listLitExprs = listLitExprs, listLitPos = anyPosition}) expectedType
            let listThing = [Comment "List thing", Dup, Push $ DInt (length listPatternExprs - 1), Push DNone, Slice] ++ l' ++ [Eq, Jf nextLabel]
            return $ lengthCheck ++ concat xToY ++ listThing
        _ -> do
            elements' <- mapM (\p -> compilePatternMatch p nextLabel expectedType) listPatternExprs
            let paramsWithIndex = zip elements' [0 ..]
            let xToY = map (\(x, index) -> [Dup, Push $ DInt index, Index] ++ x) paramsWithIndex
            let rest = [Push $ DInt (length listPatternExprs - 1), Push DNone, Slice] ++ last elements'
            return $ lengthCheck ++ concat xToY ++ rest
compilePatternMatch (Parser.TupleLit{tupleLitExprs}) nextLabel expectedType = do
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
                                nestedInstrs <- compilePatternMatch pat nestedLabel expectedType
                                let nestedCleanup = [Jmp nestedSuccessLabel, Label nestedLabel, Pop, LLoad tempVarName, Jmp nextLabel, Label nestedSuccessLabel]
                                return $ [LLoad tempVarName, Dup, Push $ DInt index, Index] ++ nestedInstrs ++ nestedCleanup ++ [LLoad tempVarName]
                            _ -> do
                                let cleanupLabel = "__tuple_cleanup_" ++ nextLabel ++ "_" ++ show index
                                let successLabel = "__tuple_success_" ++ nextLabel ++ "_" ++ show index
                                literalInstrs <- compilePatternMatch pat cleanupLabel expectedType
                                let afterMatch = [Jmp successLabel, Label cleanupLabel, Pop, LLoad tempVarName, Jmp nextLabel, Label successLabel, Pop]
                                return $ [LLoad tempVarName, Dup, Push $ DInt index, Index] ++ literalInstrs ++ afterMatch
                    )
                    (zip tupleLitExprs [0 ..])
            return $ lengthCheck ++ concat bindingsList ++ [Pop]
compilePatternMatch (Parser.StructLit{structLitName, structLitFields}) nextLabel expectedType = do
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
                b <- compileExpr x expectedType
                return $ a ++ b ++ [Eq, Jf nextLabel]
            )
            fields''
    return $ [Dup, Push $ DTypeQuery structLitName, TypeEq, Jf nextLabel] ++ fieldMappings ++ ([Pop | null fieldChecks]) ++ fieldChecks
compilePatternMatch (Parser.Placeholder _) _ _ = return []
compilePatternMatch x nextLabel expectedType = do
    x' <- compileExpr x expectedType
    return $ x' ++ [Eq, Jf nextLabel]

substituteGenerics :: Parser.Type -> [(String, Parser.Type)] -> Parser.Type
substituteGenerics (Parser.StructT name typeArgs) substitutions =
    case lookup name substitutions of
        Just subType -> subType
        Nothing -> Parser.StructT name (map (`substituteGenerics` substitutions) typeArgs)
substituteGenerics (Parser.List t) substitutions = Parser.List (substituteGenerics t substitutions)
substituteGenerics (Parser.Tuple ts) substitutions = Parser.Tuple (map (`substituteGenerics` substitutions) ts)
substituteGenerics (Parser.Fn args ret) substitutions = Parser.Fn (map (`substituteGenerics` substitutions) args) (substituteGenerics ret substitutions)
substituteGenerics t _ = t

compileExpr :: Parser.Expr -> Parser.Type -> StateT (CompilerState a) IO [Instruction]
compileExpr (Parser.Add{addLhs = x, addRhs = y}) expectedType = compileBinOp "+" x y expectedType Add
compileExpr (Parser.Sub{subLhs = x, subRhs = y}) expectedType = compileBinOp "-" x y expectedType Sub
compileExpr (Parser.Mul{mulLhs = x, mulRhs = y}) expectedType = compileBinOp "*" x y expectedType Mul
compileExpr (Parser.Div{divLhs = x, divRhs = y}) expectedType = compileBinOp "/" x y expectedType Div
compileExpr (Parser.Modulo{moduloLhs = x, moduloRhs = y}) expectedType = compileBinOp "%" x y expectedType Mod
compileExpr (Parser.Power{powerBase = x, powerExponent = y}) expectedType = compileBinOp "^" x y expectedType Pow
compileExpr (Parser.Gt{gtLhs = x, gtRhs = y}) expectedType = compileBinOp ">" x y expectedType Gt
compileExpr (Parser.Lt{ltLhs = x, ltRhs = y}) expectedType = compileBinOp "<" x y expectedType Lt
compileExpr (Parser.Ge{geLhs = x, geRhs = y}) expectedType = compileBinOpSeq ">=" x y expectedType [Lt, Not]
compileExpr (Parser.Le{leLhs = x, leRhs = y}) expectedType = compileBinOpSeq "<=" x y expectedType [Gt, Not]
compileExpr (Parser.Not{notExpr = x}) expectedType = compileExpr x expectedType >>= \x' -> return (x' ++ [Not])
compileExpr (Parser.Eq{eqLhs = x, eqRhs = y}) expectedType = compileBinOp "==" x y expectedType Eq
compileExpr (Parser.Neq{neqLhs = x, neqRhs = y}) expectedType = compileBinOp "!=" x y expectedType Neq
compileExpr (Parser.And{andLhs = x, andRhs = y}) expectedType = compileBinOp "&&" x y expectedType And
compileExpr (Parser.Or{orLhs = x, orRhs = y}) expectedType = compileBinOp "||" x y expectedType Or
compileExpr (Parser.IntLit{intValue = x}) _ = return [Push $ DInt $ fromIntegral x]
compileExpr (Parser.UnaryMinus{unaryMinusExpr = Parser.FloatLit{floatValue = x}}) _ = return [Push $ DFloat (-x)]
compileExpr (Parser.UnaryMinus{unaryMinusExpr = Parser.IntLit{intValue = x}}) _ = return [Push $ DInt $ -fromInteger x]
compileExpr (Parser.StringLit{stringValue = x}) _ = return [Push $ DString x]
compileExpr (Parser.DoBlock{doBlockExprs = exprs}) expectedType = do
    letsBefore <- gets lets
    curCon <- gets currentContext
    let inScopeLets = filter (\l -> l.context == curCon || l.context == "__outside") letsBefore
    let existingVars = map (\l -> l.name) inScopeLets
    let shadowedVars = filter (`elem` existingVars) $ map (\case Parser.Let{letName} -> letName; _ -> "") $ filter (\case Parser.Let{} -> True; _ -> False) exprs
    saveInstrs <-
        mapM
            ( \varName -> do
                let tempName = "__shadow_" ++ varName
                return [LLoad varName, LStore tempName]
            )
            shadowedVars
    let grouped = groupBy (\a b -> isFuncCall a && isFuncCall b) exprs
    let nestedSequence = concatMap (\case [] -> []; [x] -> if isFuncCall x then [Parser.FuncCall{funcName = "sequence", funcArgs = [x, Parser.FuncCall{funcName = "nop", funcArgs = [], funcPos = anyPosition}], funcPos = anyPosition}] else [x]; xs -> if all isFuncCall xs then [foldl1 (\a b -> Parser.FuncCall{funcName = "sequence", funcArgs = [a, b], funcPos = anyPosition}) xs] else xs) grouped
    bodyInstrs <- if length (filter isFuncCall exprs) == 1 then concatMapM (`compileExpr` expectedType) exprs else concatMapM (`compileExpr` expectedType) nestedSequence
    let restoreInstrs = concatMap (\varName -> [LLoad ("__shadow_" ++ varName), LStore varName]) shadowedVars
    return $ concat saveInstrs ++ bodyInstrs ++ restoreInstrs
  where
    isFuncCall :: Parser.Expr -> Bool
    isFuncCall (Parser.FuncCall{funcName}) = funcName `notElem` internalFunctions
    isFuncCall _ = False
compileExpr (Parser.Placeholder _) _ = return []
compileExpr (Parser.FuncCall{funcName = "unsafeAdd", funcArgs = [x, y]}) expectedType = compileExpr x expectedType >>= \x' -> compileExpr y expectedType >>= \y' -> return (x' ++ y' ++ [Add])
compileExpr (Parser.FuncCall{funcName = "unsafeSub", funcArgs = [x, y]}) expectedType = compileExpr x expectedType >>= \x' -> compileExpr y expectedType >>= \y' -> return (x' ++ y' ++ [Sub])
compileExpr (Parser.FuncCall{funcName = "unsafeMul", funcArgs = [x, y]}) expectedType = compileExpr x expectedType >>= \x' -> compileExpr y expectedType >>= \y' -> return (x' ++ y' ++ [Mul])
compileExpr (Parser.FuncCall{funcName = "unsafeDiv", funcArgs = [x, y]}) expectedType = compileExpr x expectedType >>= \x' -> compileExpr y expectedType >>= \y' -> return (x' ++ y' ++ [Div])
compileExpr (Parser.FuncCall{funcName = "unsafeMod", funcArgs = [x, y]}) expectedType = compileExpr x expectedType >>= \x' -> compileExpr y expectedType >>= \y' -> return (x' ++ y' ++ [Mod])
compileExpr (Parser.FuncCall{funcName = "unsafePow", funcArgs = [x, y]}) expectedType = compileExpr x expectedType >>= \x' -> compileExpr y expectedType >>= \y' -> return (x' ++ y' ++ [Pow])
compileExpr (Parser.FuncCall{funcName = "unsafeGt", funcArgs = [x, y]}) expectedType = compileExpr x expectedType >>= \x' -> compileExpr y expectedType >>= \y' -> return (x' ++ y' ++ [Gt])
compileExpr (Parser.FuncCall{funcName = "unsafeLt", funcArgs = [x, y]}) expectedType = compileExpr x expectedType >>= \x' -> compileExpr y expectedType >>= \y' -> return (x' ++ y' ++ [Lt])
compileExpr (Parser.FuncCall{funcName = "unsafeGe", funcArgs = [x, y]}) expectedType = compileExpr x expectedType >>= \x' -> compileExpr y expectedType >>= \y' -> return (x' ++ y' ++ [Lt, Not])
compileExpr (Parser.FuncCall{funcName = "unsafeLe", funcArgs = [x, y]}) expectedType = compileExpr x expectedType >>= \x' -> compileExpr y expectedType >>= \y' -> return (x' ++ y' ++ [Gt, Not])
compileExpr (Parser.FuncCall{funcName = "unsafeEq", funcArgs = [x, y]}) expectedType = compileExpr x expectedType >>= \x' -> compileExpr y expectedType >>= \y' -> return (x' ++ y' ++ [Eq])
compileExpr (Parser.FuncCall{funcName = "unsafeNeq", funcArgs = [x, y]}) expectedType = compileExpr x expectedType >>= \x' -> compileExpr y expectedType >>= \y' -> return (x' ++ y' ++ [Eq, Not])
compileExpr (Parser.FuncCall{funcName = "unsafeAnd", funcArgs = [x, y]}) expectedType = compileExpr x expectedType >>= \x' -> compileExpr y expectedType >>= \y' -> return (x' ++ y' ++ [And])
compileExpr (Parser.FuncCall{funcName = "unsafeOr", funcArgs = [x, y]}) expectedType = compileExpr x expectedType >>= \x' -> compileExpr y expectedType >>= \y' -> return (x' ++ y' ++ [Or])
compileExpr (Parser.FuncCall{funcName = "unsafePrint", funcArgs = [x]}) expectedType = compileExpr x expectedType >>= \x' -> return (x' ++ [Builtin Print])
compileExpr (Parser.FuncCall{funcName = "unsafeGetLine"}) _ = return [Builtin GetLine]
compileExpr (Parser.FuncCall{funcName = "unsafeGetChar"}) _ = return [Builtin GetChar]
compileExpr (Parser.FuncCall{funcName = "unsafeRandom"}) _ = return [Builtin Random]
#ifdef POSIX_IO
compileExpr (Parser.FuncCall{funcName = "unsafeOpenFile", funcArgs = [path, mode]}) expectedType = compileExpr path expectedType >>= \path' -> compileExpr mode expectedType >>= \mode' -> return (path' ++ mode' ++ [Builtin OpenFile])
compileExpr (Parser.FuncCall{funcName = "unsafeReadFile", funcArgs = [fd, size]}) expectedType = compileExpr size expectedType >>= \size' -> compileExpr fd expectedType >>= \fd' -> return (size' ++ fd' ++ [Builtin ReadFile])
compileExpr (Parser.FuncCall{funcName = "unsafeWriteFile", funcArgs = [fd, content]}) expectedType = compileExpr content expectedType >>= \content' -> compileExpr fd expectedType >>= \fd' -> return (content' ++ fd' ++ [Builtin WriteFile])
compileExpr (Parser.FuncCall{funcName = "unsafeCloseFile", funcArgs = [fd]}) expectedType = compileExpr fd expectedType >>= \fd' -> return (fd' ++ [Builtin CloseFile])
compileExpr (Parser.FuncCall{funcName = "unsafeSocket", funcArgs = [domain, socktype, protocol]}) expectedType = compileExpr protocol expectedType >>= \protocol' -> compileExpr socktype expectedType >>= \socktype' -> compileExpr domain expectedType >>= \domain' -> return (protocol' ++ socktype' ++ domain' ++ [Builtin Socket])
compileExpr (Parser.FuncCall{funcName = "unsafeBind", funcArgs = [fd, host, port]}) expectedType = compileExpr port expectedType >>= \port' -> compileExpr host expectedType >>= \host' -> compileExpr fd expectedType >>= \fd' -> return (port' ++ host' ++ fd' ++ [Builtin Bind])
compileExpr (Parser.FuncCall{funcName = "unsafeListen", funcArgs = [fd, backlog]}) expectedType = compileExpr backlog expectedType >>= \backlog' -> compileExpr fd expectedType >>= \fd' -> return (backlog' ++ fd' ++ [Builtin Listen])
compileExpr (Parser.FuncCall{funcName = "unsafeAccept", funcArgs = [fd]}) expectedType = compileExpr fd expectedType >>= \fd' -> return (fd' ++ [Builtin Accept])
compileExpr (Parser.FuncCall{funcName = "unsafeConnect", funcArgs = [fd, host, port]}) expectedType = compileExpr port expectedType >>= \port' -> compileExpr host expectedType >>= \host' -> compileExpr fd expectedType >>= \fd' -> return (port' ++ host' ++ fd' ++ [Builtin Connect])
compileExpr (Parser.FuncCall{funcName = "unsafeSend", funcArgs = [fd, data', flags]}) expectedType = compileExpr flags expectedType >>= \flags' -> compileExpr data' expectedType >>= \data'' -> compileExpr fd expectedType >>= \fd' -> return (flags' ++ data'' ++ fd' ++ [Builtin Send])
compileExpr (Parser.FuncCall{funcName = "unsafeRecv", funcArgs = [fd, size, flags]}) expectedType = compileExpr flags expectedType >>= \flags' -> compileExpr size expectedType >>= \size' -> compileExpr fd expectedType >>= \fd' -> return (flags' ++ size' ++ fd' ++ [Builtin Recv])
compileExpr (Parser.FuncCall{funcName = "unsafeCloseSocket", funcArgs = [fd]}) expectedType = compileExpr fd expectedType >>= \fd' -> return (fd' ++ [Builtin CloseSocket])
#endif
compileExpr (Parser.FuncCall{funcName = "unsafeListAdd", funcArgs = [y, x]}) expectedType = compileExpr x expectedType >>= \x' -> compileExpr y expectedType >>= \y' -> return (x' ++ y' ++ [ListAdd 2])
compileExpr (Parser.FuncCall{funcName = "unsafeListIndex", funcArgs = [x, y]}) expectedType = compileExpr x expectedType >>= \x' -> compileExpr y expectedType >>= \y' -> return (x' ++ y' ++ [Index])
compileExpr (Parser.FuncCall{funcName = "unsafeListLength", funcArgs = [x]}) expectedType = compileExpr x expectedType >>= \x' -> return (x' ++ [Length])
compileExpr (Parser.FuncCall{funcName = "unsafeListSlice", funcArgs = [x, y, z]}) expectedType = compileExpr x expectedType >>= \x' -> compileExpr y expectedType >>= \y' -> compileExpr z expectedType >>= \z' -> return (x' ++ y' ++ z' ++ [Slice])
compileExpr (Parser.FuncCall{funcName = "unsafeStructAccess", funcArgs = [x, y]}) expectedType = compileExpr x expectedType >>= \x' -> compileExpr y expectedType >>= \y' -> return (x' ++ y' ++ [AAccess])
compileExpr (Parser.FuncCall{funcName = "unsafeKeys", funcArgs = [x]}) expectedType = compileExpr x expectedType >>= \x' -> return (x' ++ [Keys])
compileExpr (Parser.FuncCall{funcName = "unsafeUpdate", funcArgs = [x, y, z]}) expectedType = compileExpr x expectedType >>= \x' -> compileExpr y expectedType >>= \y' -> compileExpr z expectedType >>= \z' -> return (x' ++ y' ++ z' ++ [Update])
compileExpr (Parser.FuncCall{funcName = "unsafeExit", funcArgs = [x]}) expectedType = compileExpr x expectedType >>= \x' -> return (x' ++ [Exit])
compileExpr (Parser.FuncCall{funcName = "abs", funcArgs = [x]}) expectedType = compileExpr x expectedType >>= \x' -> return (x' ++ [Abs])
compileExpr (Parser.FuncCall{funcName = "root", funcArgs = [x, Parser.FloatLit{floatValue = y}]}) expectedType = compileExpr x expectedType >>= \x' -> return (x' ++ [Push $ DFloat (1.0 / y), Pow])
compileExpr (Parser.FuncCall{funcName = "sqrt", funcArgs = [x]}) expectedType = compileExpr x expectedType >>= \x' -> return (x' ++ [Push $ DFloat 0.5, Pow])
compileExpr (Parser.FuncCall{funcName, funcArgs, funcPos}) expectedType = do
    implsForExpectedType <- implsFor (typeToString expectedType)
    functions' <- gets functions
    funcDecs' <- gets funcDecs
    funcDefs' <- gets funcDefs
    curCon <- gets currentContext
    externals' <- gets externals
    contextPath' <- gets contextPath
    fbt <- gets functionsByTrait

    -- Try to get expected argument types from function declarations for better lambda type inference
    let maybeExpectedArgTypes = case find (\x -> x.name == funcName) funcDecs' of
            Just fd -> Just $ init $ Parser.types fd
            Nothing -> Nothing

    -- Compute argument types, using expected types for lambdas when available
    argTypes <-
        zipWithM
            ( \arg idx -> case (arg, maybeExpectedArgTypes) of
                (Parser.Lambda{lambdaArgs, lambdaBody}, Just expectedTypes) | idx < length expectedTypes -> do
                    let expectedArgType = expectedTypes !! idx
                    returnType <- typeOf lambdaBody
                    case expectedArgType of
                        Parser.Fn expectedArgTypes expectedReturnType -> do
                            -- Use expected types for lambda arguments if available
                            let argTypes' =
                                    if length lambdaArgs == length expectedArgTypes
                                        then expectedArgTypes
                                        else replicate (length lambdaArgs) Parser.Any
                            return $ Parser.Fn argTypes' returnType
                        _ -> do
                            returnType <- typeOf lambdaBody
                            return $ Parser.Fn (replicate (length lambdaArgs) Parser.Any) returnType
                _ -> typeOf arg
            )
            funcArgs
            [0 ..]
    let contexts = map (T.pack . intercalate "@") (inits (Data.List.Split.splitOn "@" curCon))
    let contextFunctions = firstJust (\context -> findFunction (Data.Text.unpack context ++ "@" ++ funcName) functions' argTypes) contexts
    let implsForExpectedTypePrefixes = map (\x -> x ++ "." ++ typeToString expectedType ++ "::" ++ funcName) implsForExpectedType

    traitMethodMatch <-
        if not (null funcArgs) && not (null argTypes)
            then do
                let firstArgType = head argTypes
                let firstArgTypeStr = typeToString firstArgType
                implsForFirstArg <- implsFor firstArgTypeStr
                let implsForFirstArgPrefixes = map (\x -> x ++ "." ++ firstArgTypeStr ++ "::" ++ funcName) implsForFirstArg
                let traitFunc = find (\(Function{baseName}) -> baseName `elem` implsForFirstArgPrefixes) functions'
                case traitFunc of
                    Just tf -> return $ Just tf
                    Nothing -> do
                        let traitMethod = find (\(for, n, _, newDec) -> for == firstArgTypeStr && n == funcName && length funcArgs <= length (Parser.types newDec) - 1) fbt
                        case traitMethod of
                            Just (_, _, fqn, newDec) -> do
                                let traitFuncFound = find (\(Function{baseName = bn}) -> bn == fqn) functions'
                                case traitFuncFound of
                                    Just tf -> return $ Just tf
                                    Nothing ->
                                        case findFunction funcName functions' argTypes of
                                            Just vf -> return $ Just vf
                                            Nothing -> return Nothing
                            Nothing -> return Nothing
            else return Nothing

    let virtualFunction = findFunction funcName functions' argTypes
    let virtualFunctionAny = findAnyFunction funcName functions'

    let fun = case traitMethodMatch of
            Just _ ->
                case virtualFunctionAny of
                    Just vf -> vf
                    Nothing ->
                        fromMaybe
                            (Function{baseName = funcName, funame = funcName ++ "#0", function = [], types = [], context = "__outside"})
                            virtualFunction
            Nothing -> case virtualFunction of
                Just tm -> tm
                Nothing -> case virtualFunctionAny of
                    Just tm -> tm
                    Nothing -> case find (\(Function{baseName}) -> baseName `elem` implsForExpectedTypePrefixes) functions' of
                        Just funf -> funf
                        Nothing -> case contextFunctions of
                            (Just lf) -> lf
                            Nothing -> Function{baseName = unmangleFunctionName funcName, funame = funcName, function = [], types = [], context = "__outside"}
    -- traceShowM $ "Looking for function " ++ funcName ++ " in context " ++ curCon ++ " with types " ++ show argTypes ++ " and expected type " ++ show expectedType
    -- traceShowM fbt
    let traitMethodDec = case traitMethodMatch of
            Just _ ->
                let firstArgType = head argTypes
                    firstArgTypeStr = typeToString firstArgType
                    traitMethod = find (\(for, n, _, newDec) -> for == firstArgTypeStr && n == funcName && length funcArgs <= length (Parser.types newDec) - 1) fbt
                 in case traitMethod of
                        Just (_, _, fqn, newDec) -> Just Parser.FuncDec{Parser.name = fqn, Parser.types = Parser.types newDec, Parser.generics = [], funcDecPos = anyPosition}
                        Nothing -> Nothing
            Nothing -> Nothing

    let funcDec = case traitMethodDec of
            Just fd -> Just fd
            Nothing -> case find (\case Parser.FuncDec{name} -> name == baseName fun; _ -> False) funcDecs' of
                Just fd -> case find (\(_, n, _, newDec) -> n == funcName && take (length funcArgs) (Parser.types newDec) == argTypes) fbt of
                    Just (_, _, fqn, newDec) -> Just Parser.FuncDec{Parser.name = fqn, Parser.types = Parser.types newDec, Parser.generics = Parser.generics fd, funcDecPos = anyPosition}
                    Nothing -> Just fd
                Nothing -> Nothing
    let external = find (\x -> x.name == funcName) externals'

    let isLocal = T.pack (takeWhile (/= '@') curCon ++ "@") `isPrefixOf` T.pack fun.funame
    -- traceM $ "Local check: " ++ (takeWhile (/='@') curCon) ++ " " ++ fun.funame
    let callWay = (if isLocal then CallLocal (funame fun) else Call (funame fun))

    (argsOk, msgs) <- case funcDec of
        Just fd -> do
            let expectedArgTypes = init $ Parser.types fd
            let generics = Parser.generics fd
            if null generics
                then do
                    let zippedArgs = zip argTypes expectedArgTypes
                    wrongArgsBools <- mapM (uncurry typeCompatible) zippedArgs
                    let wrongArgs = [show t1 ++ " != " ++ show t2 | ((t1, t2), ok) <- zip zippedArgs wrongArgsBools, not ok]
                    let tooManyArgs = ["Too many arguments provided." | length argTypes > length expectedArgTypes]
                    return (null (wrongArgs ++ tooManyArgs), wrongArgs ++ tooManyArgs)
                else do
                    typesAcceptable <- functionTypesAcceptable argTypes expectedArgTypes generics
                    let tooManyArgs = ["Too many arguments provided." | length argTypes > length expectedArgTypes]
                    if typesAcceptable && null tooManyArgs
                        then return (True, [])
                        else do
                            let wrongArgs = (["Generic type constraints not satisfied" | not typesAcceptable])
                            return (False, wrongArgs ++ tooManyArgs)
        Nothing -> return (True, [])

    unless argsOk $ cerror ("Function " ++ funcName ++ " called with incompatible types: " ++ intercalate ", " msgs) funcPos

    let curConFuncDefs = reverse $ mapMaybe (\ctx -> find (\case Parser.FuncDef{name} -> name == ctx; _ -> False) funcDefs') contextPath'
        curConFuncDefParamsNames = concat [[varName | Parser.Var{varName} <- args] | Parser.FuncDef _ args _ _ <- curConFuncDefs]
        inContext = funcName `elem` curConFuncDefParamsNames

    unless
        ( inContext
            || isJust external
            || isJust funcDec
        )
        $ cerror ("Function " ++ funcName ++ " not found.") funcPos

    case external of
        Just (External _ ereturnType _ from) -> do
            retT <- case typeToData ereturnType of
                DMap _ -> do
                    fields <- getStructFields from
                    return $ DMap $ Data.Map.fromList $ map (second typeToData) fields
                _ -> return $ typeToData ereturnType
            args' <- concatMapM (`compileExpr` Parser.Any) (reverse funcArgs)
            return $ [Push retT] ++ args' ++ [CallFFI funcName from (length funcArgs)]
        Nothing ->
            case funcDec of
                (Just fd) -> do
                    if length funcArgs == length (Parser.types fd) - 1
                        then concatMapM (uncurry compileExpr) (zip funcArgs fd.types) >>= \args' -> return (args' ++ [callWay])
                        else
                            concatMapM (uncurry compileExpr) (zip funcArgs fd.types) >>= \args' ->
                                return $
                                    args'
                                        ++ [PushPf (funame fun) (length args')]
                Nothing -> do
                    -- traceM $ "Looking for funcDec " ++ funcName
                    -- gets funcDecs >>= \x -> traceM $ "\t" ++ show (map Parser.name x)
                    concatMapM (\arg -> typeOf arg >>= compileExpr arg) funcArgs >>= \args' ->
                        return $
                            args'
                                ++ [ LLoad funcName
                                   , CallS
                                   ]
compileExpr fd@(Parser.FuncDec{name, types, generics, funcDecPos}) _ = do
    let funcGenericNames = map (\(Parser.GenericExpr n _) -> n) generics
    forM_ types $ \type' -> do
        validateTypeParameters funcGenericNames type' funcDecPos ("function declaration " ++ name)
    modify (\s -> s{funcDecs = fd : funcDecs s})
    return [] -- Function declarations are only used for compilation
compileExpr fd@(Parser.FuncDef{name = origName, args, body}) expectedType = do
    curCon <- gets currentContext
    funs <- gets functions
    let previousContext = curCon
    let previousContextPath = if curCon == "__outside" then ["__outside"] else Data.List.Split.splitOn "@" curCon
    let name =
            if "." `isInfixOf` origName || curCon == "__outside"
                then origName
                else curCon ++ "@" ++ origName
    let origName' = name
    let isFirst = isNothing $ find (\x -> x.baseName == origName') funs
    -- when (not isFirst) $ traceM $ "Function " ++ name ++ " already exists"
    -- let argTypes = map Parser.typeOf args
    let newContextPath = if curCon == "__outside" then [origName] else previousContextPath ++ [origName]
    modify (\s -> s{currentContext = name, contextPath = newContextPath})
    funcDecs' <- gets funcDecs
    when (isNothing $ find (\case Parser.FuncDec{name = name'} -> name' == name; _ -> False) funcDecs') $ modify (\s -> s{funcDecs = Parser.FuncDec name (replicate (length args + 1) expectedType) [] anyPosition : funcDecs s})

    funame <- if name /= "main" then ((name ++ "#") ++) . show <$> allocId else return "main"
    modify (\s -> s{functions = Function name funame [] [] curCon : functions s})
    modify (\s -> s{funcDefs = fd : funcDefs s})
    funcDecs'' <- gets funcDecs

    let funcDec = fromJust $ find (\case Parser.FuncDec{name = name'} -> name' == name; _ -> False) funcDecs''

    body' <- compileExpr body (last $ Parser.types funcDec)

    let expectedReturnType = last $ Parser.types funcDec
    let argTypes = init $ Parser.types funcDec
    let argsWithTypes = zip args argTypes
    let paramBindings = concatMap extractParamBindings argsWithTypes
          where
            extractParamBindings (Parser.Var{varName}, t) = [Let{name = varName, vtype = t, context = name}]
            extractParamBindings _ = []
    letsBefore <- gets lets
    modify (\s -> s{lets = paramBindings ++ lets s})
    bodyType <- safeTypeOf body
    modify (\s -> s{lets = letsBefore})
    let generics = Parser.generics funcDec
    let genericNames = map (\(Parser.GenericExpr n _) -> n) generics
    let genericReturnInfo = case expectedReturnType of
            Parser.StructT t []
                | t `elem` genericNames ->
                    find (\(Parser.GenericExpr n _) -> n == t) generics
            _ -> Nothing
    let shouldCheck =
            expectedReturnType /= Parser.Any
                && expectedReturnType /= Parser.Unknown
                && bodyType /= Parser.Unknown
                && bodyType /= Parser.Any
    when shouldCheck $ case genericReturnInfo of
        Just (Parser.GenericExpr genericName (Just constraint)) -> do
            let constraintName = typeToString constraint
            let bodyTypeName = typeToString bodyType
            let isGenericItself = bodyTypeName == genericName
            bodyImpls <- implsFor bodyTypeName
            let satisfiesConstraint = isGenericItself || constraintName `elem` bodyImpls || bodyTypeName == constraintName
            unless satisfiesConstraint $
                cerror ("Return type mismatch in function `" ++ origName ++ "`: " ++ show bodyType ++ " does not implement " ++ constraintName) (extractPosition body)
        Just (Parser.GenericExpr _ Nothing) -> return ()
        Nothing -> do
            compatible <- typeCompatible bodyType expectedReturnType
            unless compatible $
                cerror ("Return type mismatch in function `" ++ origName ++ "`: expected " ++ show expectedReturnType ++ ", got " ++ show bodyType) (extractPosition body)

    nextFunId <- gets lastLabel
    args' <- concatMapM (`compileParameter` (name, funame, nextFunId)) (reverse (filter (\case Parser.Placeholder _ -> False; _ -> True) args))
    let function = Label funame : [StoreSideStack | isFirst] ++ [LoadSideStack | not isFirst] ++ args' ++ body' ++ [ClearSideStack] ++ ([Ret | name /= "main"])
    -- modify (\s -> s{functions = Function name funame function funcDec.types : tail (functions s)})
    modify (\s -> s{functions = Function name funame function funcDec.types curCon : functions s})
    let restoredContextPath = if previousContext == "__outside" then ["__outside"] else Data.List.Split.splitOn "@" previousContext
    modify (\s -> s{currentContext = previousContext, contextPath = restoredContextPath})
    return [] -- Function definitions get appended at the last stage of compilation
  where
    compileParameter :: Parser.Expr -> (String, String, Int) -> StateT (CompilerState a) IO [Instruction]
    compileParameter pat (funcName, _currentFuname, nextFunId) = do
        let nextFunName = funcName ++ "#" ++ show nextFunId
        compilePatternMatch pat nextFunName expectedType
compileExpr (Parser.ParenApply{parenApplyExpr, parenApplyArgs}) expectedType = do
    case parenApplyExpr of
        Parser.StructAccess{structAccessStruct = Parser.Var{varName = moduleName}, structAccessField = Parser.Var{varName = funcName}} -> do
            let qualifiedName = moduleName ++ "." ++ funcName
            compileExpr (Parser.FuncCall{funcName = qualifiedName, funcArgs = parenApplyArgs, funcPos = extractPosition parenApplyExpr}) expectedType
        _ -> do
            fun <- compileExpr parenApplyExpr expectedType
            args <- concatMapM (`compileExpr` expectedType) parenApplyArgs
            return $ args ++ fun ++ [CallS]
compileExpr (Parser.Var{varName, varPos}) expectedType = do
    functions' <- gets functions
    curCon <- gets currentContext
    externals' <- gets externals
    let fun =
            any ((== varName) . baseName) functions'
                || any ((\context -> any ((== context ++ "@" ++ varName) . baseName) functions') . intercalate "@") (inits (Data.List.Split.splitOn "@" curCon))
                || any ((== varName) . (Data.Text.unpack . last . splitOn "::") . fromString . baseName) functions'
    if fun || varName `elem` internalFunctions || varName `elem` map (\f -> f.name) externals'
        then (`compileExpr` expectedType) (Parser.FuncCall{funcName = varName, funcArgs = [], funcPos = varPos})
        else return [LLoad varName]
compileExpr (Parser.Let{letName, letValue}) _ = do
    curCon <- gets currentContext
    case letValue of
        Parser.Function{def, dec} -> do
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
            let fun = Parser.Function{def = renamedDef, dec = renamedDec, functionPos = Parser.anyPosition}
            compileExpr fun Parser.Any
        _ -> do
            typeOf letValue >>= \v -> modify (\s -> s{lets = Let{name = letName, vtype = v, context = curCon} : lets s})
            value' <- typeOf letValue >>= compileExpr letValue
            return $ value' ++ [LStore letName]
compileExpr (Parser.Function{def = [Parser.FuncDef{body = Parser.StrictEval{strictEvalExpr = e}}], dec}) expectedType = do
    let name = dec.name
    evaledExpr <- compileExpr e expectedType
    return $ evaledExpr ++ [LStore name]
compileExpr (Parser.Function{def = a, dec = b@Parser.FuncDec{name = _name, types = _types}}) _ = do
    curCon <- gets currentContext
    let mangledName =
            if "." `isInfixOf` b.name || curCon == "__outside"
                then b.name
                else curCon ++ "@" ++ b.name
    let mangledDec = b{Parser.name = mangledName}
    compileExpr mangledDec (last b.types) >> mapM_ (`compileExpr` last b.types) a >> return []
compileExpr (Parser.StrictEval{strictEvalExpr = e}) expectedType = compileExpr e expectedType
compileExpr (Parser.Flexible{flexibleExpr = a}) expectedType = compileExpr a expectedType >>= \a' -> return $ Meta "flex" : a'
compileExpr (Parser.ListConcat{listConcatLhs = a, listConcatRhs = b}) expectedType = do
    a' <- compileExpr a expectedType
    b' <- compileExpr b expectedType
    let a'' = case a' of
            [Meta "flex", a'''] -> [a'''] ++ b' ++ [Cast]
            _ -> a'
    let b'' = case b' of
            [Meta "flex", b'''] -> [b'''] ++ a' ++ [Cast]
            _ -> b'
    return (b'' ++ a'' ++ [Concat 2])
compileExpr (Parser.ListAdd{listAddLhs = a, listAddRhs = b}) expectedType = do
    a' <- compileExpr a expectedType
    b' <- compileExpr b expectedType
    let a'' = case a' of
            [Meta "flex", a'''] -> [a'''] ++ b' ++ [Cast]
            _ -> a'
    let b'' = case b' of
            [Meta "flex", b'''] -> [b'''] ++ a' ++ [Cast]
            _ -> b'
    return (b'' ++ a'' ++ [ListAdd 2])
compileExpr (Parser.ListLit{listLitExprs}) expectedType = do
    elems <- concatMapM (`compileExpr` expectedType) listLitExprs
    return $ elems ++ [PackList $ length listLitExprs]
compileExpr (Parser.TupleLit{tupleLitExprs}) expectedType = do
    elems <- concatMapM (`compileExpr` expectedType) tupleLitExprs
    return $ elems ++ [PackList $ length tupleLitExprs]
compileExpr (Parser.TupleAccess{tupleAccessTuple, tupleAccessIndex}) expectedType = do
    tuple' <- compileExpr tupleAccessTuple expectedType
    return $ tuple' ++ [Push $ DInt tupleAccessIndex, Index]
compileExpr (Parser.When{whenExpr = expr, whenBranches = branches, whenElse = else_}) expectedType = do
    expr' <- compileExpr expr expectedType
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
                patternInstrs <- compilePatternMatch pat nextLabel expectedType
                bodyInstrs <- compileExpr body expectedType
                let restoreInstrs = concatMap (\varName -> [LLoad ("__shadow_" ++ varName), LStore varName]) varsToSave
                return (patternInstrs, bodyInstrs, restoreInstrs)
            )
            branchPairs

    elseInstrs <- case else_ of
        Just elseExpr -> compileExpr elseExpr expectedType
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
compileExpr (Parser.If{ifCond, ifThen, ifElse}) expectedType = do
    cond' <- compileExpr ifCond expectedType
    then' <- compileExpr ifThen expectedType
    else' <- compileExpr ifElse expectedType
    elseLabel <- allocId >>= \x -> return $ "else" ++ show x
    endLabel <- allocId >>= \x -> return $ "end" ++ show x
    return $ cond' ++ [Jf elseLabel] ++ then' ++ [Jmp endLabel, Label elseLabel] ++ else' ++ [Label endLabel]
compileExpr (Parser.FloatLit{floatValue}) _ = return [Push $ DFloat floatValue]
compileExpr (Parser.DoubleLit{doubleValue}) _ = return [Push $ DDouble doubleValue]
compileExpr (Parser.BoolLit{boolValue}) _ = return [Push $ DBool boolValue]
compileExpr (Parser.Cast{castExpr, castType, castPos}) expectedType = do
    structDecs' <- gets structDecs
    let targetStructName = case castType of
            Parser.Var{varName} -> Just varName
            _ -> Nothing
    let targetStruct = case targetStructName of
            Just name -> find (\case Parser.Struct{name = name'} -> name' == name; _ -> False) structDecs'
            Nothing -> Nothing
    case targetStruct of
        Just (Parser.Struct{isValueStruct = True, fields, refinement, refinementSrc}) -> do
            when (length fields /= 1) $ cerror "value struct must have exactly one field" castPos
            let (fieldName, fieldType) = head fields
            fromType <- typeOf castExpr
            if not (Parser.compareTypes fromType fieldType)
                then do
                    cerror ("Type mismatch: cannot cast " ++ show fromType ++ " to value struct field type " ++ show fieldType) castPos
                    return []
                else do
                    let structLit = Parser.StructLit{structLitName = fromMaybe "" targetStructName, structLitFields = [(fieldName, castExpr)], structLitTypeArgs = [], structLitPos = castPos}
                    skipCheck <- gets skipRefinementCheck
                    unless skipCheck $ do
                        case refinement of
                            Just rf -> do
                                r <- runRefinement rf structLit
                                case r of
                                    Just False -> cerror ("Refinement failed (" ++ refinementSrc ++ ")") castPos
                                    Just True -> return ()
                                    Nothing -> cerror ("Cannot verify refinement (" ++ refinementSrc ++ ") at compile time - value contains variables without refinement guarantees") castPos
                            Nothing -> return ()
                    modify (\s -> s{skipRefinementCheck = True})
                    result <- compileExpr structLit expectedType
                    modify (\s -> s{skipRefinementCheck = skipCheck})
                    return result
        _ -> do
            from' <- compileExpr castExpr Parser.Unknown
            let to' = compileType castType
            return $ from' ++ to' ++ [Cast]
  where
    compileType :: Parser.Expr -> [Instruction]
    compileType (Parser.Var{varName = "Int"}) = [Push $ DInt 0]
    compileType (Parser.Var{varName = "Float"}) = [Push $ DFloat 0.0]
    compileType (Parser.Var{varName = "Double"}) = [Push $ DDouble 0.0]
    compileType (Parser.Var{varName = "Bool"}) = [Push $ DBool False]
    compileType (Parser.Var{varName = "Char"}) = [Push $ DChar '\0']
    compileType (Parser.Var{varName = "String"}) = [Push $ DString ""]
    compileType (Parser.Var{varName = "CPtr"}) = [Push $ DCPtr $ ptrToWordPtr nullPtr]
    compileType (Parser.ListLit{listLitExprs = [x]}) = compileType x ++ [PackList 1]
    compileType (Parser.TupleLit{tupleLitExprs}) = concatMap compileType tupleLitExprs ++ [PackList $ length tupleLitExprs]
    compileType (Parser.Var{}) = [Push DNone]
    compileType x = error $ "Cannot cast to type " ++ show x
compileExpr st@(Parser.Struct{name = structName, fields, is, generics, structPos}) expectedType = do
    modify (\s -> s{structDecs = st : structDecs s})
    mapM_ createFieldTrait fields
    let structGenericNames = map (\(Parser.GenericExpr n _) -> n) generics
    mapM_
        ( \t -> do
            let (traitName, traitTypeArgs) = case t of
                    Parser.StructT name args -> (name, args)
                    _ -> error "Expected StructT in is clause"
            forM_ traitTypeArgs $ \typeArg -> do
                validateTypeParameters structGenericNames typeArg structPos ("trait " ++ traitName ++ " for struct " ++ structName)
            _ <- compileExpr (Parser.Impl{trait = traitName, traitTypeArgs = traitTypeArgs, for = Parser.StructT structName [], methods = [], implPos = anyPosition}) expectedType
            return ()
        )
        is
    return []
  where
    createFieldTrait :: (String, Parser.Type) -> StateT (CompilerState a) IO ()
    createFieldTrait (name, _) = do
        let traitName = "__field_" ++ name
        let trait = Parser.Trait{name = traitName, methods = [Parser.FuncDec{Parser.name = name, Parser.types = [Parser.Self, Parser.Any], Parser.generics = [], funcDecPos = anyPosition}], generics = [], traitPos = anyPosition}
        let impl = Parser.Impl{trait = traitName, traitTypeArgs = [], for = Parser.StructT (Parser.name st) [], methods = [Parser.FuncDef{name = name, args = [Parser.Var{varName = "self", varPos = zeroPosition}], body = Parser.StructAccess{structAccessStruct = Parser.Var{varName = "self", varPos = zeroPosition}, structAccessField = Parser.Var{varName = name, varPos = zeroPosition}, structAccessPos = anyPosition}, funcDefPos = anyPosition}], implPos = anyPosition}
        _ <- compileExpr trait Parser.Any
        _ <- compileExpr impl Parser.Any
        return ()
compileExpr sl@(Parser.StructLit{structLitName, structLitFields, structLitTypeArgs, structLitPos}) _ = do
    skipCheck <- gets skipRefinementCheck
    structDecs' <- gets structDecs
    let struct = find (\case Parser.Struct{name = name'} -> name' == structLitName; _ -> False) structDecs'
    case struct of
        Just expr -> case expr of
            Parser.Struct{fields = declaredFields, generics = structGenerics} -> do
                when (not (null structGenerics) && length structLitTypeArgs /= length structGenerics) $ do
                    cerror ("Type argument count mismatch for struct " ++ structLitName ++ ": expected " ++ show (length structGenerics) ++ ", got " ++ show (length structLitTypeArgs)) structLitPos
                let genericSubstitutions =
                        if null structGenerics || null structLitTypeArgs
                            then []
                            else zip (map (\(Parser.GenericExpr n _) -> n) structGenerics) structLitTypeArgs
                let structGenericNames = map (\(Parser.GenericExpr n _) -> n) structGenerics
                unless (null structLitTypeArgs) $ do
                    forM_ structLitTypeArgs $ \typeArg -> do
                        validateTypeParameters structGenericNames typeArg structLitPos ("struct literal " ++ structLitName)
                    forM_ (zip structGenerics structLitTypeArgs) $ \(Parser.GenericExpr genName genConstraint, typeArg) -> do
                        case genConstraint of
                            Just constraint -> do
                                constraintName <- case constraint of
                                    Parser.StructT traitName [] -> return traitName
                                    _ -> do
                                        cerror "Trait constraint must be a struct type" structLitPos
                                        return ""
                                impls <- implsFor (typeToString typeArg)
                                unless (constraintName `elem` impls || typeToString typeArg == constraintName) $ do
                                    cerror ("Type argument " ++ show typeArg ++ " does not satisfy trait constraint " ++ constraintName ++ " for generic parameter " ++ genName) structLitPos
                            Nothing -> return ()
                let declaredFieldMap = Data.Map.fromList declaredFields
                let substitutedFields =
                        if null genericSubstitutions
                            then declaredFields
                            else map (second (`substituteGenerics` genericSubstitutions)) declaredFields
                let substitutedFieldMap = Data.Map.fromList substitutedFields
                forM_ structLitFields $ \(fieldName, fieldValue) -> do
                    case Data.Map.lookup fieldName substitutedFieldMap of
                        Just declaredType -> do
                            actualType <- typeOf fieldValue
                            unless (Parser.compareTypes actualType declaredType) $ do
                                cerror ("Type mismatch for field '" ++ fieldName ++ "': expected " ++ show declaredType ++ ", got " ++ show actualType) structLitPos
                        Nothing -> do
                            cerror ("Unknown field '" ++ fieldName ++ "' in struct " ++ structLitName) structLitPos
                unless skipCheck $ do
                    case expr of
                        Parser.Struct{refinement, refinementSrc} -> do
                            case refinement of
                                Just rf -> do
                                    r <- runRefinement rf sl
                                    case r of
                                        Just False -> cerror ("Refinement failed (" ++ refinementSrc ++ ")") structLitPos
                                        Just True -> return ()
                                        Nothing -> cerror ("Cannot verify refinement (" ++ refinementSrc ++ ") at compile time - struct fields contain variables without refinement guarantees") structLitPos
                                Nothing -> return ()
            _ -> return ()
        Nothing -> return ()
    fields' <- mapM ((`compileExpr` Parser.Any) . snd) structLitFields
    let names = map (DString . fst) structLitFields
    let instructions = zip names fields' >>= \(name', field) -> field ++ [Push name']
    implsForStruct <- implsFor structLitName
    return $ instructions ++ [Push $ DString structLitName, Push $ DString "__name", Push $ DList (map DString implsForStruct), Push $ DString "__traits", PackMap $ length structLitFields * 2 + 4]
compileExpr (Parser.StructAccess{structAccessStruct, structAccessField}) expectedType = do
    struct' <- compileExpr structAccessStruct expectedType
    case structAccessField of
        Parser.Var{varName = field} -> return $ struct' ++ [Access field]
        _ -> do
            fieldExpr' <- compileExpr structAccessField expectedType
            return $ struct' ++ fieldExpr' ++ [AAccess]
compileExpr (Parser.Import{objects = o, from = from, as = as, qualified = qualified, importPos}) expectedType = do
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
                    (importedProg, _importedPath) <- case Map.lookup from modules' of
                        Just (prog, path) -> return (prog, path)
                        Nothing -> do
                            sourcePath' <- gets sourcePath
                            let sourcePath = takeDirectory sourcePath'
                            fileResult <- liftIO $ try (findSourceFile (from ++ ".in") [sourcePath] >>= readFile) :: StateT (CompilerState a) IO (Either SomeException String)
                            case fileResult of
                                Left _ -> do
                                    cerror ("Module '" ++ from ++ "' not found in module map and file not found") importPos
                                    return (Parser.Program [] Nothing, "")
                                Right i -> do
                                    case parseProgram (T.pack i) Parser.initCompilerFlags{Parser.needsMain = False} of
                                        Left err -> do
                                            cerror ("Parse error in imported module '" ++ from ++ "':\n" ++ show err) importPos
                                            return (Parser.Program [] Nothing, "")
                                        Right p -> return (p, from ++ ".in")
                    let (Parser.Program exprs _) = importedProg
                    let moduleNameToUse = fromMaybe from (Parser.moduleName importedProg)
                    let prefix =
                            if qualified || isJust as
                                then if qualified then moduleNameToUse else fromJust as
                                else ""
                    if null prefix
                        then concatMapM (`compileExpr` expectedType) exprs
                        else concatMapM (`compileExpr` expectedType) (map (`mangleExprForImport` prefix) exprs)
  where
    mangleExprForImport :: Parser.Expr -> String -> Parser.Expr
    mangleExprForImport (Parser.FuncDec{name, types, generics, funcDecPos}) prefix =
        Parser.FuncDec{name = prefix ++ "." ++ name, types = types, generics = generics, funcDecPos = funcDecPos}
    mangleExprForImport (Parser.Function{def = fdef, dec, functionPos}) prefix =
        Parser.Function{def = map (`mangleExprForImport` prefix) fdef, dec = mangleExprForImport dec prefix, functionPos = functionPos}
    mangleExprForImport (Parser.FuncDef{name, args, body, funcDefPos}) prefix =
        Parser.FuncDef{name = prefix ++ "." ++ name, args = args, body = mangleExprForImport body prefix, funcDefPos = funcDefPos}
    mangleExprForImport (Parser.Struct{name, fields, refinement, refinementSrc, is, isValueStruct, generics, structPos}) prefix =
        Parser.Struct{name = prefix ++ "." ++ name, fields = fields, refinement = fmap (`mangleExprForImport` prefix) refinement, refinementSrc = refinementSrc, is = is, isValueStruct = isValueStruct, generics = generics, structPos = structPos}
    mangleExprForImport (Parser.StructLit{structLitName, structLitFields, structLitTypeArgs, structLitPos}) prefix =
        Parser.StructLit{structLitName = prefix ++ "." ++ structLitName, structLitFields = map (second (`mangleExprForImport` prefix)) structLitFields, structLitTypeArgs = structLitTypeArgs, structLitPos = structLitPos}
    mangleExprForImport (Parser.Trait{name, methods, generics, traitPos}) prefix =
        Parser.Trait{name = prefix ++ "." ++ name, methods = map (`mangleExprForImport` prefix) methods, generics = generics, traitPos = traitPos}
    mangleExprForImport (Parser.Impl{trait, traitTypeArgs, for, methods, implPos}) prefix =
        Parser.Impl{trait = prefix ++ "." ++ trait, traitTypeArgs = traitTypeArgs, for = for, methods = map (`mangleExprForImport` prefix) methods, implPos = implPos}
    mangleExprForImport (Parser.FuncCall{funcName, funcArgs, funcPos}) prefix =
        Parser.FuncCall{funcName = prefix ++ "." ++ funcName, funcArgs = map (`mangleExprForImport` prefix) funcArgs, funcPos = funcPos}
    mangleExprForImport (Parser.Var{varName, varPos}) prefix =
        Parser.Var{varName = varName, varPos = varPos}
    mangleExprForImport (Parser.StructAccess{structAccessStruct, structAccessField, structAccessPos}) prefix =
        Parser.StructAccess{structAccessStruct = mangleExprForImport structAccessStruct prefix, structAccessField = mangleExprForImport structAccessField prefix, structAccessPos = structAccessPos}
    mangleExprForImport (Parser.Let{letName, letValue, letPos}) prefix =
        Parser.Let{letName = letName, letValue = mangleExprForImport letValue prefix, letPos = letPos}
    mangleExprForImport (Parser.If{ifCond, ifThen, ifElse, ifPos}) prefix =
        Parser.If{ifCond = mangleExprForImport ifCond prefix, ifThen = mangleExprForImport ifThen prefix, ifElse = mangleExprForImport ifElse prefix, ifPos = ifPos}
    mangleExprForImport (Parser.DoBlock{doBlockExprs, doBlockPos}) prefix =
        Parser.DoBlock{doBlockExprs = map (`mangleExprForImport` prefix) doBlockExprs, doBlockPos = doBlockPos}
    mangleExprForImport (Parser.When{whenExpr, whenBranches, whenElse, whenPos}) prefix =
        Parser.When{whenExpr = mangleExprForImport whenExpr prefix, whenBranches = map (\(p, b) -> (mangleExprForImport p prefix, mangleExprForImport b prefix)) whenBranches, whenElse = fmap (`mangleExprForImport` prefix) whenElse, whenPos = whenPos}
    mangleExprForImport (Parser.ListLit{listLitExprs, listLitPos}) prefix =
        Parser.ListLit{listLitExprs = map (`mangleExprForImport` prefix) listLitExprs, listLitPos = listLitPos}
    mangleExprForImport (Parser.TupleLit{tupleLitExprs, tupleLitPos}) prefix =
        Parser.TupleLit{tupleLitExprs = map (`mangleExprForImport` prefix) tupleLitExprs, tupleLitPos = tupleLitPos}
    mangleExprForImport (Parser.Lambda{lambdaArgs, lambdaBody, lambdaPos}) prefix =
        Parser.Lambda{lambdaArgs = lambdaArgs, lambdaBody = mangleExprForImport lambdaBody prefix, lambdaPos = lambdaPos}
    mangleExprForImport (Parser.External{externalName, externalArgs, externalPos}) prefix =
        Parser.External{externalName = prefix ++ "." ++ externalName, externalArgs = map (`mangleExprForImport` prefix) externalArgs, externalPos = externalPos}
    mangleExprForImport x _ = x
compileExpr (Parser.Trait{name, methods, generics}) expectedType = do
    let methods' = map (\case Parser.FuncDec{name = name', types} -> Parser.FuncDec{name = name', types = types, generics = [], funcDecPos = anyPosition}; _ -> error "Expected FuncDec in Trait methods") methods
    modify (\s -> s{traits = Parser.Trait{name = name, methods = methods, generics = generics, traitPos = anyPosition} : traits s})
    mapM_ (`compileExpr` expectedType) methods'
    return []
compileExpr impl@(Parser.Impl{trait, traitTypeArgs, for, methods, implPos}) expectedType = do
    let forStructName = structNameFromType for
    structDecs' <- gets structDecs
    let forStructGenerics = case find (\case Parser.Struct{name = name'} -> name' == forStructName; _ -> False) structDecs' of
            Just (Parser.Struct{generics = gs}) -> map (\(Parser.GenericExpr n _) -> n) gs
            _ -> []
    trait' <- gets traits >>= \traits' -> return $ fromJust $ find (\x -> Parser.name x == trait) traits'
    let traitGenerics = Parser.generics trait'
    when (not (null traitGenerics) && length traitTypeArgs /= length traitGenerics) $ do
        cerror ("Trait type argument count mismatch for impl " ++ trait ++ " for " ++ typeToString for ++ ": expected " ++ show (length traitGenerics) ++ ", got " ++ show (length traitTypeArgs)) implPos
    when (null traitGenerics && not (null traitTypeArgs)) $ do
        cerror ("Trait '" ++ trait ++ "' has no type parameters, but impl provides type arguments") implPos
    forM_ traitTypeArgs $ \typeArg -> do
        validateTypeParameters forStructGenerics typeArg implPos ("impl " ++ trait ++ " for " ++ typeToString for)
    let genericSubstitutions =
            if null traitGenerics || null traitTypeArgs
                then []
                else zip (map (\(Parser.GenericExpr n _) -> n) traitGenerics) traitTypeArgs
    methods' <-
        catMaybes
            <$> mapM
                ( \case
                    Parser.FuncDef{name = name', args, body, funcDefPos} -> do
                        let traitTypeArgsStr = if null traitTypeArgs then "" else "<" ++ intercalate "," (map typeToString traitTypeArgs) ++ ">"
                        let forTypeStr = typeToString for
                        let fullyQualifiedName = trait ++ traitTypeArgsStr ++ "." ++ forTypeStr ++ "::" ++ name'
                        let dec = find (\x -> Parser.name x == name') (Parser.methods trait')
                        case dec of
                            Nothing -> do
                                cerror ("Method '" ++ name' ++ "' is not declared in trait '" ++ trait ++ "'") funcDefPos
                                return Nothing
                            Just dec' -> do
                                let substitutedTypes =
                                        if null genericSubstitutions
                                            then Parser.types dec'
                                            else map (`substituteGenerics` genericSubstitutions) (Parser.types dec')
                                let newDec = Parser.FuncDec{name = fullyQualifiedName, types = unself substitutedTypes for, generics = Parser.generics dec', funcDecPos = anyPosition}
                                _ <- compileExpr newDec Parser.Any
                                modify (\s -> s{functionsByTrait = (forStructName, name', fullyQualifiedName, newDec) : functionsByTrait s})
                                return $ Just $ Parser.FuncDef{name = fullyQualifiedName, args = args, body = body, funcDefPos = anyPosition}
                    _ -> error "Expected FuncDef in Impl methods"
                )
                methods
    modify (\s -> s{impls = impl : impls s})
    mapM_ (`compileExpr` expectedType) methods'
    return []
  where
    unself :: [Parser.Type] -> Parser.Type -> [Parser.Type]
    unself types selfType = map (\case Parser.Self -> selfType; x -> x) types
compileExpr (Parser.Lambda{lambdaArgs, lambdaBody}) expectedType = do
    fId <- allocId
    curCon <- gets currentContext
    let args' = if lambdaArgs == [Parser.Placeholder anyPosition] then [] else lambdaArgs
    let name = "__lambda" ++ show fId
    let def = Parser.FuncDef{name = name, args = args', body = lambdaBody, funcDefPos = anyPosition}
    let dec = Parser.FuncDec{name = name, types = replicate (length args' + 1) Parser.Any, generics = [], funcDecPos = anyPosition}
    let fun = Parser.Function{def = [def], dec = dec, functionPos = anyPosition}
    _ <- compileExpr fun expectedType
    lets' <- gets functions
    let fullName = (fromJust $ findAnyFunction (curCon ++ "@" ++ name) lets').funame
    return [PushPf fullName 0]
compileExpr (Parser.Pipeline{pipelineLhs = a, pipelineRhs = Parser.Var{varName = b}}) expectedType = compileExpr (Parser.FuncCall{funcName = b, funcArgs = [a], funcPos = zeroPosition}) expectedType
compileExpr (Parser.Pipeline{pipelineLhs = a, pipelineRhs = Parser.FuncCall{funcName = f, funcArgs}}) expectedType = compileExpr (Parser.FuncCall{funcName = f, funcArgs = a : funcArgs, funcPos = zeroPosition}) expectedType
compileExpr (Parser.Pipeline{pipelineLhs = a, pipelineRhs = Parser.Then{thenLhs = b, thenRhs = c}}) expectedType = compileExpr (Parser.Then{thenLhs = Parser.Pipeline{pipelineLhs = a, pipelineRhs = b, pipelinePos = anyPosition}, thenRhs = c, thenPos = anyPosition}) expectedType
compileExpr (Parser.Then{thenLhs = a, thenRhs = b}) expectedType = do
    a' <- compileExpr a expectedType
    b' <- compileExpr b expectedType
    return $ a' ++ b'
compileExpr (Parser.UnaryMinus{unaryMinusExpr = x}) expectedType = compileExpr x expectedType >>= \x' -> return (x' ++ [Push $ DInt (-1), Mul])
compileExpr (Parser.External{externalArgs = []}) _ = return []
compileExpr (Parser.External{externalName = from, externalArgs = (Parser.FuncDec{name, types} : xs)}) expectedType = do
    modify (\s -> s{externals = External{name, returnType = last types, args = init types, from} : externals s})
    _ <- compileExpr (Parser.External{externalName = from, externalArgs = xs, externalPos = anyPosition}) expectedType
    return []
compileExpr (Parser.CharLit{charValue = x}) _ = return [Push $ DChar x]
compileExpr x _ = error $ show x ++ " is not implemented"

transformRefinementExpr :: Parser.Expr -> [(String, Parser.Type)] -> Parser.Expr
transformRefinementExpr expr fieldNames = case expr of
    Parser.Var{varName}
        | varName `elem` map fst fieldNames ->
            Parser.StructAccess
                (Parser.Var "it" Parser.zeroPosition)
                (Parser.Var varName Parser.zeroPosition)
                Parser.anyPosition
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
             in [Parser.Function{def = renamedDef, dec = renamedDec, functionPos = Parser.anyPosition}]
        _ -> extractFunctions [letValue]
    extractFunction (Parser.DoBlock{doBlockExprs}) = extractFunctions doBlockExprs
    extractFunction _ = []

runRefinement :: Parser.Expr -> Parser.Expr -> StateT (CompilerState a) IO (Maybe Bool)
runRefinement refinement value = do
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
                        then [Parser.FuncCall "__refinement" [value] Parser.anyPosition]
                        else neededLets ++ [Parser.FuncCall "__refinement" [value] Parser.anyPosition]
            let progExprs =
                    maybeToList structDef
                        ++ functionDefs
                        ++ [ Parser.FuncDec "__refinement" [Parser.Any, Parser.StructT "Bool" []] [] Parser.anyPosition
                           , Parser.FuncDef "__refinement" [Parser.Var "it" Parser.zeroPosition] transformedRefinement Parser.anyPosition
                           , Parser.FuncDec "main" [Parser.StructT "IO" []] [] Parser.anyPosition
                           , Parser.FuncDef
                                "main"
                                []
                                (Parser.DoBlock mainBody Parser.anyPosition)
                                Parser.anyPosition
                           ]
            let prog = Parser.Program progExprs Nothing
            let freshState = initCompilerState prog
                extractedFuncDecs = filter (\case Parser.FuncDec{} -> True; _ -> False) functionDefs
                stateWithContext =
                    freshState
                        { funcDecs = currentState.funcDecs ++ extractedFuncDecs ++ [Parser.FuncDec "__refinement" [Parser.Any, Parser.StructT "Bool" []] [] Parser.anyPosition]
                        , structDecs = currentState.structDecs
                        , skipRefinementCheck = True
                        }
            compiled <- liftIO $ evalStateT (compileProgram prog) stateWithContext
            case compiled of
                Right _ -> return $ Just False
                Left bytecode -> do
                    let mainPc = locateLabel bytecode "main"
                    let vm = (initVM (V.fromList bytecode)){pc = mainPc, callStack = [StackFrame{returnAddress = 0, locals = []}, StackFrame{returnAddress = mainPc, locals = []}], shouldExit = False, running = True}
                    result <- liftIO $ runVMVM vm
                    let finalStack = stack result
                    let isRunning = running result
                    if isRunning
                        then return $ Just False
                        else return $ Just $ case find (\case DBool _ -> True; _ -> False) finalStack of
                            Just (DBool b) -> b
                            _ -> False

createVirtualFunctions :: StateT (CompilerState a) IO ()
createVirtualFunctions = do
    impls' <- gets impls
    traits' <- gets traits
    let traitsAssoc = map (\x -> (x, filter (\y -> Parser.trait y == Parser.name x) impls')) traits'
    mapM_ compileTrait traitsAssoc
  where
    compileTrait :: (Parser.Expr, [Parser.Expr]) -> StateT (CompilerState a) IO ()
    compileTrait (trait, impl) = do
        let methods = Parser.methods trait
        let fors = map Parser.for impl
        mapM_ (\method -> createBaseDef method trait fors) methods
    createBaseDef :: Parser.Expr -> Parser.Expr -> [Parser.Type] -> StateT (CompilerState a) IO ()
    createBaseDef (Parser.FuncDec{name, types = typess}) trait fors = do
        let traitName = Parser.name trait
        impls' <- gets impls
        let funame = name ++ "#0"
        let body =
                Label funame
                    : concatMap
                        ( \forType ->
                            let forStructName = structNameFromType forType
                                implForFor = find (\impl -> structNameFromType (Parser.for impl) == forStructName && Parser.trait impl == traitName) impls'
                             in case implForFor of
                                    Just impl ->
                                        let traitTypeArgs = Parser.traitTypeArgs impl
                                            traitTypeArgsStr = if null traitTypeArgs then "" else "<" ++ intercalate "," (map typeToString traitTypeArgs) ++ ">"
                                            implForTypeStr = typeToString (Parser.for impl)
                                         in [LStow (length typess - 2) "__ts", Dup, Push $ DTypeQuery forStructName, TypeEq, LStore "__ta", LUnstow "__ts", LLoad "__ta", Jt (traitName ++ traitTypeArgsStr ++ "." ++ implForTypeStr ++ "::" ++ name)]
                                    Nothing -> []
                        )
                        fors
                    ++ [Push $ DString $ "\npanic: No matching implementation of " ++ traitName ++ ", tried calling " ++ name ++ "\n", Builtin Print, Exit]
        modify (\s -> s{functions = functions s ++ [Function name funame body typess "__outside"]})
        return ()
    createBaseDef _ _ _ = return ()

locateLabel :: [Instruction] -> String -> Int
locateLabel program label = do
    let x = elemIndex (Label label) program
    case x of
        Just x' -> x'
        Nothing -> error $ "Label " ++ label ++ " not found"

renderCompilerErrors :: [CompilerError] -> String -> String
renderCompilerErrors errors input =
    let sourceErrors = map (\(CompilerError msg pos) -> SourceError{errorMessage = msg, errorPosition = pos}) errors
     in renderErrors sourceErrors input

compileDry :: Parser.Program -> String -> IO (Either [Instruction] [CompilerError])
compileDry prog sourcePath' =
    liftIO
        ( evalStateT
            (compileProgramBare prog)
            (initCompilerStateWithFile prog sourcePath')
        )

compileFail :: String -> [CompilerError] -> String -> IO ()
compileFail fileName errors file = putStrLn (fileName ++ "\x1b[31m\x1b[0m \n" ++ renderCompilerErrors errors file)
