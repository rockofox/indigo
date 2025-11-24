module BytecodeCompiler where

import AST (Position (..), anyPosition, zeroPosition)
import AST qualified as Parser
import AST qualified as Parser.Type (Type (Unknown))
import Control.Monad (unless, when, (>=>))
import Control.Monad.Loops (firstM)
import Control.Monad.State (MonadIO (liftIO), StateT, evalStateT, gets, modify)
import Data.Bifunctor (second)
import Data.Functor ((<&>))
import Data.List (elemIndex, find, groupBy, inits, intercalate, nub)
import Data.List.Split qualified
import Data.Map qualified
import Data.Maybe (fromJust, isJust, isNothing)
import Data.String
import Data.Text (isPrefixOf, splitOn)
import Data.Text qualified
import Data.Text qualified as T
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
    , funcDecs :: [Parser.Expr]
    , structDecs :: [Parser.Expr]
    , lastLabel :: Int
    , lets :: [Let]
    , traits :: [Parser.Expr]
    , impls :: [Parser.Expr]
    , currentContext :: String -- TODO: Nested contexts
    , externals :: [External]
    , functionsByTrait :: [(String, String, String, Parser.Expr)]
    , errors :: [CompilerError]
    , sourcePath :: String
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
        0
        []
        []
        []
        "__outside"
        []
        []
        []
        "no file"

initCompilerStateWithFile :: Parser.Program -> String -> CompilerState a
initCompilerStateWithFile prog =
    CompilerState
        prog
        []
        []
        []
        0
        []
        []
        []
        "__outside"
        []
        []
        []

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
extractPosition (Parser.Impl{implPos}) = implPos
extractPosition (Parser.StrictEval{strictEvalPos}) = strictEvalPos
extractPosition (Parser.External{externalPos}) = externalPos
extractPosition (Parser.When{whenPos}) = whenPos

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
        Right prog@(Parser.Program progExpr) -> do
            _ <-
                liftIO $
                    compileDry prog "<prelude>" >>= \case
                        Right err -> compileFail "<prelude>" err i >> error ""
                        Left p' -> return p'
            return $ progExpr ++ [Parser.FuncDef "__sep" [] (Parser.Placeholder anyPosition) anyPosition]

compileProgram :: Parser.Program -> StateT (CompilerState a) IO (Either [Instruction] [CompilerError])
compileProgram (Parser.Program expr) = do
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
compileProgramBare (Parser.Program expr) = do
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
            if bothNumeric
                then return True
                else return $ y' `elem` impls

evaluateFunction :: [Parser.Type] -> Int -> Parser.Type
evaluateFunction types taken = case drop (length types - taken + 1) types of
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
    compileTypeFromType (Parser.StructT "Int") = [Push $ DInt 0]
    compileTypeFromType (Parser.StructT "Float") = [Push $ DFloat 0.0]
    compileTypeFromType (Parser.StructT "Double") = [Push $ DDouble 0.0]
    compileTypeFromType (Parser.StructT "Bool") = [Push $ DBool False]
    compileTypeFromType (Parser.StructT "Char") = [Push $ DChar '\0']
    compileTypeFromType (Parser.StructT "String") = [Push $ DString ""]
    compileTypeFromType (Parser.StructT "CPtr") = [Push $ DCPtr $ ptrToWordPtr nullPtr]
    compileTypeFromType _ = [Push DNone]

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
typeOf (Parser.IntLit _ _) = return $ Parser.StructT "Int"
typeOf (Parser.FloatLit _ _) = return $ Parser.StructT "Float"
typeOf (Parser.BoolLit _ _) = return $ Parser.StructT "Bool"
typeOf (Parser.StringLit _ _) = return $ Parser.StructT "String"
typeOf (Parser.Add x _ _) = typeOf x
typeOf (Parser.Sub x _ _) = typeOf x
typeOf (Parser.Mul x _ _) = typeOf x
typeOf (Parser.Div x _ _) = typeOf x
typeOf (Parser.Power x _ _) = typeOf x
typeOf (Parser.UnaryMinus x _) = typeOf x
typeOf (Parser.Eq{}) = return $ Parser.StructT "Bool"
typeOf (Parser.Neq{}) = return $ Parser.StructT "Bool"
typeOf (Parser.Lt{}) = return $ Parser.StructT "Bool"
typeOf (Parser.Gt{}) = return $ Parser.StructT "Bool"
typeOf (Parser.Le{}) = return $ Parser.StructT "Bool"
typeOf (Parser.Ge{}) = return $ Parser.StructT "Bool"
typeOf (Parser.And{}) = return $ Parser.StructT "Bool"
typeOf (Parser.Or{}) = return $ Parser.StructT "Bool"
typeOf (Parser.Not _ _) = return $ Parser.StructT "Bool"
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
typeOf (Parser.StructLit x _ _) = return $ Parser.StructT x
typeOf (Parser.ListLit [Parser.Var{varName}] _) = return $ Parser.List $ Parser.StructT varName
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
typeOf (Parser.Lambda{}) = return $ Parser.Fn [] Parser.Any
typeOf (Parser.Cast _ (Parser.Var to _) _) = return $ Parser.StructT to
typeOf (Parser.Cast _ b _) = typeOf b
typeOf (Parser.TypeLit x _) = return x
typeOf (Parser.Flexible x _) = typeOf x
typeOf (Parser.Trait{}) = error "Cannot infer type of trait"
typeOf (Parser.Impl{}) = error "Cannot infer type of impl"
typeOf (Parser.Then _ b _) = typeOf b
typeOf (Parser.StrictEval x _) = typeOf x
typeOf (Parser.External{}) = error "Cannot infer type of external"
typeOf (Parser.CharLit _ _) = return $ Parser.StructT "Char"
typeOf (Parser.DoubleLit _ _) = return $ Parser.StructT "Double"
typeOf (Parser.ParenApply a _ _) = typeOf a
typeOf (Parser.ListAdd x _ _) = typeOf x
typeOf (Parser.When _ branches else_ _) =
    case branches of
        [] -> maybe (return Parser.Unknown) typeOf else_
        ((_, body) : _) -> typeOf body

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
    let impls'' = filter (\x -> Parser.for x == structName) impls'
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
    let impls'' = filter (\x -> Parser.for x == structName) impls'
    let methods = concatMap Parser.methods impls''
    return $ map Parser.name methods

findBaseDecInTraits :: String -> StateT (CompilerState a) IO (Maybe Parser.Expr)
findBaseDecInTraits funcName = do
    traits' <- gets traits
    let baseDecs = map (find (\y -> Parser.name y == funcName) . Parser.methods) traits'
    return $ firstJust id baseDecs

typeToData :: Parser.Type -> VM.Data
typeToData (Parser.StructT "Int") = VM.DInt 0
typeToData (Parser.StructT "Float") = VM.DFloat 0
typeToData (Parser.StructT "Double") = VM.DDouble 0
typeToData (Parser.StructT "Bool") = VM.DBool False
typeToData (Parser.StructT "String") = VM.DString ""
typeToData (Parser.StructT "IO") = VM.DNone -- Hmmm...
typeToData (Parser.StructT "Char") = VM.DChar ' '
typeToData (Parser.StructT "CPtr") = VM.DCPtr 0
typeToData Parser.StructT{} = VM.DMap Data.Map.empty
typeToData Parser.Any = VM.DNone
typeToData x = error $ "Cannot convert type " ++ show x ++ " to data"

typeToString :: Parser.Type -> String
typeToString (Parser.StructT x) = x
typeToString x = show x

typesEqual :: Parser.Type -> Parser.Type -> Bool
typesEqual (Parser.StructT x) (Parser.StructT y) = x == y
typesEqual x y = x == y

extractPatternVars :: Parser.Expr -> [String]
extractPatternVars (Parser.Var{varName}) = [varName]
extractPatternVars (Parser.ListLit{listLitExprs}) = concatMap extractPatternVars listLitExprs
extractPatternVars (Parser.ListPattern{listPatternExprs}) = concatMap extractPatternVars listPatternExprs
extractPatternVars (Parser.StructLit{structLitFields}) = concatMap (\(_, e) -> extractPatternVars e) structLitFields
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
    let existingVars = map (\l -> l.name) letsBefore
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
    argTypes <- mapM typeOf funcArgs
    functions' <- gets functions
    funcDecs' <- gets funcDecs
    curCon <- gets currentContext
    externals' <- gets externals
    fbt <- gets functionsByTrait
    let contexts = map (T.pack . intercalate "@") (inits (Data.List.Split.splitOn "@" curCon))
    -- Find the function in any context using firstM
    let contextFunctions = firstJust (\context -> findFunction (Data.Text.unpack context ++ "@" ++ funcName) functions' argTypes) contexts
    let implsForExpectedTypePrefixes = map (\x -> x ++ "." ++ typeToString expectedType ++ "::" ++ funcName) implsForExpectedType

    let fun = case find (\(Function{baseName}) -> baseName `elem` implsForExpectedTypePrefixes) functions' of
            Just funf -> funf
            Nothing -> case contextFunctions of
                (Just lf) -> lf
                Nothing -> case findFunction funcName functions' argTypes of
                    (Just f) -> f
                    Nothing -> Function{baseName = unmangleFunctionName funcName, funame = funcName, function = [], types = [], context = "__outside"}
    -- traceShowM $ "Looking for function " ++ funcName ++ " in context " ++ curCon ++ " with types " ++ show argTypes ++ " and expected type " ++ show expectedType
    -- traceShowM fbt
    let funcDec = case find (\case Parser.FuncDec{name} -> name == baseName fun; _ -> False) funcDecs' of
            Just fd -> do
                case find (\(_, n, _, newDec) -> n == funcName && take (length funcArgs) (Parser.types newDec) == argTypes) fbt of
                    Just (_, _, fqn, newDec) -> Just Parser.FuncDec{Parser.name = fqn, Parser.types = Parser.types newDec, Parser.generics = [], funcDecPos = anyPosition}
                    Nothing -> Just fd
            Nothing -> find (\case Parser.FuncDec{name} -> name == baseName fun; _ -> False) funcDecs'
    let external = find (\x -> x.name == funcName) externals'

    let isLocal = T.pack (takeWhile (/= '@') curCon ++ "@") `isPrefixOf` T.pack fun.funame
    -- traceM $ "Local check: " ++ (takeWhile (/='@') curCon) ++ " " ++ fun.funame
    let callWay = (if isLocal then CallLocal (funame fun) else Call (funame fun))

    (argsOk, msgs) <- case funcDec of
        Just fd -> do
            let expectedArgTypes = init $ Parser.types fd
            let zippedArgs = zip argTypes expectedArgTypes
            wrongArgsBools <- mapM (uncurry typeCompatible) zippedArgs
            let wrongArgs = [show t1 ++ " != " ++ show t2 | ((t1, t2), ok) <- zip zippedArgs wrongArgsBools, not ok]
            let tooManyArgs = ["Too many arguments provided." | length argTypes > length expectedArgTypes]
            return (null (wrongArgs ++ tooManyArgs), wrongArgs ++ tooManyArgs)
        Nothing -> return (True, [])

    unless argsOk $ cerror ("Function " ++ funcName ++ " called with incompatible types: " ++ intercalate ", " msgs) funcPos

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
compileExpr fd@(Parser.FuncDec{}) _ = do
    modify (\s -> s{funcDecs = fd : funcDecs s})
    return [] -- Function declarations are only used for compilation
compileExpr (Parser.FuncDef{name = origName, args, body}) expectedType = do
    curCon <- gets currentContext
    funs <- gets functions
    let previousContext = curCon
    let name = if curCon /= "__outside" then curCon ++ "@" ++ origName else origName
    let origName' = if curCon /= "__outside" then curCon ++ "@" ++ origName else origName
    let isFirst = isNothing $ find (\x -> x.baseName == origName') funs
    -- when (not isFirst) $ traceM $ "Function " ++ name ++ " already exists"
    -- let argTypes = map Parser.typeOf args
    modify (\s -> s{currentContext = name})
    funcDecs' <- gets funcDecs
    when (isNothing $ find (\case Parser.FuncDec{name = name'} -> name' == name; _ -> False) funcDecs') $ modify (\s -> s{funcDecs = Parser.FuncDec name (replicate (length args + 1) expectedType) [] anyPosition : funcDecs s})

    funame <- if name /= "main" then ((name ++ "#") ++) . show <$> allocId else return "main"
    modify (\s -> s{functions = Function name funame [] [] curCon : functions s})
    funcDecs'' <- gets funcDecs

    let funcDec = fromJust $ find (\case Parser.FuncDec{name = name'} -> name' == name; _ -> False) funcDecs''

    body' <- compileExpr body (last $ Parser.types funcDec)

    nextFunId <- gets lastLabel
    args' <- concatMapM (`compileParameter` (name, funame, nextFunId)) (reverse (filter (\case Parser.Placeholder _ -> False; _ -> True) args))
    let function = Label funame : [StoreSideStack | isFirst] ++ [LoadSideStack | not isFirst] ++ args' ++ body' ++ [ClearSideStack] ++ ([Ret | name /= "main"])
    -- modify (\s -> s{functions = Function name funame function funcDec.types : tail (functions s)})
    modify (\s -> s{functions = Function name funame function funcDec.types curCon : functions s})
    modify (\s -> s{currentContext = previousContext})
    return [] -- Function definitions get appended at the last stage of compilation
  where
    compileParameter :: Parser.Expr -> (String, String, Int) -> StateT (CompilerState a) IO [Instruction]
    compileParameter pat (funcName, _currentFuname, nextFunId) = do
        let nextFunName = funcName ++ "#" ++ show nextFunId
        compilePatternMatch pat nextFunName expectedType
compileExpr (Parser.ParenApply{parenApplyExpr, parenApplyArgs}) expectedType = do
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
    typeOf letValue >>= \v -> modify (\s -> s{lets = Let{name = letName, vtype = v, context = curCon} : lets s})
    value' <- typeOf letValue >>= compileExpr letValue
    return $ value' ++ [LStore letName]
compileExpr (Parser.Function{def = [Parser.FuncDef{body = Parser.StrictEval{strictEvalExpr = e}}], dec}) expectedType = do
    let name = dec.name
    evaledExpr <- compileExpr e expectedType
    return $ evaledExpr ++ [LStore name]
compileExpr (Parser.Function{def = a, dec = b@Parser.FuncDec{name = _name, types = _types}}) _ = do
    curCon <- gets currentContext
    let mangledName = if curCon /= "__outside" then curCon ++ "@" ++ b.name else b.name
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
compileExpr (Parser.Cast{castExpr, castType}) _ = do
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
    compileType (Parser.Var{}) = [Push DNone]
    compileType x = error $ "Cannot cast to type " ++ show x
compileExpr st@(Parser.Struct{name = structName, fields, is}) expectedType = do
    modify (\s -> s{structDecs = st : structDecs s})
    mapM_ createFieldTrait fields
    mapM_ ((`compileExpr` expectedType) . (\t -> Parser.Impl{trait = t, for = structName, methods = [], implPos = anyPosition})) is
    return []
  where
    createFieldTrait :: (String, Parser.Type) -> StateT (CompilerState a) IO ()
    createFieldTrait (name, _) = do
        let traitName = "__field_" ++ name
        let trait = Parser.Trait{name = traitName, methods = [Parser.FuncDec{Parser.name = name, Parser.types = [Parser.Self, Parser.Any], Parser.generics = [], funcDecPos = anyPosition}], traitPos = anyPosition}
        let impl = Parser.Impl{trait = traitName, for = Parser.name st, methods = [Parser.FuncDef{name = name, args = [Parser.Var{varName = "self", varPos = zeroPosition}], body = Parser.StructAccess{structAccessStruct = Parser.Var{varName = "self", varPos = zeroPosition}, structAccessField = Parser.Var{varName = name, varPos = zeroPosition}, structAccessPos = anyPosition}, funcDefPos = anyPosition}], implPos = anyPosition}
        _ <- compileExpr trait Parser.Any
        _ <- compileExpr impl Parser.Any
        return ()
compileExpr (Parser.StructLit{structLitName, structLitFields}) _ = do
    fields' <- mapM ((`compileExpr` Parser.Any) . snd) structLitFields
    let names = map (DString . fst) structLitFields
    let instructions = zip names fields' >>= \(name', field) -> field ++ [Push name']
    implsForStruct <- implsFor structLitName
    return $ instructions ++ [Push $ DString structLitName, Push $ DString "__name", Push $ DList (map DString implsForStruct), Push $ DString "__traits", PackMap $ length structLitFields * 2 + 4]
compileExpr (Parser.StructAccess{structAccessStruct, structAccessField = Parser.Var{varName = field}}) expectedType = do
    struct' <- compileExpr structAccessStruct expectedType
    return $ struct' ++ [Access field]
compileExpr (Parser.Import{objects = o, from = from, as = as, qualified = qualified}) expectedType = do
    when (o /= ["*"]) $ error "Only * imports are supported right now"
    sourcePath' <- gets sourcePath
    let sourcePath = takeDirectory sourcePath'
    i <- liftIO (findSourceFile (from ++ ".in") [sourcePath] >>= readFile)
    let (expr, prog) = case parseProgram (T.pack i) Parser.initCompilerFlags{Parser.needsMain = False} of -- FIXME: pass on flags
            Left err -> error $ "Parse error:\n" ++ renderErrors (parseErrorBundleToSourceErrors err (T.pack i)) i
            Right p@(Parser.Program exprs) -> (exprs, p)
    _ <-
        liftIO $
            compileDry prog sourcePath' >>= \case
                Right err -> error $ "Compile error: " ++ show err
                Left p' -> return p'
    if qualified || isJust as
        then do
            let alias = if qualified then from else fromJust as
            concatMapM (`compileExpr` expectedType) (map (`mangleAST` alias) expr)
        else concatMapM (`compileExpr` expectedType) expr
  where
    mangleAST :: Parser.Expr -> String -> Parser.Expr
    mangleAST (Parser.FuncDec{name, types}) alias = Parser.FuncDec{name = alias ++ "@" ++ name, types = types, generics = [], funcDecPos = anyPosition}
    mangleAST (Parser.Function{def = fdef, dec}) alias = Parser.Function{def = map (`mangleAST` alias) fdef, dec = mangleAST dec alias, functionPos = anyPosition}
    mangleAST (Parser.FuncDef{name, args, body}) alias = Parser.FuncDef{name = alias ++ "@" ++ name, args = args, body = mangleAST body alias, funcDefPos = anyPosition}
    mangleAST x _ = x
compileExpr (Parser.Trait{name, methods}) expectedType = do
    let methods' = map (\case Parser.FuncDec{name = name', types} -> Parser.FuncDec{name = name', types = types, generics = [], funcDecPos = anyPosition}; _ -> error "Expected FuncDec in Trait methods") methods
    modify (\s -> s{traits = Parser.Trait{name = name, methods = methods, traitPos = anyPosition} : traits s})
    mapM_ (`compileExpr` expectedType) methods'
    return []
compileExpr (Parser.Impl{trait, for, methods}) expectedType = do
    methods' <-
        mapM
            ( \case
                Parser.FuncDef{name = name', args, body} -> do
                    let fullyQualifiedName = trait ++ "." ++ for ++ "::" ++ name'
                    trait' <- gets traits >>= \traits' -> return $ fromJust $ find (\x -> Parser.name x == trait) traits'
                    let dec = fromJust $ find (\x -> Parser.name x == name') (Parser.methods trait')
                    let newDec = Parser.FuncDec{name = fullyQualifiedName, types = unself dec.types for, generics = dec.generics, funcDecPos = anyPosition}
                    _ <- compileExpr newDec Parser.Any
                    modify (\s -> s{functionsByTrait = (for, name', fullyQualifiedName, newDec) : functionsByTrait s})
                    return $ Parser.FuncDef{name = fullyQualifiedName, args = args, body = body, funcDefPos = anyPosition}
                _ -> error "Expected FuncDef in Impl methods"
            )
            methods
    modify (\s -> s{impls = Parser.Impl{trait = trait, for = for, methods = methods, implPos = anyPosition} : impls s})
    mapM_ (`compileExpr` expectedType) methods'
    return []
  where
    unself :: [Parser.Type] -> String -> [Parser.Type]
    unself types self = map (\case Parser.Self -> Parser.StructT self; x -> x) types
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
    createBaseDef :: Parser.Expr -> Parser.Expr -> [String] -> StateT (CompilerState a) IO ()
    createBaseDef (Parser.FuncDec{name, types = typess}) trait fors = do
        let traitName = Parser.name trait
        let body =
                Label name
                    : concatMap (\for -> [LStow (length typess - 2) "__ts", Dup, Push $ DTypeQuery for, TypeEq, LStore "__ta", LUnstow "__ts", LLoad "__ta", Jt (traitName ++ "." ++ for ++ "::" ++ name)]) fors
                    ++ [Push $ DString $ "\npanic: No matching implementation of " ++ traitName ++ ", tried calling " ++ name ++ "\n", Builtin Print, Exit]
        modify (\s -> s{functions = functions s ++ [Function name (name ++ "#0") body typess "__outside"]})
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
