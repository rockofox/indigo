module BytecodeCompiler where

import AST (Position (..), anyPosition, zeroPosition)
import AST qualified as Parser
import AST qualified as Parser.Type (Type (Unknown))
import Control.Monad (unless, when, (>=>))
import Control.Monad.Loops (firstM)
import Control.Monad.State (MonadIO (liftIO), StateT, evalStateT, gets, modify)
import Data.Bifunctor (second)
import Data.Functor ((<&>))
import Data.List (elemIndex, find, groupBy, inits, intercalate)
import Data.List.Split qualified
import Data.Map qualified
import Data.Maybe (fromJust, isJust, isNothing)
import Data.String
import Data.Text (isPrefixOf, splitOn)
import Data.Text qualified
import Data.Text qualified as T
import Data.Void
import Debug.Trace (trace, traceM, traceShowM)
import Foreign (nullPtr, ptrToWordPtr)
import Foreign.C.Types ()
import GHC.Generics (Generic)
import Parser (CompilerFlags (CompilerFlags), name, parseProgram, types)
import Parser qualified
import Paths_indigo qualified
import System.Directory (doesFileExist)
import System.Environment
import System.FilePath
import Text.Megaparsec (ParseError, errorBundlePretty)
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
    -- TODO
    unless (pos == AST.zeroPosition) $ modify (\s -> s{errors = CompilerError msg pos : errors s})

allocId :: StateT (CompilerState a) IO Int
allocId = do
    s <- gets lastLabel
    modify (\s' -> s'{lastLabel = s + 1})
    return s

preludeExpr :: IO [Parser.Expr]
preludeExpr = do
    i <- liftIO preludeFile
    case parseProgram (T.pack i) CompilerFlags{verboseMode = False, needsMain = False} of -- FIXME: pass on flags
        Left err -> error $ "Parse error: " ++ errorBundlePretty err
        Right prog@(Parser.Program progExpr) -> do
            _ <-
                liftIO $
                    compileDry prog "<prelude>" >>= \case
                        Right err -> compileFail "<prelude>" err i >> error ""
                        Left p' -> return p'
            return $ progExpr ++ [Parser.FuncDef "__sep" [] Parser.Placeholder]

compileProgram :: Parser.Program -> StateT (CompilerState a) IO (Either [Instruction] [CompilerError])
compileProgram (Parser.Program expr) = do
    prelude <- liftIO preludeExpr
    prelude' <- concatMapM (`compileExpr` Parser.Any) prelude
    freePart <- concatMapM (`compileExpr` Parser.Any) expr
    createVirtualFunctions
    functions' <- gets functions >>= \x -> return $ concatMap function (reverse x)
    errors' <- gets errors
    if null errors'
        then
            return $ Left $ prelude' ++ functions' ++ freePart ++ [Push $ DInt 0, Exit]
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
    let impls' = map (\x -> x ++ "." ++ y') impls
    -- let compatible = x' == y' || x' == "Any" || y' == "Any"
    let compatible = Parser.compareTypes x y
    if compatible
        then return True
        else do
            return $ y' `elem` impls

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

doBinOp :: Parser.Expr -> Parser.Expr -> Parser.Type -> Instruction -> StateT (CompilerState a) IO [Instruction]
doBinOp x y expectedType op = do
    id' <- allocId
    functions' <- gets functions
    let f = findAnyFunction (Data.Text.unpack $ Data.Text.toLower $ Data.Text.pack $ show op) functions'
    let aName = "__op_a_" ++ show id'
    let bName = "__op_b_" ++ show id'
    x' <- compileExpr x expectedType
    y' <- compileExpr y expectedType

    cast <- case (x, y) of
        (Parser.Flexible{}, Parser.Flexible{}) -> error "Double cast"
        (Parser.Flexible{}, _) -> return $ Cast : y'
        (_, Parser.Flexible{}) -> return $ [Swp, Cast] ++ x'
        _ -> return []
    case (x, y) of
        (Parser.Placeholder, Parser.Placeholder) -> return [PushPf (funame $ fromJust f) 0]
        (Parser.Placeholder, _) -> return $ y' ++ [PushPf (funame $ fromJust f) 1]
        (_, Parser.Placeholder) -> return $ x' ++ [PushPf (funame $ fromJust f) 1]
        _ -> return (x' ++ LStore aName : y' ++ [LStore bName, LLoad aName, LLoad bName] ++ cast ++ [op])

typeOf :: Parser.Expr -> StateT (CompilerState a) IO Parser.Type
typeOf (Parser.FuncCall{funcName, funcArgs}) = do
    funcDecs' <- gets funcDecs
    let a =
            maybe
                [Parser.Type.Unknown]
                Parser.types
                (find (\x -> x.name == funcName) funcDecs')
    return $ evaluateFunction a (length funcArgs)
typeOf (Parser.Var{varName}) = do
    lets' <- gets lets
    let a = maybe Parser.Any vtype (find (\x -> x.name == varName) lets')
    return a
typeOf x = return $ Parser.typeOf x

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

compileExpr :: Parser.Expr -> Parser.Type -> StateT (CompilerState a) IO [Instruction]
compileExpr (Parser.Add{addLhs = x, addRhs = y}) expectedType = compileExpr (Parser.FuncCall{funcName = "+", funcArgs = [x, y], funcPos = zeroPosition}) expectedType >>= doBinOp x y expectedType . last
compileExpr (Parser.Sub{subLhs = x, subRhs = y}) expectedType = compileExpr (Parser.FuncCall{funcName = "-", funcArgs = [x, y], funcPos = zeroPosition}) expectedType >>= doBinOp x y expectedType . last
compileExpr (Parser.Mul{mulLhs = x, mulRhs = y}) expectedType = compileExpr (Parser.FuncCall{funcName = "*", funcArgs = [x, y], funcPos = zeroPosition}) expectedType >>= doBinOp x y expectedType . last
compileExpr (Parser.Div{divLhs = x, divRhs = y}) expectedType = compileExpr (Parser.FuncCall{funcName = "/", funcArgs = [x, y], funcPos = zeroPosition}) expectedType >>= doBinOp x y expectedType . last
compileExpr (Parser.Modulo{moduloLhs = x, moduloRhs = y}) expectedType = compileExpr (Parser.FuncCall{funcName = "%", funcArgs = [x, y], funcPos = zeroPosition}) expectedType >>= doBinOp x y expectedType . last
compileExpr (Parser.Power{powerBase = x, powerExponent = y}) expectedType = compileExpr (Parser.FuncCall{funcName = "^", funcArgs = [x, y], funcPos = zeroPosition}) expectedType >>= doBinOp x y expectedType . last
compileExpr (Parser.Gt{gtLhs = x, gtRhs = y}) expectedType = compileExpr (Parser.FuncCall{funcName = ">", funcArgs = [x, y], funcPos = zeroPosition}) expectedType >>= doBinOp x y expectedType . last
compileExpr (Parser.Lt{ltLhs = x, ltRhs = y}) expectedType = compileExpr (Parser.FuncCall{funcName = "<", funcArgs = [x, y], funcPos = zeroPosition}) expectedType >>= doBinOp x y expectedType . last
compileExpr (Parser.Ge{geLhs = x, geRhs = y}) expectedType = compileExpr (Parser.FuncCall{funcName = ">=", funcArgs = [x, y], funcPos = zeroPosition}) expectedType >>= doBinOp x y expectedType . last
compileExpr (Parser.Le{leLhs = x, leRhs = y}) expectedType = compileExpr (Parser.FuncCall{funcName = "<=", funcArgs = [x, y], funcPos = zeroPosition}) expectedType >>= doBinOp x y expectedType . last
compileExpr (Parser.Not{notExpr = x}) expectedType = compileExpr x expectedType >>= \x' -> return (x' ++ [Not])
compileExpr (Parser.Eq{eqLhs = x, eqRhs = y}) expectedType = compileExpr (Parser.FuncCall{funcName = "==", funcArgs = [x, y], funcPos = zeroPosition}) expectedType >>= doBinOp x y expectedType . last
compileExpr (Parser.Neq{neqLhs = x, neqRhs = y}) expectedType = compileExpr (Parser.FuncCall{funcName = "!=", funcArgs = [x, y], funcPos = zeroPosition}) expectedType >>= doBinOp x y expectedType . last
compileExpr (Parser.And{andLhs = x, andRhs = y}) expectedType = compileExpr (Parser.FuncCall{funcName = "&&", funcArgs = [x, y], funcPos = zeroPosition}) expectedType >>= doBinOp x y expectedType . last
compileExpr (Parser.Or{orLhs = x, orRhs = y}) expectedType = compileExpr (Parser.FuncCall{funcName = "||", funcArgs = [x, y], funcPos = zeroPosition}) expectedType >>= doBinOp x y expectedType . last
compileExpr (Parser.IntLit{intValue = x}) _ = return [Push $ DInt $ fromIntegral x]
compileExpr (Parser.UnaryMinus{unaryMinusExpr = Parser.FloatLit{floatValue = x}}) _ = return [Push $ DFloat (-x)]
compileExpr (Parser.UnaryMinus{unaryMinusExpr = Parser.IntLit{intValue = x}}) _ = return [Push $ DInt $ -fromInteger x]
compileExpr (Parser.StringLit{stringValue = x}) _ = return [Push $ DString x]
compileExpr (Parser.DoBlock{doBlockExprs = exprs}) expectedType = do
    let grouped = groupBy (\a b -> isFuncCall a && isFuncCall b) exprs
    let nestedSequence = concatMap (\case [] -> []; [x] -> if isFuncCall x then [Parser.FuncCall{funcName = "sequence", funcArgs = [x, Parser.FuncCall{funcName = "nop", funcArgs = [], funcPos = anyPosition}], funcPos = anyPosition}] else [x]; xs -> if all isFuncCall xs then [foldl1 (\a b -> Parser.FuncCall{funcName = "sequence", funcArgs = [a, b], funcPos = anyPosition}) xs] else xs) grouped
    if length (filter isFuncCall exprs) == 1 then concatMapM (`compileExpr` expectedType) exprs else concatMapM (`compileExpr` expectedType) nestedSequence
  where
    isFuncCall :: Parser.Expr -> Bool
    isFuncCall (Parser.FuncCall{funcName}) = funcName `notElem` internalFunctions
    isFuncCall _ = False
compileExpr Parser.Placeholder _ = return []
compileExpr (Parser.FuncCall{funcName = "unsafeAdd", funcArgs = [x, y], ..}) expectedType = compileExpr x expectedType >>= \x' -> compileExpr y expectedType >>= \y' -> return (x' ++ y' ++ [Add])
compileExpr (Parser.FuncCall{funcName = "unsafeSub", funcArgs = [x, y], ..}) expectedType = compileExpr x expectedType >>= \x' -> compileExpr y expectedType >>= \y' -> return (x' ++ y' ++ [Sub])
compileExpr (Parser.FuncCall{funcName = "unsafeMul", funcArgs = [x, y], ..}) expectedType = compileExpr x expectedType >>= \x' -> compileExpr y expectedType >>= \y' -> return (x' ++ y' ++ [Mul])
compileExpr (Parser.FuncCall{funcName = "unsafeDiv", funcArgs = [x, y], ..}) expectedType = compileExpr x expectedType >>= \x' -> compileExpr y expectedType >>= \y' -> return (x' ++ y' ++ [Div])
compileExpr (Parser.FuncCall{funcName = "unsafeMod", funcArgs = [x, y], ..}) expectedType = compileExpr x expectedType >>= \x' -> compileExpr y expectedType >>= \y' -> return (x' ++ y' ++ [Mod])
compileExpr (Parser.FuncCall{funcName = "unsafePow", funcArgs = [x, y], ..}) expectedType = compileExpr x expectedType >>= \x' -> compileExpr y expectedType >>= \y' -> return (x' ++ y' ++ [Pow])
compileExpr (Parser.FuncCall{funcName = "unsafeGt", funcArgs = [x, y], ..}) expectedType = compileExpr x expectedType >>= \x' -> compileExpr y expectedType >>= \y' -> return (x' ++ y' ++ [Gt])
compileExpr (Parser.FuncCall{funcName = "unsafeLt", funcArgs = [x, y], ..}) expectedType = compileExpr x expectedType >>= \x' -> compileExpr y expectedType >>= \y' -> return (x' ++ y' ++ [Lt])
compileExpr (Parser.FuncCall{funcName = "unsafeGe", funcArgs = [x, y], ..}) expectedType = compileExpr x expectedType >>= \x' -> compileExpr y expectedType >>= \y' -> return (x' ++ y' ++ [Lt, Not])
compileExpr (Parser.FuncCall{funcName = "unsafeLe", funcArgs = [x, y], ..}) expectedType = compileExpr x expectedType >>= \x' -> compileExpr y expectedType >>= \y' -> return (x' ++ y' ++ [Gt, Not])
compileExpr (Parser.FuncCall{funcName = "unsafeEq", funcArgs = [x, y], ..}) expectedType = compileExpr x expectedType >>= \x' -> compileExpr y expectedType >>= \y' -> return (x' ++ y' ++ [Eq])
compileExpr (Parser.FuncCall{funcName = "unsafeNeq", funcArgs = [x, y], ..}) expectedType = compileExpr x expectedType >>= \x' -> compileExpr y expectedType >>= \y' -> return (x' ++ y' ++ [Eq, Not])
compileExpr (Parser.FuncCall{funcName = "unsafeAnd", funcArgs = [x, y], ..}) expectedType = compileExpr x expectedType >>= \x' -> compileExpr y expectedType >>= \y' -> return (x' ++ y' ++ [And])
compileExpr (Parser.FuncCall{funcName = "unsafeOr", funcArgs = [x, y], ..}) expectedType = compileExpr x expectedType >>= \x' -> compileExpr y expectedType >>= \y' -> return (x' ++ y' ++ [Or])
compileExpr (Parser.FuncCall{funcName = "unsafePrint", funcArgs = [x], ..}) expectedType = compileExpr x expectedType >>= \x' -> return (x' ++ [Builtin Print])
compileExpr (Parser.FuncCall{funcName = "unsafeGetLine", ..}) _ = return [Builtin GetLine]
compileExpr (Parser.FuncCall{funcName = "unsafeGetChar", ..}) _ = return [Builtin GetChar]
compileExpr (Parser.FuncCall{funcName = "unsafeRandom", ..}) _ = return [Builtin Random]
compileExpr (Parser.FuncCall{funcName = "unsafeListAdd", funcArgs = [y, x], ..}) expectedType = compileExpr x expectedType >>= \x' -> compileExpr y expectedType >>= \y' -> return (x' ++ y' ++ [ListAdd 2])
compileExpr (Parser.FuncCall{funcName = "unsafeListIndex", funcArgs = [x, y], ..}) expectedType = compileExpr x expectedType >>= \x' -> compileExpr y expectedType >>= \y' -> return (x' ++ y' ++ [Index])
compileExpr (Parser.FuncCall{funcName = "unsafeListLength", funcArgs = [x], ..}) expectedType = compileExpr x expectedType >>= \x' -> return (x' ++ [Length])
compileExpr (Parser.FuncCall{funcName = "unsafeListSlice", funcArgs = [x, y, z], ..}) expectedType = compileExpr x expectedType >>= \x' -> compileExpr y expectedType >>= \y' -> compileExpr z expectedType >>= \z' -> return (x' ++ y' ++ z' ++ [Slice])
compileExpr (Parser.FuncCall{funcName = "unsafeStructAccess", funcArgs = [x, y], ..}) expectedType = compileExpr x expectedType >>= \x' -> compileExpr y expectedType >>= \y' -> return (x' ++ y' ++ [AAccess])
compileExpr (Parser.FuncCall{funcName = "unsafeKeys", funcArgs = [x], ..}) expectedType = compileExpr x expectedType >>= \x' -> return (x' ++ [Keys])
compileExpr (Parser.FuncCall{funcName = "unsafeUpdate", funcArgs = [x, y, z], ..}) expectedType = compileExpr x expectedType >>= \x' -> compileExpr y expectedType >>= \y' -> compileExpr z expectedType >>= \z' -> return (x' ++ y' ++ z' ++ [Update])
compileExpr (Parser.FuncCall{funcName = "unsafeExit", funcArgs = [x], ..}) expectedType = compileExpr x expectedType >>= \x' -> return (x' ++ [Exit])
compileExpr (Parser.FuncCall{funcName = "abs", funcArgs = [x], ..}) expectedType = compileExpr x expectedType >>= \x' -> return (x' ++ [Abs])
compileExpr (Parser.FuncCall{funcName = "root", funcArgs = [x, Parser.FloatLit{floatValue = y}], ..}) expectedType = compileExpr x expectedType >>= \x' -> return (x' ++ [Push $ DFloat (1.0 / y), Pow])
compileExpr (Parser.FuncCall{funcName = "sqrt", funcArgs = [x], ..}) expectedType = compileExpr x expectedType >>= \x' -> return (x' ++ [Push $ DFloat 0.5, Pow])
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
    let funcDec = case find (\(Parser.FuncDec{name}) -> name == baseName fun) funcDecs' of
            Just fd -> do
                case find (\(_, n, _, newDec) -> n == funcName && take (length funcArgs) (Parser.types newDec) == argTypes) fbt of
                    Just (_, _, fqn, newDec) -> Just Parser.FuncDec{Parser.name = fqn, Parser.types = Parser.types newDec, Parser.generics = []}
                    Nothing -> Just fd
            Nothing -> find (\(Parser.FuncDec{name}) -> name == baseName fun) funcDecs'
    let external = find (\x -> x.name == funcName) externals'

    let isLocal = T.pack (takeWhile (/= '@') curCon ++ "@") `isPrefixOf` T.pack fun.funame
    -- traceM $ "Local check: " ++ (takeWhile (/='@') curCon) ++ " " ++ fun.funame
    let callWay = (if isLocal then CallLocal (funame fun) else Call (funame fun))

    (argsOk, msgs) <- case funcDec of
        Just fd -> do
            let zippedArgs = zip argTypes (init $ Parser.types fd)
            wrongArgsBools <- mapM (uncurry typeCompatible) zippedArgs
            let wrongArgs = [show t1 ++ " != " ++ show t2 | ((t1, t2), ok) <- zip zippedArgs wrongArgsBools, not ok]
            let tooManyArgs = ["Too many arguments provided." | length argTypes > length (init $ Parser.types fd)]
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
    when (isNothing $ find (\(Parser.FuncDec{name = name'}) -> name' == name) funcDecs') $ modify (\s -> s{funcDecs = Parser.FuncDec name (replicate (length args + 1) expectedType) [] : funcDecs s})

    funame <- if name /= "main" then ((name ++ "#") ++) . show <$> allocId else return "main"
    modify (\s -> s{functions = Function name funame [] [] curCon : functions s})
    funcDecs'' <- gets funcDecs

    let funcDec = fromJust $ find (\(Parser.FuncDec{name = name'}) -> name' == name) funcDecs''

    body' <- compileExpr body (last $ Parser.types funcDec)

    args' <- concatMapM (`compileParameter` name) (reverse (filter (/= Parser.Placeholder) args))
    let function = Label funame : [StoreSideStack | isFirst] ++ [LoadSideStack | not isFirst] ++ args' ++ body' ++ [ClearSideStack] ++ ([Ret | name /= "main"])
    -- modify (\s -> s{functions = Function name funame function funcDec.types : tail (functions s)})
    modify (\s -> s{functions = Function name funame function funcDec.types curCon : functions s})
    modify (\s -> s{currentContext = previousContext})
    return [] -- Function definitions get appended at the last stage of compilation
  where
    compileParameter :: Parser.Expr -> String -> StateT (CompilerState a) IO [Instruction]
    compileParameter (Parser.Var{varName}) _ = return [LStore varName]
    compileParameter lex@(Parser.ListLit{listLitExprs}) funcName = do
        nextFunName <- ((funcName ++ "#") ++) . show . (+ 1) <$> allocId
        if null listLitExprs
            then return [Dup, Push $ DList [], Eq, Jf nextFunName, Pop]
            else do
                lex' <- compileExpr lex expectedType
                return $ lex' ++ [Eq, Jf nextFunName] -- TODO: Check if this works
    compileParameter (Parser.ListPattern{listPatternExprs}) n = do
        nextFunName <- ((n ++ "#") ++) . show . (+ 1) <$> allocId
        let lengthCheck = [Dup, Length, Push $ DInt $ fromIntegral $ length listPatternExprs - 1, Lt, StackLength, Push $ DInt 1, Neq, And, Jt nextFunName]
        case last listPatternExprs of
            Parser.ListLit{listLitExprs} -> do
                elements' <- concatMapM (`compileParameter` n) (init listPatternExprs)
                let paramsWithIndex = zip elements' [0 ..]
                let xToY = map (\(x, index) -> [Dup, Push $ DInt index, Index, x]) paramsWithIndex
                l' <- compileExpr (Parser.ListLit{listLitExprs = listLitExprs}) expectedType
                let listThing = [Comment "List thing", Dup, Push $ DInt (length listPatternExprs - 1), Push DNone, Slice] ++ l' ++ [Eq, Jf nextFunName]
                return $ lengthCheck ++ concat xToY ++ listThing
            _ -> do
                elements' <- concatMapM (`compileParameter` n) listPatternExprs
                let paramsWithIndex = zip elements' [0 ..]
                let xToY = map (\(x, index) -> [Dup, Push $ DInt index, Index, x]) paramsWithIndex
                let rest = [Push $ DInt (length listPatternExprs - 1), Push DNone, Slice, last elements']
                return $ lengthCheck ++ concat xToY ++ rest
    compileParameter (Parser.StructLit{structLitName, structLitFields}) funcName = do
        nextFunName <- ((funcName ++ "#") ++) . show . (+ 1) <$> allocId
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
                    return $ a ++ b ++ [Eq, Jf nextFunName]
                )
                fields''
        return $ [Dup, Push $ DTypeQuery structLitName, TypeEq, Jf nextFunName] ++ fieldMappings ++ ([Pop | null fieldChecks]) ++ fieldChecks
    compileParameter Parser.Placeholder _ = return []
    compileParameter x funcName = do
        nextFunName <- ((funcName ++ "#") ++) . show . (+ 1) <$> allocId
        x' <- compileExpr x expectedType
        return $ x' ++ [Eq, Jf nextFunName]
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
compileExpr (Parser.Function{def = a, dec = b@Parser.FuncDec{..}}) _ =
    mapM_ (`compileExpr` last b.types) a >> compileExpr b (last b.types) >> return []
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
    from' <- compileExpr castExpr Parser.Any
    let to' = compileType castType
    return $ from' ++ to' ++ [Cast]
  where
    compileType :: Parser.Expr -> [Instruction]
    compileType (Parser.Var{varName = "Int", ..}) = [Push $ DInt 0]
    compileType (Parser.Var{varName = "Float", ..}) = [Push $ DFloat 0.0]
    compileType (Parser.Var{varName = "Double", ..}) = [Push $ DDouble 0.0]
    compileType (Parser.Var{varName = "Bool", ..}) = [Push $ DBool False]
    compileType (Parser.Var{varName = "Char", ..}) = [Push $ DChar '\0']
    compileType (Parser.Var{varName = "String", ..}) = [Push $ DString ""]
    compileType (Parser.Var{varName = "CPtr", ..}) = [Push $ DCPtr $ ptrToWordPtr nullPtr]
    compileType (Parser.ListLit{listLitExprs = [x]}) = compileType x ++ [PackList 1]
    compileType (Parser.Var{..}) = [Push DNone]
    compileType x = error $ "Cannot cast to type " ++ show x
compileExpr st@(Parser.Struct{name = structName, fields, is}) expectedType = do
    modify (\s -> s{structDecs = st : structDecs s})
    mapM_ createFieldTrait fields
    mapM_ ((`compileExpr` expectedType) . (\t -> Parser.Impl{trait = t, for = structName, methods = []})) is
    return []
  where
    createFieldTrait :: (String, Parser.Type) -> StateT (CompilerState a) IO ()
    createFieldTrait (name, _) = do
        let traitName = "__field_" ++ name
        let trait = Parser.Trait{name = traitName, methods = [Parser.FuncDec{Parser.name = name, Parser.types = [Parser.Self, Parser.Any], Parser.generics = []}]}
        let impl = Parser.Impl{trait = traitName, for = Parser.name st, methods = [Parser.FuncDef{name = name, args = [Parser.Var{varName = "self", varPos = zeroPosition}], body = Parser.StructAccess{structAccessStruct = Parser.Var{varName = "self", varPos = zeroPosition}, structAccessField = Parser.Var{varName = name, varPos = zeroPosition}}}]}
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
            Left err -> error $ "Parse error: " ++ errorBundlePretty err
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
    mangleAST (Parser.FuncDec{name, types, ..}) alias = Parser.FuncDec{name = alias ++ "@" ++ name, types = types, generics = []}
    mangleAST (Parser.Function{def = fdef, dec}) alias = Parser.Function{def = map (`mangleAST` alias) fdef, dec = mangleAST dec alias}
    mangleAST (Parser.FuncDef{name, args, body}) alias = Parser.FuncDef{name = alias ++ "@" ++ name, args = args, body = mangleAST body alias}
    mangleAST x _ = x
compileExpr (Parser.Trait{name, methods}) expectedType = do
    let methods' = map (\(Parser.FuncDec{name = name', types, ..}) -> Parser.FuncDec{name = name', types = types, generics = []}) methods
    modify (\s -> s{traits = Parser.Trait{name = name, methods = methods} : traits s})
    mapM_ (`compileExpr` expectedType) methods'
    return []
compileExpr (Parser.Impl{trait, for, methods}) expectedType = do
    methods' <-
        mapM
            ( \(Parser.FuncDef{name = name', args, body}) -> do
                let fullyQualifiedName = trait ++ "." ++ for ++ "::" ++ name'
                trait' <- gets traits >>= \traits' -> return $ fromJust $ find (\x -> Parser.name x == trait) traits'
                let dec = fromJust $ find (\x -> Parser.name x == name') (Parser.methods trait')
                let newDec = Parser.FuncDec{name = fullyQualifiedName, types = unself dec.types for, generics = dec.generics}
                _ <- compileExpr newDec Parser.Any
                modify (\s -> s{functionsByTrait = (for, name', fullyQualifiedName, newDec) : functionsByTrait s})
                return $ Parser.FuncDef{name = fullyQualifiedName, args = args, body = body}
            )
            methods
    modify (\s -> s{impls = Parser.Impl{trait = trait, for = for, methods = methods} : impls s})
    mapM_ (`compileExpr` expectedType) methods'
    return []
  where
    unself :: [Parser.Type] -> String -> [Parser.Type]
    unself types self = map (\case Parser.Self -> Parser.StructT self; x -> x) types
compileExpr (Parser.Lambda{lambdaArgs, lambdaBody}) expectedType = do
    fId <- allocId
    curCon <- gets currentContext
    let args' = if lambdaArgs == [Parser.Placeholder] then [] else lambdaArgs
    let name = "__lambda" ++ show fId
    let def = Parser.FuncDef{name = name, args = args', body = lambdaBody}
    let dec = Parser.FuncDec{name = name, types = replicate (length args' + 1) Parser.Any, generics = []}
    let fun = Parser.Function{def = [def], dec = dec}
    _ <- compileExpr fun expectedType
    lets' <- gets functions
    let fullName = (fromJust $ findAnyFunction (curCon ++ "@" ++ name) lets').funame
    return [PushPf fullName 0]
compileExpr (Parser.Pipeline{pipelineLhs = a, pipelineRhs = Parser.Var{varName = b, ..}}) expectedType = compileExpr (Parser.FuncCall{funcName = b, funcArgs = [a], funcPos = zeroPosition}) expectedType
compileExpr (Parser.Pipeline{pipelineLhs = a, pipelineRhs = Parser.FuncCall{funcName = f, funcArgs, ..}}) expectedType = compileExpr (Parser.FuncCall{funcName = f, funcArgs = a : funcArgs, funcPos = zeroPosition}) expectedType
compileExpr (Parser.Pipeline{pipelineLhs = a, pipelineRhs = Parser.Then{thenLhs = b, thenRhs = c}}) expectedType = compileExpr (Parser.Then{thenLhs = Parser.Pipeline{pipelineLhs = a, pipelineRhs = b}, thenRhs = c}) expectedType
compileExpr (Parser.Then{thenLhs = a, thenRhs = b}) expectedType = do
    a' <- compileExpr a expectedType
    b' <- compileExpr b expectedType
    return $ a' ++ b'
compileExpr (Parser.UnaryMinus{unaryMinusExpr = x}) expectedType = compileExpr x expectedType >>= \x' -> return (x' ++ [Push $ DInt (-1), Mul])
compileExpr (Parser.External{externalArgs = [], ..}) _ = return []
compileExpr (Parser.External{externalName = from, externalArgs = (Parser.FuncDec{name, types, ..} : xs)}) expectedType = do
    modify (\s -> s{externals = External{name, returnType = last types, args = init types, from} : externals s})
    _ <- compileExpr (Parser.External{externalName = from, externalArgs = xs}) expectedType
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
    createBaseDef (Parser.FuncDec{name, types = typess, ..}) trait fors = do
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
    let linesOfInput = lines input
        errorLines = map (\(CompilerError msg pos) -> (pos, msg)) errors
     in unlines $
            map
                ( \(AST.Position (start, end), msg) ->
                    let (startLine, startColumn) = getLineAndColumn start linesOfInput
                        (endLine, endColumn) = getLineAndColumn end linesOfInput
                        lineContent =
                            if startLine <= 0 || startLine > length linesOfInput
                                then "Line " ++ show startLine ++ " out of range"
                                else linesOfInput !! (startLine - 1)
                        lineIndicator =
                            replicate startColumn ' '
                                ++ ( if startLine == endLine
                                        then "\x1b[31m  └" ++ replicate (endColumn - startColumn) '─' ++ "┘\x1b[0m"
                                        else "\x1b[31m  └" ++ replicate (length lineContent - startColumn) '─' ++ "┘\x1b[0m"
                                   )
                     in "\x1b[33m"
                            ++ show startLine
                            ++ "\x1b[0m:\x1b[91m "
                            ++ lineContent
                            ++ "\x1b[0m\n"
                            ++ lineIndicator
                            ++ " "
                            ++ "\x1b[31m"
                            ++ msg
                            ++ "\x1b[0m"
                )
                errorLines

getLineAndColumn :: Int -> [String] -> (Int, Int)
getLineAndColumn charPos linesOfInput =
    let go _ _ [] = (0, 0)
        go remaining pos (l : ls)
            | remaining <= length l = (pos, remaining)
            | otherwise = go (remaining - length l - 1) (pos + 1) ls
     in go charPos 1 linesOfInput

compileDry :: Parser.Program -> String -> IO (Either [Instruction] [CompilerError])
compileDry prog sourcePath' =
    liftIO
        ( evalStateT
            (compileProgramBare prog)
            (initCompilerStateWithFile prog sourcePath')
        )

compileFail :: String -> [CompilerError] -> String -> IO ()
compileFail fileName errors file = putStrLn (fileName ++ "\x1b[31m\x1b[0m \n" ++ renderCompilerErrors errors file)
