module BytecodeCompiler where

import AST (zeroPosition)
import AST qualified as Parser.Type (Type (Unknown))
import Control.Monad (when, (>=>))
import Control.Monad.Loops (firstM)
import Control.Monad.State (MonadIO (liftIO), StateT, gets, modify)
import Data.Bifunctor (second)
import Data.Functor ((<&>))
import Data.List (elemIndex, find, inits, intercalate)
import Data.List.Split qualified
import Data.Map qualified
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Text (isPrefixOf, splitOn)
import Data.Text qualified
import Data.Text qualified as T
import Foreign (nullPtr, ptrToWordPtr)
import Foreign.C.Types ()
import GHC.Generics (Generic)
import Parser (CompilerFlags (CompilerFlags), name, parseProgram, types)
import Parser qualified
import Paths_indigo qualified
import System.Directory (doesFileExist)
import System.Environment
import System.FilePath
import Text.Megaparsec (errorBundlePretty)
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
    }
    deriving (Show)

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
        Right (Parser.Program progExpr) -> return $ progExpr ++ [Parser.FuncDef "__sep" [] Parser.Placeholder]

compileProgram :: Parser.Program -> StateT (CompilerState a) IO [Instruction]
compileProgram (Parser.Program expr) = do
    prelude <- liftIO preludeExpr
    prelude' <- concatMapM compileExpr prelude
    freePart <- concatMapM compileExpr expr
    createVirtualFunctions
    functions' <- gets functions >>= \x -> return $ concatMap function (reverse x)
    return $ prelude' ++ functions' ++ freePart ++ [Exit]

compileProgramBare :: Parser.Program -> StateT (CompilerState a) IO [Instruction]
compileProgramBare (Parser.Program expr) = do
    freePart <- concatMapM compileExpr expr
    functions' <- gets functions >>= \x -> return $ concatMap function (reverse x)
    return $ functions' ++ freePart ++ [Exit]

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

unmangleFunctionName :: String -> String
unmangleFunctionName = takeWhile (/= '#')

findSourceFile :: String -> IO String
findSourceFile fileName = do
    dataFile <- Paths_indigo.getDataFileName fileName
    executablePathFile <- getExecutablePath >>= \x -> return $ takeDirectory x </> fileName
    let pathsToTry = map (</> fileName) ["/usr/local/lib/indigo/", "/usr/lib/indigo/"] ++ [executablePathFile, dataFile]
    firstThatExists <- firstM doesFileExist pathsToTry
    case firstThatExists of
        Just x -> return x
        Nothing -> error $ "Source file " ++ fileName ++ " not found. Tried:\n* " ++ intercalate "\n* " pathsToTry

preludeFile :: IO String
preludeFile = findSourceFile "std/prelude.in" >>= readFile

doBinOp :: Parser.Expr -> Parser.Expr -> [Instruction] -> StateT (CompilerState a) IO [Instruction]
doBinOp x y op = do
    id' <- allocId
    functions' <- gets functions
    let f = findAnyFunction (Data.Text.unpack $ Data.Text.toLower $ Data.Text.pack $ show op) functions'
    -- let aName = "__op_a_" ++ show id'
    -- let bName = "__op_b_" ++ show id'
    x' <- compileExpr x
    y' <- compileExpr y

    cast <- case (x, y) of
        (Parser.Flexible _, Parser.Flexible _) -> error "Double cast"
        (Parser.Flexible _, _) -> return $ Cast : y'
        (_, Parser.Flexible _) -> return $ [Swp, Cast] ++ x'
        _ -> return []
    case (x, y) of
        (Parser.Placeholder, Parser.Placeholder) -> return [PushPf (funame $ fromJust f) 0]
        (Parser.Placeholder, _) -> return $ y' ++ [PushPf (funame $ fromJust f) 1]
        (_, Parser.Placeholder) -> return $ x' ++ [PushPf (funame $ fromJust f) 1]
        -- _ -> return (x' ++ LStore aName : y' ++ [LStore bName, LLoad aName, LLoad bName] ++ cast ++ [op])
        _ -> return (x' ++ MovReg id' : y' ++ MovReg (id' + 1) : [PushReg id', PushReg (id' + 1)] ++ cast ++ op)

typeOf :: Parser.Expr -> StateT (CompilerState a) IO Parser.Type
typeOf (Parser.FuncCall funcName _ _) = do
    funcDecs' <- gets funcDecs
    let a =
            maybe
                [Parser.Type.Unknown]
                Parser.types
                (find (\x -> x.name == funcName) funcDecs')
    return $ last a
typeOf (Parser.Var varName _) = do
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
typeToData _ = VM.DNone

compileExpr :: Parser.Expr -> StateT (CompilerState a) IO [Instruction]
compileExpr (Parser.Add x y) = compileExpr (Parser.FuncCall "+" [x, y] zeroPosition) >>= doBinOp x y
compileExpr (Parser.Sub x y) = compileExpr (Parser.FuncCall "-" [x, y] zeroPosition) >>= doBinOp x y
compileExpr (Parser.Mul x y) = compileExpr (Parser.FuncCall "*" [x, y] zeroPosition) >>= doBinOp x y
compileExpr (Parser.Div x y) = compileExpr (Parser.FuncCall "/" [x, y] zeroPosition) >>= doBinOp x y
compileExpr (Parser.Modulo x y) = compileExpr (Parser.FuncCall "%" [x, y] zeroPosition) >>= doBinOp x y
compileExpr (Parser.Power x y) = compileExpr (Parser.FuncCall "^" [x, y] zeroPosition) >>= doBinOp x y
compileExpr (Parser.Gt x y) = compileExpr (Parser.FuncCall ">" [x, y] zeroPosition) >>= doBinOp x y
compileExpr (Parser.Lt x y) = compileExpr (Parser.FuncCall "<" [x, y] zeroPosition) >>= doBinOp x y
compileExpr (Parser.Ge x y) = compileExpr (Parser.FuncCall ">=" [x, y] zeroPosition) >>= doBinOp x y
compileExpr (Parser.Le x y) = compileExpr (Parser.FuncCall "<=" [x, y] zeroPosition) >>= doBinOp x y
compileExpr (Parser.Not x) = compileExpr x >>= \x' -> return (x' ++ [Not])
compileExpr (Parser.Eq x y) = compileExpr (Parser.FuncCall "==" [x, y] zeroPosition) >>= doBinOp x y
compileExpr (Parser.Neq x y) = compileExpr (Parser.FuncCall "!=" [x, y] zeroPosition) >>= doBinOp x y
compileExpr (Parser.And x y) = compileExpr (Parser.FuncCall "&&" [x, y] zeroPosition) >>= doBinOp x y
compileExpr (Parser.Or x y) = compileExpr (Parser.FuncCall "||" [x, y] zeroPosition) >>= doBinOp x y
compileExpr (Parser.IntLit x) = return [Push $ DInt $ fromIntegral x]
compileExpr (Parser.UnaryMinus (Parser.FloatLit x)) = return [Push $ DFloat (-x)]
compileExpr (Parser.UnaryMinus (Parser.IntLit x)) = return [Push $ DInt $ -fromInteger x]
compileExpr (Parser.StringLit x) = return [Push $ DString x]
compileExpr (Parser.DoBlock exprs) = concatMapM compileExpr exprs
compileExpr Parser.Placeholder = return []
compileExpr (Parser.FuncCall "unsafeAdd" [x, y] _) = compileExpr x >>= \x' -> compileExpr y >>= \y' -> return (x' ++ y' ++ [Add])
compileExpr (Parser.FuncCall "unsafeSub" [x, y] _) = compileExpr x >>= \x' -> compileExpr y >>= \y' -> return (x' ++ y' ++ [Sub])
compileExpr (Parser.FuncCall "unsafeMul" [x, y] _) = compileExpr x >>= \x' -> compileExpr y >>= \y' -> return (x' ++ y' ++ [Mul])
compileExpr (Parser.FuncCall "unsafeDiv" [x, y] _) = compileExpr x >>= \x' -> compileExpr y >>= \y' -> return (x' ++ y' ++ [Div])
compileExpr (Parser.FuncCall "unsafeMod" [x, y] _) = compileExpr x >>= \x' -> compileExpr y >>= \y' -> return (x' ++ y' ++ [Mod])
compileExpr (Parser.FuncCall "unsafePow" [x, y] _) = compileExpr x >>= \x' -> compileExpr y >>= \y' -> return (x' ++ y' ++ [Pow])
compileExpr (Parser.FuncCall "unsafeGt" [x, y] _) = compileExpr x >>= \x' -> compileExpr y >>= \y' -> return (x' ++ y' ++ [Gt])
compileExpr (Parser.FuncCall "unsafeLt" [x, y] _) = compileExpr x >>= \x' -> compileExpr y >>= \y' -> return (x' ++ y' ++ [Lt])
compileExpr (Parser.FuncCall "unsafeGe" [x, y] _) = compileExpr x >>= \x' -> compileExpr y >>= \y' -> return (x' ++ y' ++ [Lt, Not])
compileExpr (Parser.FuncCall "unsafeLe" [x, y] _) = compileExpr x >>= \x' -> compileExpr y >>= \y' -> return (x' ++ y' ++ [Gt, Not])
compileExpr (Parser.FuncCall "unsafeEq" [x, y] _) = compileExpr x >>= \x' -> compileExpr y >>= \y' -> return (x' ++ y' ++ [Eq])
compileExpr (Parser.FuncCall "unsafeNeq" [x, y] _) = compileExpr x >>= \x' -> compileExpr y >>= \y' -> return (x' ++ y' ++ [Eq, Not])
compileExpr (Parser.FuncCall "unsafeAnd" [x, y] _) = compileExpr x >>= \x' -> compileExpr y >>= \y' -> return (x' ++ y' ++ [And])
compileExpr (Parser.FuncCall "unsafeOr" [x, y] _) = compileExpr x >>= \x' -> compileExpr y >>= \y' -> return (x' ++ y' ++ [Or])
compileExpr (Parser.FuncCall "unsafePrint" [x] _) = compileExpr x >>= \x' -> return (x' ++ [Builtin Print])
compileExpr (Parser.FuncCall "unsafeGetLine" _ _) = return [Builtin GetLine]
compileExpr (Parser.FuncCall "unsafeGetChar" _ _) = return [Builtin GetChar]
compileExpr (Parser.FuncCall "unsafeRandom" _ _) = return [Builtin Random]
compileExpr (Parser.FuncCall "abs" [x] _) = compileExpr x >>= \x' -> return (x' ++ [Abs])
compileExpr (Parser.FuncCall "root" [x, Parser.FloatLit y] _) = compileExpr x >>= \x' -> return (x' ++ [Push $ DFloat (1.0 / y), Pow])
compileExpr (Parser.FuncCall "sqrt" [x] _) = compileExpr x >>= \x' -> return (x' ++ [Push $ DFloat 0.5, Pow])
compileExpr (Parser.FuncCall funcName args _) = do
    argTypes <- mapM typeOf args
    functions' <- gets functions
    funcDecs' <- gets funcDecs
    curCon <- gets currentContext
    externals' <- gets externals
    let contexts = map (T.pack . intercalate "@") (inits (Data.List.Split.splitOn "@" curCon))
    -- Find the function in any context using firstM
    let contextFunctions = firstJust (\context -> findFunction (Data.Text.unpack context ++ "@" ++ funcName) functions' argTypes) contexts

    let fun = case contextFunctions of
            (Just lf) -> lf
            Nothing -> case findFunction funcName functions' argTypes of
                (Just f) -> f
                Nothing -> Function{baseName = unmangleFunctionName funcName, funame = funcName, function = [], types = [], context = "__outside"}
    let funcDec = find (\(Parser.FuncDec name' _ _) -> name' == baseName fun) funcDecs'
    let external = find (\x -> x.name == funcName) externals'
    -- If the funcName starts with curCon@, it's a local function
    let callWay = (if T.pack (curCon ++ "@") `isPrefixOf` T.pack fun.funame then CallLocal (funame fun) else Call (funame fun))

    case external of
        Just (External _ ereturnType _ from) -> do
            retT <- case typeToData ereturnType of
                DMap _ -> do
                    fields <- getStructFields from
                    return $ DMap $ Data.Map.fromList $ map (second typeToData) fields
                _ -> return $ typeToData ereturnType
            args' <- concatMapM compileExpr (reverse args)
            return $ [Push retT] ++ args' ++ [CallFFI funcName from (length args)]
        Nothing ->
            case funcDec of
                (Just fd) -> do
                    if length args == length (Parser.types fd) - 1
                        then concatMapM compileExpr args >>= \args' -> return (args' ++ [PragmaMethodTypes (map typeToData argTypes)] ++ [callWay])
                        else
                            concatMapM compileExpr args >>= \args' ->
                                return $
                                    args'
                                        ++ [PushPf (funame fun) (length args')]
                Nothing ->
                    concatMapM compileExpr args >>= \args' ->
                        return $
                            args'
                                ++ [ LLoad funcName
                                   , CallS
                                   ]
compileExpr fd@(Parser.FuncDec{}) = do
    modify (\s -> s{funcDecs = fd : funcDecs s})
    return [] -- Function declarations are only used for compilation
compileExpr (Parser.FuncDef origName args body) = do
    curCon <- gets currentContext
    let previousContext = curCon
    let name = if curCon /= "__outside" then curCon ++ "@" ++ origName else origName
    -- let argTypes = map Parser.typeOf args
    modify (\s -> s{currentContext = name})
    funcDecs' <- gets funcDecs
    when (isNothing $ find (\(Parser.FuncDec name' _ _) -> name' == name) funcDecs') $ modify (\s -> s{funcDecs = Parser.FuncDec name (replicate (length args + 1) Parser.Any) [] : funcDecs s})

    funame <- if name /= "main" then ((name ++ "#") ++) . show <$> allocId else return "main"
    modify (\s -> s{functions = Function name funame [] [] curCon : functions s})

    body' <- compileExpr body
    funcDecs'' <- gets funcDecs
    args' <- concatMapM (`compileParameter` name) (reverse (filter (/= Parser.Placeholder) args))
    let funcDec = fromJust $ find (\(Parser.FuncDec name' _ _) -> name' == name) funcDecs''
    let function = Label funame : PragmaMethodTypes (map typeToData (init funcDec.types)) : args' ++ body' ++ ([Ret | name /= "main"]) -- TODO: Put in actual types
    -- modify (\s -> s{functions = Function name funame function funcDec.types : tail (functions s)})
    modify (\s -> s{functions = Function name funame function funcDec.types curCon : functions s})
    modify (\s -> s{currentContext = previousContext})
    return [] -- Function definitions get appended at the last stage of compilation
  where
    compileParameter :: Parser.Expr -> String -> StateT (CompilerState a) IO [Instruction]
    compileParameter (Parser.Var varName _) _ = return [LStore varName]
    compileParameter lex@(Parser.ListLit l) funcName = do
        nextFunName <- ((funcName ++ "#") ++) . show . (+ 1) <$> allocId
        if null l
            then return [Dup, Push $ DList [], Eq, Jf nextFunName, Pop]
            else do
                lex' <- compileExpr lex
                return $ lex' ++ [Eq, Jf nextFunName] -- TODO: Check if this works
    compileParameter (Parser.ListPattern elements) n = do
        nextFunName <- ((n ++ "#") ++) . show . (+ 1) <$> allocId
        let lengthCheck = [Dup, Length, Push $ DInt $ fromIntegral $ length elements - 1, Lt, StackLength, Push $ DInt 1, Neq, And, Jt nextFunName]
        case last elements of
            Parser.ListLit l -> do
                elements' <- concatMapM (`compileParameter` n) (init elements)
                let paramsWithIndex = zip elements' [0 ..]
                let xToY = map (\(x, index) -> [Dup, Push $ DInt index, Index, x]) paramsWithIndex
                l' <- compileExpr (Parser.ListLit l)
                let listThing = [Comment "List thing", Dup, Push $ DInt (length elements - 1), Push DNone, Slice] ++ l' ++ [Eq, Jf nextFunName]
                return $ lengthCheck ++ concat xToY ++ listThing
            _ -> do
                elements' <- concatMapM (`compileParameter` n) elements
                let paramsWithIndex = zip elements' [0 ..]
                let xToY = map (\(x, index) -> [Dup, Push $ DInt index, Index, x]) paramsWithIndex
                let rest = [Push $ DInt (length elements - 1), Push DNone, Slice, last elements']
                return $ lengthCheck ++ concat xToY ++ rest
    compileParameter (Parser.IntLit x) funcName = do
        -- TODO: fix this
        nextFunName <- ((funcName ++ "#") ++) . show . (+ 1) <$> allocId
        return [Dup, Push $ DInt $ fromIntegral x, Eq, Jf nextFunName]
    compileParameter Parser.Placeholder _ = return []
    compileParameter x _ = error $ show x ++ ": not implemented as a function parameter"
compileExpr (Parser.Var x _) = do
    functions' <- gets functions
    curCon <- gets currentContext
    externals' <- gets externals

    let fun = any ((== x) . baseName) functions' || any ((== curCon ++ "@" ++ x) . baseName) functions'
    if fun || x `elem` internalFunctions || x `elem` map (\f -> f.name) externals'
        then compileExpr (Parser.FuncCall x [] zeroPosition)
        else return [LLoad x]
compileExpr (Parser.Let name value) = do
    curCon <- gets currentContext
    typeOf value >>= \v -> modify (\s -> s{lets = Let{name, vtype = v, context = curCon} : lets s})
    value' <- compileExpr value
    return $ value' ++ [LStore name]
compileExpr (Parser.Function [Parser.FuncDef{body = Parser.StrictEval e}] dec) = do
    let name = dec.name
    evaledExpr <- compileExpr e
    return $ evaledExpr ++ [LStore name]
compileExpr (Parser.Function a b) = mapM_ compileExpr a >> compileExpr b >> return []
compileExpr (Parser.Flexible a) = compileExpr a >>= \a' -> return $ Meta "flex" : a'
compileExpr (Parser.ListConcat a b) = do
    a' <- compileExpr a
    b' <- compileExpr b
    let a'' = case a' of
            [Meta "flex", a'''] -> [a'''] ++ b' ++ [Cast]
            _ -> a'
    let b'' = case b' of
            [Meta "flex", b'''] -> [b'''] ++ a' ++ [Cast]
            _ -> b'
    return (b'' ++ a'' ++ [Concat 2])
compileExpr (Parser.ListLit elements) = concatMapM compileExpr elements >>= \elements' -> return (reverse elements' ++ [PackList $ length elements])
compileExpr (Parser.If ifCond ifThen ifElse) = do
    cond' <- compileExpr ifCond
    then' <- compileExpr ifThen
    else' <- compileExpr ifElse
    elseLabel <- allocId >>= \x -> return $ "else" ++ show x
    endLabel <- allocId >>= \x -> return $ "end" ++ show x
    return $ cond' ++ [Jf elseLabel] ++ then' ++ [Jmp endLabel, Label elseLabel] ++ else' ++ [Label endLabel]
compileExpr (Parser.FloatLit x) = return [Push (DFloat x)]
compileExpr (Parser.DoubleLit x) = return [Push (DDouble x)]
compileExpr (Parser.BoolLit x) = return [Push (DBool x)]
compileExpr (Parser.Cast from to) = do
    from' <- compileExpr from
    let to' = compileType to
    return $ from' ++ to' ++ [Cast]
  where
    compileType :: Parser.Expr -> [Instruction]
    compileType (Parser.Var "Int" _) = [Push $ DInt 0]
    compileType (Parser.Var "Float" _) = [Push $ DFloat 0.0]
    compileType (Parser.Var "Double" _) = [Push $ DDouble 0.0]
    compileType (Parser.Var "Bool" _) = [Push $ DBool False]
    compileType (Parser.Var "Char" _) = [Push $ DChar '\0']
    compileType (Parser.Var "String" _) = [Push $ DString ""]
    compileType (Parser.Var "CPtr" _) = [Push $ DCPtr $ ptrToWordPtr nullPtr]
    compileType (Parser.ListLit [x]) = compileType x ++ [PackList 1]
    compileType x = error $ "Type " ++ show x ++ " is not implemented"
compileExpr st@(Parser.Struct{name = structName, fields, is}) = do
    modify (\s -> s{structDecs = st : structDecs s})
    mapM_ createFieldTrait fields
    mapM_ (compileExpr . (\t -> Parser.Impl{methods = [], for = structName, trait = t})) is
    return []
  where
    createFieldTrait :: (String, Parser.Type) -> StateT (CompilerState a) IO ()
    createFieldTrait (name, _) = do
        let traitName = "__field_" ++ name
        let trait = Parser.Trait traitName [Parser.FuncDec{Parser.name = name, Parser.types = [Parser.Self, Parser.Any], Parser.generics = []}]
        let impl = Parser.Impl traitName (Parser.name st) [Parser.FuncDef{name = name, args = [Parser.Var "self" zeroPosition], body = Parser.StructAccess (Parser.Var "self" zeroPosition) (Parser.Var name zeroPosition)}]
        _ <- compileExpr trait
        _ <- compileExpr impl
        return ()
compileExpr (Parser.StructLit name fields _) = do
    fields' <- mapM (compileExpr . snd) fields
    let names = map (DString . fst) fields
    let instructions = zip names fields' >>= \(name', field) -> field ++ [Push name']
    implsForStruct <- implsFor name
    return $ instructions ++ [Push $ DString name, Push $ DString "__name", Push $ DList (map DString implsForStruct), Push $ DString "__traits", PackMap $ length fields * 2 + 4]
compileExpr (Parser.StructAccess struct (Parser.Var field _)) = do
    struct' <- compileExpr struct
    return $ struct' ++ [Access field]
compileExpr (Parser.Import{objects = o, from = from, as = as, qualified = qualified}) = do
    when (o /= ["*"]) $ error "Only * imports are supported right now"
    let convertedPath = map (\x -> if x == '@' then '/' else x) from
    i <- liftIO $ readFile $ convertedPath ++ ".in"
    let expr = case parseProgram (T.pack i) Parser.initCompilerFlags of -- FIXME: pass on flags
            Left err -> error $ "Parse error: " ++ errorBundlePretty err
            Right (Parser.Program exprs) -> exprs
    let p =
            if qualified || isJust as
                then do
                    let alias = if qualified then from else fromJust as
                    concatMapM compileExpr (map (`mangleAST` alias) expr)
                else concatMapM compileExpr expr
    p >>= \p' -> return $ p' ++ [Label "__sep"]
  where
    mangleAST :: Parser.Expr -> String -> Parser.Expr
    mangleAST (Parser.FuncDec name types _) alias = Parser.FuncDec (alias ++ "@" ++ name) types []
    mangleAST (Parser.Function fdef dec) alias = Parser.Function (map (`mangleAST` alias) fdef) (mangleAST dec alias)
    mangleAST (Parser.FuncDef name args body) alias = Parser.FuncDef (alias ++ "@" ++ name) args (mangleAST body alias)
    mangleAST x _ = x
compileExpr (Parser.Trait name methods) = do
    let methods' = map (\(Parser.FuncDec name' types _) -> Parser.FuncDec name' types []) methods
    modify (\s -> s{traits = Parser.Trait name methods : traits s})
    mapM_ compileExpr methods'
    return []
compileExpr (Parser.Impl name for methods) = do
    let methods' = map (\(Parser.FuncDef name' args body) -> Parser.FuncDef (name ++ "." ++ for ++ "::" ++ name') args body) methods
    modify (\s -> s{impls = Parser.Impl name for methods : impls s})
    mapM_ compileExpr methods'
    return []
compileExpr (Parser.Lambda args body) = do
    fId <- allocId
    curCon <- gets currentContext
    let name = "__lambda" ++ show fId
    let def = Parser.FuncDef name args body
    let dec = Parser.FuncDec name (replicate (length args + 1) Parser.Any) []
    let fun = Parser.Function [def] dec
    _ <- compileExpr fun
    lets' <- gets functions
    let fullName = (fromJust $ findAnyFunction (curCon ++ "@" ++ name) lets').funame
    return [PushPf fullName 0]
compileExpr (Parser.Pipeline a (Parser.Var b _)) = compileExpr (Parser.FuncCall b [a] zeroPosition)
compileExpr (Parser.Pipeline a (Parser.FuncCall f args _)) = compileExpr (Parser.FuncCall f (a : args) zeroPosition)
compileExpr (Parser.Pipeline a (Parser.Then b c)) = compileExpr (Parser.Then (Parser.Pipeline a b) c)
compileExpr (Parser.Then a b) = do
    a' <- compileExpr a
    b' <- compileExpr b
    return $ a' ++ b'
compileExpr (Parser.UnaryMinus x) = compileExpr x >>= \x' -> return (x' ++ [Push $ DInt (-1), Mul])
compileExpr (Parser.External _ []) = return []
compileExpr (Parser.External from ((Parser.FuncDec name types _) : xs)) = do
    modify (\s -> s{externals = External{name, returnType = last types, args = init types, from} : externals s})
    _ <- compileExpr (Parser.External from xs)
    return []
compileExpr (Parser.CharLit x) = return [Push $ DChar x]
compileExpr x = error $ show x ++ " is not implemented"

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
    createBaseDef (Parser.FuncDec name typess _) trait fors = do
        let traitName = Parser.name trait
        let body =
                Label name
                    : concatMap (\for -> [LStow (length typess - 2) "__ts", Dup, Push $ DTypeQuery for, TypeEq, LStore "__ta", LUnstow "__ts", LLoad "__ta", Jt (traitName ++ "." ++ for ++ "::" ++ name)]) fors
        modify (\s -> s{functions = functions s ++ [Function name (name ++ "#0") body typess "__outside"]})
        return ()
    createBaseDef _ _ _ = return ()

locateLabel :: [Instruction] -> String -> Int
locateLabel program label = do
    let x = elemIndex (Label label) program
    case x of
        Just x' -> x'
        Nothing -> error $ "Label " ++ label ++ " not found"
