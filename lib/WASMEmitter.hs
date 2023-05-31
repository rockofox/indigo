module WASMEmitter (compileProgramToWAST) where

import Binaryen qualified
import Binaryen.Export (getKind, getValue)
import Binaryen.Export qualified as Export
import Binaryen.Expression (constInt32, refFunc)
import Binaryen.Expression qualified
import Binaryen.Expression qualified as Binaryen
import Binaryen.ExpressionId (localSetId)
import Binaryen.ExternalKind (externalFunction)
import Binaryen.Function (getParams, getResults)
import Binaryen.Module (getExportByIndex, getFunction, getNumExports)
import Binaryen.Module qualified
import Binaryen.Module qualified as Binaryen
import Binaryen.Op qualified as Binaryen
import Binaryen.Type (auto, create, int32, none, funcref, Type)
import Control.Monad (filterM, unless, when)
import Control.Monad.State (MonadIO (liftIO), MonadState (get, put), StateT (runStateT))
import Data.ByteString qualified as BS
import Data.List (elemIndex, find, findIndex)
import Data.Maybe (fromMaybe, isJust)
import Foreign (Ptr, Storable (poke), allocaArray, malloc, newArray, nullPtr, pokeArray)
import Foreign.C (CChar, newCString, peekCString)
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Marshal.Utils (copyBytes)
import Parser ( Expr(..), Program(Program), Type, Type(..) )
import System.Directory ( doesFileExist )
import System.IO (hIsTerminalDevice)
import System.IO qualified as IO
import Prelude hiding (mod)
import Data.Functor ((<&>))
import Debug.Trace (trace, traceShow)

pop :: [a] -> [a]
pop [] = []
pop xs = init xs

typeToWASTType :: Parser.Type -> Binaryen.Type
typeToWASTType Parser.Int = int32
typeToWASTType Parser.IO = none
typeToWASTType Parser.String = int32
typeToWASTType _ = int32

sizeOf :: Expr -> Int
sizeOf (IntLit _) = 4
sizeOf (StringLit s) = length s + 1
sizeOf _ = 1

data VarTableEntry = VarTableEntry
    { name :: String
    , context :: Maybe String
    }
    deriving (Show)

data FunctionTableEntry = FunctionTableEntry
    { ftname :: String
    ,  ftargnames :: [String]
    , ftparams :: [Parser.Type]
    , ftresult :: Parser.Type
    }
    deriving (Show)

data MemoryCell = MemoryCell
    { size :: Int
    , data_ :: Ptr CChar
    }
    deriving (Show)

data CompilerState = CompilerState
    { mod :: Binaryen.Module
    , varTable :: [VarTableEntry]
    , memory :: [MemoryCell]
    , currentFunction :: Maybe String
    , importedModules :: [(String, Binaryen.Module)]
    , functionTable :: [FunctionTableEntry]
    }
    deriving (Show)

getCellOffset :: Int -> StateT CompilerState IO Int
getCellOffset index = do
    state <- get
    let memory_ = memory state
    let offset = sum $ map size (take index memory_)
    return (offset + 8)

findVarIndex :: String -> StateT CompilerState IO (Maybe Int)
findVarIndex name_ = do
    state <- get
    let varTable_ = varTable state
    let context_ = fromMaybe "" $ currentFunction state
    let varsInContext =
            filter
                ( \x -> case context x of
                    Just c -> context_ == c
                    Nothing -> False
                )
                varTable_
    return $ elemIndex name_ (map name varsInContext)

compileProgramToWAST :: Program -> IO String
compileProgramToWAST (Program exprs) = do
    newModule <- Binaryen.Module.create
    let initialState = CompilerState{mod = newModule, varTable = [], memory = [], currentFunction = Nothing, importedModules = [], functionTable = []}
    (_, state) <- runStateT (mapM_ compileExprToWAST exprs) initialState
    let mod_ = mod state
    let memory_ = memory state

    memoryName <- newCString "memory"
    dataPtr <- newArray (fmap data_ memory_)
    sizePtr <- newArray (fmap (fromIntegral . size) memory_)
    offsets <- newArray =<< mapM (liftIO . constInt32 mod_ . fromIntegral) . fst =<< runStateT (mapM getCellOffset [0 .. length memory_ - 1]) state
    segmentsPassive <- malloc
    poke segmentsPassive 0

    _ <- Binaryen.setMemory mod_ 1 (fromIntegral (length memory_)) memoryName dataPtr segmentsPassive offsets sizePtr (fromIntegral $ length memory_) 0

    functionTablePtr <- newArray =<< mapM ((newCString . (++ "\0")) . ftname) (functionTable state)
    offsetExpr <- Binaryen.constInt32 mod_ 0
    _ <- Binaryen.setFunctionTable mod_ (fromIntegral $ length $ functionTable state) (fromIntegral $ length $ functionTable state) functionTablePtr (fromIntegral $ length (functionTable state)) offsetExpr
    -- status <- Binaryen.validate mod_        FIXME: Validator is broken ;(
    let status = (1 :: Integer)

    isTTY <- hIsTerminalDevice IO.stdin
    when isTTY $ Binaryen.setColorsEnabled 0
    if status == 1
        then do
            -- _ <- Binaryen.autoDrop mod_
            x <- Binaryen.allocateAndWriteText mod_
            peekCString x
        else error "Error validating module"

getLocals :: Binaryen.Expression -> IO [Binaryen.Expression]
getLocals expr = do
    numChildren <- Binaryen.Expression.blockGetNumChildren expr
    exprs <- liftIO $ mapM (Binaryen.Expression.blockGetChild expr) [0 .. numChildren - 1]
    filterM
        ( \x -> do
            xid <- Binaryen.getId x
            return $ xid == localSetId
        )
        exprs

readBinaryFileToPtr :: FilePath -> IO (Ptr CChar, Int)
readBinaryFileToPtr filePath = do
    bs <- BS.readFile filePath
    let len = BS.length bs
    ptr <- mallocBytes len
    BS.useAsCString bs $ \cstr -> copyBytes ptr cstr len
    return (ptr, len)

findModule :: String -> String
findModule name = name ++ ".wasm" -- This will, in the future, search the include path for the module

moduleFromFile :: String -> IO Binaryen.Module
moduleFromFile path = do
    exists <- doesFileExist path
    if exists
        then do
            (contents, len) <- readBinaryFileToPtr path
            Binaryen.read contents (fromIntegral len)
        else error $ "File not found: " ++ path

-- Binaryen.Module.create -- FIXME: obviously

unreachable :: a
unreachable = error "Unreachable code reached"

compileExprToWAST :: Expr -> StateT CompilerState IO Binaryen.Expression
compileExprToWAST (InternalFunction name args) = do
    state <- get
    let mod_ = mod state
    case name of
        "__wasm_i32_store" -> do
            compiledArgs <- mapM compileExprToWAST args
            liftIO $ Binaryen.Expression.store mod_ 4 0 0 (head compiledArgs) (compiledArgs !! 1) int32
        "__wasm_i32_load" -> do
            compiledArgs <- mapM compileExprToWAST args
            liftIO $ Binaryen.Expression.load mod_ 4 0 0 0 int32 (head compiledArgs)
        _ -> error $ "Unknown internal function: " ++ name
compileExprToWAST (Import objects source) = do
    state <- get
    let mod_ = mod state
    let isModuleImported = any (\(x, _) -> x == source) (importedModules state)
    unless isModuleImported $ do
        module_ <- liftIO $ moduleFromFile (findModule source)
        put $ state{importedModules = (source, module_) : importedModules state}
    state <- get
    let module_ = fromMaybe (error "Module not imported") $ lookup source (importedModules state)

    moduleNumExports <- liftIO $ getNumExports module_
    exports <- liftIO $ mapM (liftIO . getExportByIndex module_) [0 .. moduleNumExports - 1]

    matchedObjects <-
        mapM
            ( \x -> do
                name <- liftIO $ peekCString =<< Export.getName x
                return (find (name ==) objects, x)
            )
            exports
            >>= liftIO . filterM (pure . isJust . fst) >>= mapM (\(Just x, y) -> return (x, y))

    functions <- liftIO $ filterM (\x -> (==) <$> getKind (snd x) <*> pure externalFunction) matchedObjects >>= mapM (\(_, y) -> (,) <$> Export.getName y <*> (getValue y >>= getFunction module_))
    importedFunctions <- liftIO $ filterM (\(name, _) -> peekCString name <&> flip elem objects) functions

    mapM_
        ( \(name, function) -> do
            params <- liftIO $ getParams function
            results <- liftIO $ getResults function
            sourcePtr <- liftIO $ newCString source
            liftIO $ Binaryen.addFunctionImport mod_ name sourcePtr name params results
        )
        importedFunctions

    liftIO $ Binaryen.nop mod_
compileExprToWAST (ExternDec lang name types) = do
    state <- get
    let mod_ = mod state
    let types_ = pop $ map typeToWASTType types
    let ret = last $ map typeToWASTType types
    funcName <- liftIO $ newCString name
    moduleName <- liftIO $ newCString lang
    externalName <- liftIO $ newCString name
    x <- liftIO $ allocaArray (length types_) $ \ta -> do
        liftIO $ pokeArray ta types_
        return ta
    type_ <- liftIO $ Binaryen.Type.create x (fromIntegral $ length types_)
    liftIO $ Binaryen.addFunctionImport mod_ funcName moduleName externalName type_ ret
    liftIO $ Binaryen.Expression.nop mod_
compileExprToWAST (If cond true false) = do
    state <- get
    let mod_ = mod state
    compiledCond <- compileExprToWAST cond
    compiledTrue <- compileExprToWAST true
    compiledFalse <- compileExprToWAST false
    liftIO $ Binaryen.Expression.if_ mod_ compiledCond compiledTrue compiledFalse
compileExprToWAST (Eq a b) = do
    state <- get
    let mod_ = mod state
    compiledA <- compileExprToWAST a
    compiledB <- compileExprToWAST b
    liftIO $ Binaryen.binary mod_ Binaryen.eqInt32 compiledA compiledB
compileExprToWAST (Neq a b) = do
    state <- get
    let mod_ = mod state
    compiledA <- compileExprToWAST a
    compiledB <- compileExprToWAST b
    liftIO $ Binaryen.binary mod_ Binaryen.neInt32 compiledA compiledB
compileExprToWAST (Add a b) = do
    state <- get
    let mod_ = mod state
    compiledA <- compileExprToWAST a
    compiledB <- compileExprToWAST b
    liftIO $ Binaryen.binary mod_ Binaryen.addInt32 compiledA compiledB
compileExprToWAST (Sub a b) = do
    state <- get
    let mod_ = mod state
    compiledA <- compileExprToWAST a
    compiledB <- compileExprToWAST b
    liftIO $ Binaryen.binary mod_ Binaryen.subInt32 compiledA compiledB
compileExprToWAST (Mul a b) = do
    state <- get
    let mod_ = mod state
    compiledA <- compileExprToWAST a
    compiledB <- compileExprToWAST b
    liftIO $ Binaryen.binary mod_ Binaryen.mulInt32 compiledA compiledB
compileExprToWAST (Div a b) = do
    state <- get
    let mod_ = mod state
    compiledA <- compileExprToWAST a
    compiledB <- compileExprToWAST b
    liftIO $ Binaryen.binary mod_ Binaryen.divSInt32 compiledA compiledB
compileExprToWAST (FuncCall fname args) = do
    state <- get
    let parentVarTable = filter (\x -> context x == currentFunction state) (varTable state)
    let fn = fromMaybe unreachable $ find (\x -> ftname x == fromMaybe unreachable (currentFunction state)) (functionTable state)
    let mod_ = mod state
    funcName <- liftIO $ newCString fname
    compiledArgs <- mapM compileExprToWAST args
    wasmArgs <- liftIO $ allocaArray (length args) $ \ta -> do
        liftIO $ pokeArray ta compiledArgs
        return ta
    if fname `notElem` map name parentVarTable then
        liftIO $ Binaryen.Expression.call mod_ funcName wasmArgs (fromIntegral $ length args) int32
    else do
        varIndex <- findVarIndex fname
        case varIndex of
            Nothing -> error $ "Variable " ++ fname ++ " not found"
            Just varIndex -> do
                let argIndex = fromMaybe unreachable $ elemIndex fname (ftargnames fn)
                let fnType = ftparams fn !! argIndex
                case fnType of
                    (Fn args ret) -> do
                        tableIndexConst <- liftIO $ Binaryen.localGet mod_ (fromIntegral varIndex) int32
                        let types_ = map typeToWASTType args
                        typePtr <- liftIO $ allocaArray (length types_) $ \ta -> do
                            liftIO $ pokeArray ta types_
                            return ta
                        type_ <- liftIO $ Binaryen.Type.create typePtr (fromIntegral $ length types_)
                        liftIO $ Binaryen.Expression.callIndirect mod_ tableIndexConst wasmArgs (fromIntegral $ length args) type_ (typeToWASTType ret)
                    _ -> unreachable
compileExprToWAST (IntLit int) = do
    state <- get
    let mod_ = mod state
    liftIO $ Binaryen.Expression.constInt32 mod_ (fromIntegral int)
compileExprToWAST (StringLit str) = do
    state <- get
    cStr <- liftIO $ newCString str
    put $ state{memory = MemoryCell{size = length str, data_ = cStr} : memory state}
    m <- getCellOffset $ length $ memory state
    let mod_ = mod state

    liftIO $ Binaryen.constInt32 mod_ (fromIntegral m)
compileExprToWAST (Let name expr) = do
    state <- get
    let mod_ = mod state
    put $ state{varTable = varTable state ++ [VarTableEntry{name = name, context = currentFunction state}]}
    state <- get
    let desiredIndex = length (filter (\x -> context x == currentFunction state) $ varTable state) - 1
    compiledExpr <- compileExprToWAST expr
    liftIO $ Binaryen.Expression.localSet mod_ (fromIntegral desiredIndex) compiledExpr
compileExprToWAST (Var vname) = do
    state <- get
    let mod_ = mod state
    varIndex <- findVarIndex vname
    case varIndex of
        Nothing -> error $ "Unknown variable: " ++ vname
        Just varIndex -> liftIO $ Binaryen.Expression.localGet mod_ (fromIntegral varIndex) int32
compileExprToWAST (DoBlock exprs) = do
    state <- get
    let mod_ = mod state
    compiledExprs <- mapM compileExprToWAST exprs
    wasmExprs <- liftIO $ allocaArray (length exprs) $ \ta -> do
        liftIO $ pokeArray ta compiledExprs
        return ta
    liftIO $ Binaryen.Expression.block mod_ nullPtr wasmExprs (fromIntegral $ length exprs) auto
compileExprToWAST (Discard expr) = do
    state <- get
    let mod_ = mod state
    compiledExpr <- compileExprToWAST expr
    type_ <- liftIO $ Binaryen.getType compiledExpr
    liftIO $ Binaryen.drop mod_ compiledExpr
compileExprToWAST (Ref expr) = do
    state <- get
    let mod_ = mod state
    case expr of
        FuncCall name args -> do
            namePtr <- liftIO $ newCString name
            let functionIndex = elemIndex name $ map ftname $ functionTable state
            liftIO $ constInt32 mod_ (maybe unreachable fromIntegral functionIndex)
        _ -> error "Can only take reference of function call"
compileExprToWAST (ModernFunc def dec) = do
    state <- get
    put $ state{currentFunction = Just $ fname dec}
    state <- get
    varTableEntriesForFArgs <-
        liftIO
            $ mapM
                ( \x -> return $ VarTableEntry{name = x, context = Just $ fname dec}
                )
            $ fargs def

    put $ state{varTable = varTable state ++ varTableEntriesForFArgs}
    state <- get

    state <- get
    -- put $ state{ currentFunction = Nothing}
    state <- get
    let mod_ = mod state

    funcName <- liftIO $ newCString $ fname dec
    let types = map typeToWASTType $ ftypes dec -- TODO: Look at this
    let types_ = tail types
    let ret = if null types then Binaryen.Type.none else head types
    -- ret <- liftIO $ Binaryen.getType body
    typePtr <- liftIO $ allocaArray (length types_) $ \ta -> do
        liftIO $ pokeArray ta types_
        return ta
    type_ <- liftIO $ Binaryen.Type.create typePtr (fromIntegral $ length types_)
    state <- get
    put $ state{functionTable = functionTable state ++ [FunctionTableEntry { ftname = fname dec, ftparams = tail (ftypes dec), ftresult = if null types then head (ftypes dec) else Parser.IO, ftargnames = fargs def }]}
    body <- compileExprToWAST $ fbody def

    bodyLocals <- liftIO $ getLocals body
    localsValues <- liftIO $ mapM Binaryen.localSetGetValue bodyLocals
    localsTypes <- liftIO $ mapM Binaryen.getType localsValues

    localsTypesArr <- liftIO $ allocaArray (length localsTypes) $ \ta -> do
        liftIO $ pokeArray ta localsTypes
        return ta

    fun <- liftIO $ Binaryen.addFunction mod_ funcName type_ ret localsTypesArr (fromIntegral (length bodyLocals)) body
    if fname dec == "main"
        then do
            _ <- liftIO $ Binaryen.setStart mod_ fun
            liftIO $ Binaryen.Expression.nop mod_
        else liftIO $ Binaryen.Expression.nop mod_
compileExprToWAST _ = do
    state <- get
    let mod_ = mod state
    liftIO $ Binaryen.Expression.nop mod_
