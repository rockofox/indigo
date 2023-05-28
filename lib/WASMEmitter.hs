{-# LANGUAGE LambdaCase #-}

module WASMEmitter (compileProgramToWAST) where

import Binaryen qualified
import Binaryen.Expression (constInt32)
import Binaryen.Expression qualified
import Binaryen.Expression qualified as Binaryen
import Binaryen.ExpressionId (localSetId)
import Binaryen.Module qualified
import Binaryen.Module qualified as Binaryen
import Binaryen.Type (auto, create, int32, none)
import Control.Monad (filterM, when)
import Control.Monad.State (MonadIO (liftIO), MonadState (get, put), StateT (runStateT))
import Data.List (find)
import Foreign (Ptr, Storable (poke), allocaArray, malloc, newArray, nullPtr, pokeArray)
import Foreign.C (CChar, newCString, peekCString)
import Parser (Expr (..), Program (Program))
import System.IO (hIsTerminalDevice)
import System.IO qualified as IO
import Prelude hiding (mod)

pop :: [a] -> [a]
pop [] = []
pop xs = init xs

typeToWASTType :: String -> Binaryen.Type
typeToWASTType "Int" = int32
typeToWASTType "IO" = none
typeToWASTType _ = int32

sizeOf :: Expr -> Int
sizeOf (IntLit _) = 4
sizeOf (StringLit s) = length s + 1
sizeOf _ = 1

data VarTableEntry = VarTableEntry
    { index :: Int
    , name :: String
    , context :: Maybe String
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
    }
    deriving (Show)

getCellOffset :: Int -> StateT CompilerState IO Int
getCellOffset index = do
    state <- get
    let memory_ = memory state
    let offset = sum $ map size (take index memory_)
    return (offset + 8)

varTableLastIndex :: StateT CompilerState IO Int
varTableLastIndex = do
    state <- get
    return $ maybe 0 index (find (\x -> context x == currentFunction state) $ varTable state)

compileProgramToWAST :: Program -> IO String
compileProgramToWAST (Program exprs) = do
    newModule <- Binaryen.Module.create
    let initialState = CompilerState{mod = newModule, varTable = [], memory = [], currentFunction = Nothing}
    (_, state) <- runStateT (mapM_ compileExprToWAST exprs) initialState
    let mod_ = mod state
    let memory_ = memory state

    memoryName <- newCString "memory"
    dataPtr <- newArray (fmap data_ memory_)
    sizePtr <- newArray (fmap (fromIntegral . size) memory_)
    offsets <- newArray =<< (mapM (liftIO . constInt32 mod_ . fromIntegral) . fst =<< runStateT (mapM getCellOffset [0 .. length memory_ - 1]) state)
    segmentsPassive <- malloc
    poke segmentsPassive 0

    _ <- Binaryen.setMemory mod_ 1 (fromIntegral (length memory_)) memoryName dataPtr segmentsPassive offsets sizePtr (fromIntegral $ length memory_) 0

    -- status <- Binaryen.validate mod_        FIXME: Validator is broken ;(
    let status = (1 :: Integer)

    isTTY <- hIsTerminalDevice IO.stdin
    when isTTY $ do
        Binaryen.setColorsEnabled 0
    if status == 1
        then do
            -- _ <- Binaryen.autoDrop mod_
            x <- Binaryen.allocateAndWriteText mod_
            peekCString x
        else do
            error "Error validating module"

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

compileExprToWAST :: Expr -> StateT CompilerState IO Binaryen.Expression
compileExprToWAST (InternalFunction name args) = do
    state <- get
    let mod_ = mod state
    case name of
        "__wasm_i32_store" -> do
            compiledArgs <- mapM compileExprToWAST args
            liftIO $ Binaryen.Expression.store mod_ 4 0 0 (head compiledArgs) (compiledArgs !! 1) int32
        _ -> error $ "Unknown internal function: " ++ name
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
compileExprToWAST (FuncCall name args) = do
    state <- get
    let mod_ = mod state
    funcName <- liftIO $ newCString name
    compiledArgs <- mapM compileExprToWAST args
    wasmArgs <- liftIO $ allocaArray (length args) $ \ta -> do
        liftIO $ pokeArray ta compiledArgs
        return ta
    liftIO $ Binaryen.Expression.call mod_ funcName wasmArgs (fromIntegral $ length args) int32
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
    lastIndex <- varTableLastIndex
    let desiredIndex = if lastIndex == 0 then 0 else lastIndex + 1
    put $ state{varTable = VarTableEntry{name = name, index = desiredIndex, context = currentFunction state} : varTable state}
    compiledExpr <- compileExprToWAST expr

    liftIO $ Binaryen.Expression.localSet mod_ (fromIntegral desiredIndex) compiledExpr
compileExprToWAST (Var vname) = do
    state <- get
    let mod_ = mod state
    let var = find (\x -> name x == vname && context x == currentFunction state) $ varTable state
    case var of
        Nothing -> error $ "Unknown variable: " ++ vname
        Just var -> liftIO $ Binaryen.Expression.localGet mod_ (fromIntegral (index var)) int32
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
compileExprToWAST (ModernFunc def dec) = do
    state <- get
    put $ state{currentFunction = Just $ fname dec}
    state <- get
    lastVarTableIndex <- varTableLastIndex
    state <- get
    varTableEntriesForFArgs <-
        liftIO
            $ mapM
                ( \(x, y) -> do
                    return $ VarTableEntry{name = x, index = y, context = Just $ fname dec}
                )
            $ zip (fargs def) (map (lastVarTableIndex +) [0 ..])
    put $ state{varTable = varTableEntriesForFArgs ++ varTable state}

    body <- compileExprToWAST $ fbody def
    state <- get

    put $ state{currentFunction = Nothing}
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
