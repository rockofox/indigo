{-# LANGUAGE LambdaCase #-}

module WASMEmitter (compileProgramToWAST) where

import Binaryen qualified
import Binaryen.Expression (constInt32)
import Binaryen.Expression qualified
import Binaryen.Expression qualified as Binaryen
import Binaryen.Module qualified
import Binaryen.Module qualified as Binaryen
import Binaryen.Type (auto, create, int32, none)
import Control.Monad (void)
import Control.Monad.State (MonadIO (liftIO), MonadState (get, put), StateT (runStateT))
import Debug.Trace (trace)
import Foreign (Storable (poke), allocaArray, malloc, newArray, nullPtr, pokeArray)
import Foreign.C (newCString, peekCString)
import Parser (Expr (..), Program (Program), integer)
import Prelude hiding (mod)

pop :: [a] -> [a]
pop [] = []
pop xs = init xs

typeToWASTType :: String -> Binaryen.Type
typeToWASTType "Int" = int32
typeToWASTType _ = int32

sizeOf :: Expr -> Int
sizeOf (IntLit _) = 4
sizeOf (StringLit s) = length s + 1

data CompilerState = CompilerState
    { mod :: Binaryen.Module
    , lastDataOffset :: Int
    , varTable :: [(String, Int)]
    }

compileProgramToWAST :: Program -> IO String
compileProgramToWAST (Program exprs) = do
    newModule <- Binaryen.Module.create
    let initialState = CompilerState{mod = newModule, lastDataOffset = 0, varTable = []}
    (_, state) <- runStateT (mapM_ compileExprToWAST exprs) initialState
    let mod_ = mod state

    status <- Binaryen.validate mod_
    if status == 0
        then do
            _ <- Binaryen.autoDrop mod_
            x <- Binaryen.allocateAndWriteText mod_
            peekCString x
        else do
            error "Error validating module"

alloc :: Int -> StateT CompilerState IO Int
alloc size = do
    state <- get
    let lastDataOffset_ = lastDataOffset state
    put $ state{lastDataOffset = lastDataOffset_ + size}
    trace ("Allocating " ++ show size ++ " bytes at " ++ show lastDataOffset_) $ return ()
    return lastDataOffset_

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
compileExprToWAST s@(StringLit str) = do
    m <- alloc (sizeOf s)
    state <- get
    let mod_ = mod state

    liftIO $ do
        memory <- newCString "memory"
        data_ <- newCString str
        dataPtr <- newArray [data_]
        offset <- liftIO $ Binaryen.constInt32 mod_ (fromIntegral m)
        offsetPtr <- newArray [offset]

        ptrIndex <- malloc
        poke ptrIndex (fromIntegral (length str))

        segmentsPassive <- malloc
        poke segmentsPassive 0
        _ <- Binaryen.setMemory mod_ 0 100 memory dataPtr segmentsPassive offsetPtr ptrIndex 1 0
        return ()

    liftIO $ Binaryen.constInt32 mod_ (fromIntegral m)
compileExprToWAST (Let name expr) = do
    mem <- alloc (sizeOf expr)
    state <- get
    let mod_ = mod state
    compiledExpr <- compileExprToWAST expr
    intMem <- liftIO $ Binaryen.Expression.constInt32 mod_ (fromIntegral mem)
    -- put $ state{varTable = (name, mem) : varTable state}
    liftIO $ Binaryen.Expression.store mod_ 4 0 0 intMem compiledExpr int32
compileExprToWAST (Var name) = do
    state <- get
    let mod_ = mod state
    location <- liftIO $ Binaryen.Expression.constInt32 mod_ (fromIntegral $ snd $ head $ filter (\(n, _) -> n == name) $ varTable state)
    liftIO $ Binaryen.Expression.load mod_ 4 0 0 0 int32 location
compileExprToWAST (DoBlock exprs) = do
    state <- get
    let mod_ = mod state
    compiledExprs <- mapM compileExprToWAST exprs
    wasmExprs <- liftIO $ allocaArray (length exprs) $ \ta -> do
        liftIO $ pokeArray ta compiledExprs
        return ta
    liftIO $ Binaryen.Expression.block mod_ nullPtr wasmExprs (fromIntegral $ length exprs) auto
compileExprToWAST (ModernFunc def dec) = do
    body <- compileExprToWAST $ fbody def
    state <- get
    let mod_ = mod state
    funcName <- liftIO $ newCString $ fname dec
    let types = map typeToWASTType $ fargs def
    let types_ = pop types
    let ret = if null types then Binaryen.Type.none else last types
    x <- liftIO $ allocaArray (length types_) $ \ta -> do
        liftIO $ pokeArray ta types_
        return ta
    type_ <- liftIO $ Binaryen.Type.create x (fromIntegral $ length types_)
    fun <- liftIO $ Binaryen.addFunction mod_ funcName type_ ret nullPtr 0 body
    if fname dec == "main"
        then do
            _ <- liftIO $ Binaryen.setStart mod_ fun
            liftIO $ Binaryen.Expression.nop mod_
        else liftIO $ Binaryen.Expression.nop mod_
compileExprToWAST _ = do
    state <- get
    let mod_ = mod state
    liftIO $ Binaryen.Expression.nop mod_
