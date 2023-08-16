{-# LANGUAGE ForeignFunctionInterface #-}

module WASMReactor (runProgramRaw, runProgramRawBuffered, mallocPtr, mallocBytes, free_) where

import BytecodeCompiler
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy (pack, toStrict)
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.ByteString.Unsafe qualified as BU
import Data.Char (ord)
import Data.IORef (readIORef)
import Data.Text qualified
import Foreign
    ( Ptr
    , Storable (poke)
    , copyBytes
    , free
    , malloc
    , mallocBytes
    )
import Foreign.C (CString, newCString, peekCStringLen)
import Foreign.C.String (peekCString)
import Foreign.C.Types (CChar)
import Parser (CompilerFlags (CompilerFlags), parseProgram)
import VM
    ( IOBuffer (output)
    , IOMode (VMBuffer)
    , StackFrame (StackFrame, locals, returnAddress)
    , VM (breakpoints, callStack, ioBuffer, ioMode, pc)
    , initVM
    , printAssembly
    , runVM
    , runVMVM
    )
import Parser qualified
import Control.Monad
import Control.Monad.State (MonadIO (liftIO), StateT, evalStateT, gets, modify)
import Text.Megaparsec (errorBundlePretty)


-- TODO: Put this stuff into a seperate file
runProgramRaw :: Ptr CChar -> Int -> IO ()
runProgramRaw inputPtr inputLen = do
    input <- peekCStringLen (inputPtr, fromIntegral inputLen)
    let p = parseProgram (Data.Text.pack input) CompilerFlags{verboseMode = False}
    case p of
        Left err -> putStrLn $ errorBundlePretty err
        Right program -> do
            xxx <- evalStateT (compileProgram program) (CompilerState program [] [] [] 0)
            -- print program
            putStrLn $ printAssembly xxx True
            let xxxPoint = locateLabel xxx "main"
            runVM $ (initVM xxx){pc = xxxPoint, breakpoints = [], callStack = [StackFrame{returnAddress = xxxPoint, locals = []}]}

runProgramRawBuffered :: Ptr CChar -> Int -> Ptr (Ptr CChar) -> IO Int
runProgramRawBuffered inputPtr inputLen outputPtrPtr = do
    program <- peekCStringLen (inputPtr, inputLen)
    let p = parseProgram (Data.Text.pack program) Parser.CompilerFlags{verboseMode = False}
    case p of
        Left err -> error $ errorBundlePretty err
        Right program -> do
            xxx <- evalStateT (compileProgram program) (CompilerState program [] [] [] 0)
            let xxxPoint = locateLabel xxx "main"
            vm <- runVMVM $ (initVM xxx){pc = xxxPoint, breakpoints = [], callStack = [StackFrame{returnAddress = xxxPoint, locals = []}], ioMode = VMBuffer}
            let output' = BS.pack $ output $ ioBuffer vm
            BU.unsafeUseAsCStringLen output' $ \(buf, len) -> do
                outputPtr <- mallocBytes len
                poke outputPtrPtr outputPtr
                copyBytes outputPtr buf len
                pure len

foreign export ccall mallocPtr :: IO (Ptr (Ptr a))

mallocPtr :: IO (Ptr (Ptr a))
mallocPtr = malloc

foreign export ccall runProgramRawBuffered :: Ptr CChar -> Int -> Ptr (Ptr CChar) -> IO Int

foreign export ccall runProgramRaw :: Ptr CChar -> Int -> IO ()

foreign export ccall mallocBytes :: Int -> IO (Ptr a)

free_ :: Ptr a -> IO ()
free_ = free

foreign export ccall free_ :: Ptr a -> IO ()
