{-# LANGUAGE ForeignFunctionInterface #-}

module WASMReactor (runProgramRaw, runProgramRawBuffered, mallocPtr, mallocBytes, free_) where

import BytecodeCompiler
import Control.Monad
import Control.Monad.State (MonadIO (liftIO), StateT, evalStateT, gets, modify)
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy (pack, toStrict)
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.ByteString.Unsafe qualified as BU
import Data.Char (ord)
import Data.IORef (readIORef)
import Data.Text qualified
import Data.Vector qualified as V
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
import Parser (CompilerFlags (CompilerFlags), exprs, initCompilerFlags, parseProgram)
import Parser qualified
import Text.Megaparsec (errorBundlePretty)
import VM
    ( IOBuffer (..)
    , IOMode (VMBuffer)
    , StackFrame (StackFrame, locals, returnAddress)
    , VM (breakpoints, callStack, ioBuffer, ioMode, pc)
    , initVM
    , printAssembly
    , runVM
    , runVMVM
    , shouldExit
    )
import Verifier (verifyProgram)

-- TODO: Put this stuff into a seperate file
runProgramRaw :: Ptr CChar -> Int -> IO ()
runProgramRaw progPtr progLen = do
    input <- peekCStringLen (progPtr, fromIntegral progLen)
    let p = parseProgram (Data.Text.pack input) initCompilerFlags
    case p of
        Left err -> putStrLn $ errorBundlePretty err
        Right program -> do
            xxx <-
                evalStateT (compileProgram program) (initCompilerState program) >>= \case
                    Left instructions -> return instructions
                    Right errors -> do
                        compileFail "<input>" errors input
                        errorWithoutStackTrace ""
            putStrLn $ printAssembly (V.fromList xxx) True
            let xxxPoint = locateLabel xxx "main"
            runVM $ (initVM (V.fromList xxx)){pc = xxxPoint, breakpoints = [], callStack = [StackFrame{returnAddress = xxxPoint, locals = []}]}

runProgramRawBuffered :: Ptr CChar -> Int -> Ptr CChar -> Int -> Ptr (Ptr CChar) -> IO Int
runProgramRawBuffered progPtr progLen inputPtr inputLen outputPtrPtr = do
    programStr <- peekCStringLen (progPtr, progLen)
    input <- peekCStringLen (inputPtr, inputLen)
    let p = parseProgram (Data.Text.pack programStr) Parser.initCompilerFlags
    case p of
        Left err -> errorWithoutStackTrace $ errorBundlePretty err
        Right program -> do
            verified <- verifyProgram "" (Data.Text.pack programStr) (exprs program)
            case verified of
                Left err -> errorWithoutStackTrace $ errorBundlePretty err
                Right _ -> do
                    xxx <-
                        evalStateT (compileProgram program) (initCompilerState program) >>= \case
                            Left instructions -> return instructions
                            Right errors -> do
                                compileFail "<input>" errors programStr
                                errorWithoutStackTrace ""
                    let xxxPoint = locateLabel xxx "main"
                    vm <- runVMVM $ (initVM (V.fromList xxx)){pc = xxxPoint, breakpoints = [], callStack = [StackFrame{returnAddress = xxxPoint, locals = []}], ioMode = VMBuffer, ioBuffer = IOBuffer{input = input, output = ""}, shouldExit = False}
                    let output' = BS.pack $ output $ ioBuffer vm
                    BU.unsafeUseAsCStringLen output' $ \(buf, len) -> do
                        outputPtr <- mallocBytes len
                        poke outputPtrPtr outputPtr
                        copyBytes outputPtr buf len
                        pure len

foreign export ccall mallocPtr :: IO (Ptr (Ptr a))

mallocPtr :: IO (Ptr (Ptr a))
mallocPtr = malloc

foreign export ccall runProgramRawBuffered :: Ptr CChar -> Int -> Ptr CChar -> Int -> Ptr (Ptr CChar) -> IO Int

foreign export ccall runProgramRaw :: Ptr CChar -> Int -> IO ()

foreign export ccall mallocBytes :: Int -> IO (Ptr a)

free_ :: Ptr a -> IO ()
free_ = free

foreign export ccall free_ :: Ptr a -> IO ()
