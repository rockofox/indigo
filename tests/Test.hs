{-# LANGUAGE DataKinds #-}

import Data.ByteString.Lazy.Char8 as Char8
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy (unpack)
import GHC.IO.Exception (ExitCode (..))
import Parser (Program, parseProgram)
import System.IO
import System.Process.Typed
import Test.Hspec
import Text.Megaparsec.Error (ParseErrorBundle, errorBundlePretty)
import Text.RawString.QQ (r)
import WASMEmitter (compileProgramToWAST)

executeWasmtime :: String -> IO String
executeWasmtime input = do
    -- (exitCode, out, err) <- readProcess $ setStdin (byteStringInput $ Char8.pack input) "wasmtime -"
    -- return $ Char8.unpack out
    let processConfig =
            setStdin (byteStringInput (Char8.pack input)) $
                setStdout byteStringOutput $
                    setStderr byteStringOutput $
                        shell "wasmtime -"
    (exitCode, stdout, stderr) <- readProcess processConfig
    case exitCode of
        ExitSuccess -> return $ Char8.unpack stdout
        ExitFailure _ -> return $ Char8.unpack stderr

tryParse :: String -> Parser.Program
tryParse program = do
    let parseResult = parseProgram (T.pack program)
    case parseResult of
        Left err -> error $ "Parse error: " ++ errorBundlePretty err
        Right expr -> do
            expr

main :: IO ()
main = hspec $ do
    describe "Hello World" $ do
        it "prints 'hello world'" $ do
            let program =
                    [r|
                        extern wasi_unstable fd_write :: Int -> Int -> Int -> Int -> Int

                        puts str : String -> len : Int => IO = do
                            __wasm_i32_store 0 str
                            __wasm_i32_store 4 len
                            discard (fd_write 1 0 1 20)
                        end

                        main => IO = do
                            let x = "hello world\n"
                            puts (x) 12
                        end
            |]
            wat <- compileProgramToWAST (tryParse program)
            executeWasmtime wat `shouldReturn` "hello world\n"