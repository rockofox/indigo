module FrontendSpec (spec) where

import BytecodeCompiler
import Control.Monad.State (evalStateT)
import Data.Text qualified
import Data.Vector qualified as V
import Parser
import Parser qualified
import Test.Hspec
import Test.QuickCheck hiding (output)
import Text.Megaparsec (errorBundlePretty)
import VM

compileAndRun :: String -> IO String
compileAndRun prog = do
    let p = parseProgram (Data.Text.pack prog) Parser.CompilerFlags{verboseMode = False}
    case p of
        Left err -> error $ errorBundlePretty err
        Right program -> do
            xxx <- evalStateT (compileProgram program) (initCompilerState program)
            let xxxPoint = locateLabel xxx "main"
            vm <- runVMVM $ (initVM (V.fromList xxx)){pc = xxxPoint, breakpoints = [], callStack = [StackFrame{returnAddress = xxxPoint, locals = []}], ioMode = VMBuffer}
            pure $ output $ ioBuffer vm

spec = do
    describe "Hello World" $ do
        it "Should print Hello, world!" $ do
            compileAndRun "main => IO = println \"Hello, world!\"" `shouldReturn` "Hello, world!\n"
    describe "println" $ do
        it "Can print any string" $ do
            property $ \s -> compileAndRun ("main => IO = println " ++ show s) `shouldReturn` (s ++ "\n")
    describe "Pattern matching" $ do
        it "Can return the first element of a list" $ do
            compileAndRun "t :: List{Int} -> Int\nt (x:xs) = x\nmain => IO = println (t [1, 2, 3])" `shouldReturn` "1\n"
        it "Can return the first two" $ do
            compileAndRun "t :: List{Int} -> Int\nt (x:y:xs) = x + y\nmain => IO = println (t [1, 2, 3])" `shouldReturn` "3\n"
        it "Can return the excess" $ do
            compileAndRun "t :: List{Int} -> Int\nt (x:y:xs) = xs\nmain => IO = println (t [1, 2, 3])" `shouldReturn` "[3]\n"
    describe "Prelude" $ do
        it "Can use map" $ do
            compileAndRun "main => IO = println (map (`+`1), [1, 2, 3])" `shouldReturn` "[2,3,4]\n"
        it "Can use sum on integers" $ do
            compileAndRun "main => IO = println (sum [1, 2, 3])" `shouldReturn` "6\n"
        it "Can use sum on floats" $ do
            compileAndRun "main => IO = println (sum [1.0, 2.0, 3.0])" `shouldReturn` "6.0\n"
        xit "Can use foldl" $ do
            compileAndRun "main => IO = println (foldl (`+`), 0, [1, 2, 3])" `shouldReturn` "6\n"
    describe "Overloading" $ do
        it "Can find function based on type" $ do
            compileAndRun "f :: Int -> Int\nf x = x\nf :: String -> String\nf x = \"'\":x:\"'\"\nmain => IO = do\n println f 1\nprintln f \"test\"\nend" `shouldReturn` "1\n'test'\n"
