module FrontendSpec (spec) where

import BytecodeCompiler
    ( compileProgram
    , initCompilerState
    , locateLabel
    )
import Control.Monad.State (evalStateT)
import Data.Text qualified
import Data.Vector qualified as V
import Parser
    ( CompilerFlags (CompilerFlags, verboseMode)
    , parseProgram
    )
import Parser qualified
import Test.Hspec (describe, it, shouldReturn, xit)
import Test.QuickCheck (Testable (property))
import Text.Megaparsec (errorBundlePretty)
import Text.RawString.QQ (r)
import VM
    ( IOBuffer (output)
    , IOMode (VMBuffer)
    , StackFrame (StackFrame, locals, returnAddress)
    , VM (breakpoints, callStack, ioBuffer, ioMode, pc)
    , initVM
    , runVMVM
    )

compileAndRun :: String -> IO String
compileAndRun prog = do
    let p = parseProgram (Data.Text.pack prog) Parser.CompilerFlags{verboseMode = False}
    case p of
        Left err -> error $ errorBundlePretty err
        Right program -> do
            xxx <- evalStateT (compileProgram program) (initCompilerState program)
            let xxxPoint = locateLabel xxx "main"
            -- putStrLn $ printAssembly (V.fromList xxx) True
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
        xit "Can use sum on floats" $ do
            compileAndRun "main => IO = println (sum [1.0, 2.0, 3.0])" `shouldReturn` "6.0\n"
        xit "Can use foldl" $ do
            compileAndRun "main => IO = println (foldl (`+`), 0, [1, 2, 3])" `shouldReturn` "6\n"
    describe "Implicit casting" $ do
        it "Can cast from int to float" $ do
            compileAndRun [r|main => IO = println ^2 + 4.0|] `shouldReturn` "6.0\n"
            compileAndRun [r|main => IO = println 2.0 + ^4|] `shouldReturn` "6.0\n"
        it "Can cast from float to int" $ do
            compileAndRun [r|main => IO = println ^2.0 + 4|] `shouldReturn` "6\n"
            compileAndRun [r|main => IO = println 2 + ^4.0|] `shouldReturn` "6\n"
        it "Can cast from int to string" $ do
            compileAndRun [r|main => IO = println ^2 + "test"|] `shouldReturn` "2test\n"
        it "Can cast from string to int" $ do
            compileAndRun [r|main => IO = println ^"2" + 4|] `shouldReturn` "6\n"
    describe "Explicit casting" $ do
        it "Can cast from int to float" $ do
            compileAndRun [r|main => IO = println (2 as Float) + 4.0|] `shouldReturn` "6.0\n"
            compileAndRun [r|main => IO = println 2.0 + (4 as Float)|] `shouldReturn` "6.0\n"
        it "Can cast a function call" $ do
            compileAndRun
                [r|f :: Int -> Int
                            f x = x + 1
                            main => IO = println ((f 2) as Float) + 4.0|]
                `shouldReturn` "7.0\n"
    describe "Overloading" $ do
        it "Can find function based on type" $ do
            compileAndRun
                [r|
                f :: Int -> Int
                f x = x + 1
                f :: String -> String
                f x = "'":x:"'" 
                main => IO = do
                    println f 1
                    println f "test"
                end|]
                `shouldReturn` "2\n'test'\n"
        it "Can find function based on type with lists" $ do
            compileAndRun
                [r|
                f :: List{Int} -> Int
                f x = sum x
                f :: List{String} -> String
                f x = x
                main => IO = do
                    println f [1, 2, 3]
                    println f ["test", "test2"]
                end|]
                `shouldReturn` "6\ntesttest2\n"
    describe "Traits" $ do
        it "Can use a trait" $ do
            compileAndRun
                [r|
                    trait Animal = do
                        makeNoise :: Self -> IO
                    end

                    impl Animal for Dog = do
                        makeNoise self = println "Woof"
                    end

                    impl Animal for Cat = do
                        makeNoise self = println "Meow"
                    end

                    main => IO = do
                        makeNoise (Dog {})
                        makeNoise (Cat {})
                    end
                |]
                `shouldReturn` "Woof\nMeow\n"

-- it "Can find function based on type with lists, multiple definitions and pattern matching" $ do
--     compileAndRun
--         [r|
--         f :: List{Int} -> Int
--         f [] = 0
--         f (x:xs) = x + (f xs)
--         f :: List{Float} -> Float
--         # f [] = 0.0
--         f (x:xs) = x + ((f xs) as Float)
--         # f :: List{String} -> String
--         # f x = x
--         main => IO = do
--             println f [1, 2, 3]
--             println f [1.0, 2.0, 3.0]
--             # println f ["test", "test2"]
--         end|]
--         `shouldReturn` "6\n6.0\ntesttest2\n"
