module VMSpec (spec) where

import Control.Monad.State
import Data.Map qualified as Map
import Data.Vector qualified as V
import Test.Hspec
import VM

spec :: Spec
spec = do
    describe "VM Stability & Crash-Proofing" $ do
        describe "Stack Underflows" $ do
            it "handles Add on empty stack without crashing" $ do
                let vm = (initVM (V.fromList [Add])){shouldExit = False}
                res <- runVMVM vm
                running res `shouldBe` False
                vmError res `shouldBe` Just "Add: expected 2 values on stack"

            it "handles Sub on 1-element stack without crashing" $ do
                let vm = (initVM (V.fromList [Push (DInt 5), Sub])){shouldExit = False}
                res <- runVMVM vm
                running res `shouldBe` False
                vmError res `shouldBe` Just "Sub: expected 2 values on stack"

            it "handles Pop on empty stack without crashing" $ do
                let vm = (initVM (V.fromList [Pop])){shouldExit = False}
                res <- runVMVM vm
                running res `shouldBe` False
                vmError res `shouldBe` Just "Stack underflow: pop from empty stack"

            it "handles Swp on 1-element stack without crashing" $ do
                let vm = (initVM (V.fromList [Push (DInt 1), Swp])){shouldExit = False}
                res <- runVMVM vm
                running res `shouldBe` False
                vmError res `shouldBe` Just "Swp: expected 2 values on stack"

            it "handles Dup on empty stack without crashing" $ do
                let vm = (initVM (V.fromList [Dup])){shouldExit = False}
                res <- runVMVM vm
                vmError res `shouldBe` Nothing
                stack res `shouldBe` []

        describe "Arithmetic Fault Tolerance" $ do
            it "handles integer modulo by zero without crashing" $ do
                let vm = (initVM (V.fromList [Push (DInt 10), Push (DInt 0), Mod])){shouldExit = False}
                res <- runVMVM vm
                stack res `shouldBe` [DInt 0]

            it "handles negative exponent on integer gracefully" $ do
                let vm = (initVM (V.fromList [Push (DInt 2), Push (DInt (-2)), Pow])){shouldExit = False}
                res <- runVMVM vm
                stack res `shouldBe` [DFloat 0.25]

            it "handles absolute value on Double without crashing" $ do
                let vm = (initVM (V.fromList [Push (DDouble (-4.5)), Abs])){shouldExit = False}
                res <- runVMVM vm
                stack res `shouldBe` [DDouble 4.5]

            it "handles mixed numeric arithmetic correctly" $ do
                let vm = (initVM (V.fromList [Push (DInt 10), Push (DFloat 2.5), Add, Push (DDouble 1.5), Mul])){shouldExit = False}
                res <- runVMVM vm
                stack res `shouldBe` [DDouble 18.75]

        describe "Memory Safety" $ do
            it "handles loading uninitialized or negative memory indices" $ do
                let vm = (initVM (V.fromList [Load 100, Load (-5)])){shouldExit = False}
                res <- runVMVM vm
                stack res `shouldBe` [DNone, DNone]

            it "dynamically grows memory on large index store" $ do
                let vm = (initVM (V.fromList [Push (DString "stored"), Store 5, Load 5])){shouldExit = False}
                res <- runVMVM vm
                stack res `shouldBe` [DString "stored"]
                length (memory res) `shouldBe` 6

        describe "Control Flow Safety" $ do
            it "stops gracefully when Ret is called on empty call stack" $ do
                let vm = (initVM (V.fromList [Ret])){shouldExit = False}
                res <- runVMVM vm
                running res `shouldBe` False

            it "halts with clean error on jump to non-existent label" $ do
                let vm = (initVM (V.fromList [Jmp "missing_label"])){shouldExit = False}
                res <- runVMVM vm
                running res `shouldBe` False
                vmError res `shouldBe` Just "Label not found: missing_label"

            it "halts with clean error on call to non-existent label" $ do
                let vm = (initVM (V.fromList [Call "nonexistent_fn"])){shouldExit = False}
                res <- runVMVM vm
                running res `shouldBe` False
                vmError res `shouldBe` Just "Label not found: nonexistent_fn"

            it "handles CallLocal with empty call stack safely" $ do
                let vm = (initVM (V.fromList [Label "target", Ret, Label "start", CallLocal "target"])){pc = 2, shouldExit = False}
                res <- runVMVM vm
                running res `shouldBe` False

            it "stops cleanly when PC advances past program bounds" $ do
                let vm = (initVM (V.fromList [Push (DInt 42)])){shouldExit = False}
                res <- runVMVM vm
                running res `shouldBe` False
                stack res `shouldBe` [DInt 42]

        describe "Total Data Types & Casts" $ do
            it "compares every constructor combination in Ord Data without throwing" $ do
                let sampleValues =
                        [ DNone
                        , DBool True
                        , DBool False
                        , DInt 42
                        , DFloat 3.14
                        , DDouble 2.718
                        , DChar 'z'
                        , DString "hello"
                        , DList [DInt 1, DInt 2]
                        , DMap Map.empty
                        , DFuncRef "f" [] []
                        , DTypeQuery "Int"
                        , DCPtr 0
                        ]
                let allPairs = [(a, b) | a <- sampleValues, b <- sampleValues]
                let results = map (uncurry compare) allPairs
                length results `shouldBe` length sampleValues * length sampleValues

            it "safely casts invalid numeric string without parse exception" $ do
                let vm = (initVM (V.fromList [Push (DString "not_a_number"), Push (DInt 0), Cast])){shouldExit = False}
                res <- runVMVM vm
                stack res `shouldBe` [DInt 0]

            it "safely casts empty string to Char without crashing" $ do
                let vm = (initVM (V.fromList [Push (DString ""), Push (DChar 'a'), Cast])){shouldExit = False}
                res <- runVMVM vm
                stack res `shouldBe` [DChar '\0']

            it "safely accesses missing struct field without throwing" $ do
                let vm = (initVM (V.fromList [Push (DMap (Map.singleton "a" (DInt 1))), Access "missing_field"])){shouldExit = False}
                res <- runVMVM vm
                stack res `shouldBe` [DNone]

            it "safely executes Slice with negative and out-of-bounds indices" $ do
                let vm = (initVM (V.fromList [Push (DString "abcdef"), Push (DInt (-3)), Push (DInt 10), Slice])){shouldExit = False}
                res <- runVMVM vm
                stack res `shouldBe` [DString "def"]
