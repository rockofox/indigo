module BytecodeCompilerSpec (spec) where

import AST qualified
import BytecodeCompiler
import Control.Monad (join)
import Control.Monad.State (evalStateT, liftIO)
import Data.Functor ((<&>))
import Data.List (isInfixOf)
import Data.Text qualified
import Debug.Trace
import ErrorRenderer (parseErrorBundleToSourceErrors, renderErrors)
import Foreign
import GHC.IO (unsafePerformIO)
import Parser qualified
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic
import Text.RawString.QQ (r)
import Util
import VM (Action (..), Data (..), Instruction (..))

instance Arbitrary WordPtr where
    arbitrary = fromIntegral <$> (arbitrary :: Gen Word64)
    shrink x = [x]

instance Arbitrary Parser.Type where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary VM.Data where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary VM.Action where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary VM.Instruction where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary BytecodeCompiler.Function where
    arbitrary = genericArbitrary
    shrink = genericShrink

compile :: String -> IO [VM.Instruction]
compile prog = do
    let p = Parser.parseProgram (Data.Text.pack prog) Parser.initCompilerFlags
    case p of
        Left err -> error $ renderErrors (parseErrorBundleToSourceErrors err (Data.Text.pack prog)) prog
        Right program -> do
            evalStateT
                ( compileProgramBare program >>= \case
                    Right err -> do
                        liftIO $ compileFail "<input>" err prog
                        error ""
                    Left p -> return p
                )
                (initCompilerState program)
                    { funcDecs = [AST.FuncDec "+" [AST.StructT "Int", AST.StructT "Int", AST.StructT "Int"] [] AST.anyPosition]
                    }

compileWithErrors :: String -> IO (Either [BytecodeCompiler.CompilerError] [VM.Instruction])
compileWithErrors prog = do
    let p = Parser.parseProgram (Data.Text.pack prog) Parser.initCompilerFlags
    case p of
        Left err -> error $ renderErrors (parseErrorBundleToSourceErrors err (Data.Text.pack prog)) prog
        Right program -> do
            result <-
                evalStateT
                    (compileProgram program)
                    (initCompilerState program)
                        { funcDecs = [AST.FuncDec "+" [AST.StructT "Int", AST.StructT "Int", AST.StructT "Int"] [] AST.anyPosition]
                        }
            return $ case result of
                Left instructions -> Right instructions
                Right errors -> Left errors

spec :: Spec
spec = do
    describe "Addition" $ do
        it "Should compile 2+4" $ do
            compile [r|2+4|]
                `shouldReturn` [Label "main", StoreSideStack, Push 2, LStore "__op_a_0", Push 4, LStore "__op_b_0", LLoad "__op_a_0", LLoad "__op_b_0", Call "+", ClearSideStack, Push $ DInt 0, Exit]
        it "Should work properly with function calls" $ do
            compile
                [r|
                f x = x + 1
                let main : IO = unsafePrint (f 2) + 4|]
                `shouldReturn` [Label "f#0", StoreSideStack, LStore "x", LLoad "x", LStore "__op_a_1", Push 1, LStore "__op_b_1", LLoad "__op_a_1", LLoad "__op_b_1", Call "+", ClearSideStack, Ret, Label "main", StoreSideStack, Push 2, Call "f#0", LStore "__op_a_2", Push 4, LStore "__op_b_2", LLoad "__op_a_2", LLoad "__op_b_2", Call "+", Builtin Print, ClearSideStack, Push $ DInt 0, Exit]
    describe "Hello World" $ do
        it "Should print Hello, world!" $ do
            compile [r|let main : IO = unsafePrint "Hello, world!"|]
                `shouldReturn` [Label "main", StoreSideStack, Push $ DString "Hello, world!", Builtin Print, ClearSideStack, Push $ DInt 0, Exit]
    -- describe "Implicit casting" $ do
    --     it "Should cast from int to float" $ do
    --         compile [r|let main : IO = unsafePrint ^2 + 4.0|]
    --             `shouldReturn` [Label "main", StoreSideStack, Meta "flex", Push 2, LStore "__op_a_0", Push 4.0, LStore "__op_b_0", LLoad "__op_a_0", LLoad "__op_b_0", Cast, Push 4.0, Call "+", Builtin Print, ClearSideStack, Push $ DInt 0, Exit]
    --         compile [r|let main : IO = unsafePrint 2.0 + ^4|]
    --             `shouldReturn` [Label "main", StoreSideStack, Push 2.0, LStore "__op_a_0", Meta "flex", Push 4, LStore "__op_b_0", LLoad "__op_a_0", LLoad "__op_b_0", Swp, Cast, Push 2.0, Call "+", Builtin Print, ClearSideStack, Push $ DInt 0, Exit]
    describe "Explicit casting" $ do
        it "Can cast from int to float" $ do
            compile [r|2 as Float|]
                `shouldReturn` [Label "main", StoreSideStack, Push 2, Push 0.0, Cast, ClearSideStack, Push $ DInt 0, Exit]
        it "Casts are compatible with binary operations" $ do
            compile [r|(2 as Float) + 4.0|]
                `shouldReturn` [Label "main", StoreSideStack, Push 2, Push 0.0, Cast, LStore "__op_a_0", Push 4.0, LStore "__op_b_0", LLoad "__op_a_0", LLoad "__op_b_0", Call "+", ClearSideStack, Push $ DInt 0, Exit]
    xdescribe "typesMatch" $ do
        it "Should be true for exact matches" $
            property $
                \t f ->
                    typesMatch f{types = [t]} [t]
                        `shouldBe` True
        it "Should be true for exact matches with multiple types" $
            property $
                \t1 t2 f ->
                    typesMatch f{types = [t1, t2]} [t1, t2]
                        `shouldBe` True
        it "Should be false for exact matches with multiple types in different order" $
            property $
                \t1 t2 f ->
                    Parser.Any
                        `notElem` [t1, t2]
                        && t1
                            /= t2
                        ==> typesMatch f{types = [t1, t2]} [t2, t1]
                            `shouldBe` False
        it "Should be true for Any" $
            property $
                \t f ->
                    typesMatch f{types = [Parser.Any]} [t]
                        `shouldBe` True
        it "Should be true for partial matches" $
            property $
                \t1 t2 f ->
                    typesMatch f{types = [t1, t2]} [t1]
                        `shouldBe` True
        it "Should be false for more arguments than types" $
            property $
                \t1 t2 f ->
                    typesMatch f{types = [t1]} [t1, t2]
                        `shouldBe` False
    describe "Refinement types" $ do
        it "Should compile successfully when refinement passes" $ do
            result <-
                compileWithErrors
                    [r|
                struct Age = (value: Int) satisfies (value >= 0)
                let main : IO = do
                    let a = Age{value: 5}
                end
                |]
            case result of
                Left errors ->
                    let errorMessages = map BytecodeCompiler.errorMessage errors
                     in if any ("Refinement failed" `isInfixOf`) errorMessages
                            then expectationFailure $ "Expected successful compilation, got refinement error: " ++ show errors
                            else expectationFailure $ "Expected successful compilation, got errors: " ++ show errors
                Right _ -> return ()
        it "Should produce error when refinement fails" $ do
            result <-
                compileWithErrors
                    [r|
                struct Age = (value: Int) satisfies (value >= 0)
                let main : IO = do
                    let a = Age{value: 0 - 1}
                end
                |]
            case result of
                Left errors -> do
                    let errorMessages = map BytecodeCompiler.errorMessage errors
                    any ("Refinement failed" `isInfixOf`) errorMessages
                        `shouldBe` True
                Right _ -> expectationFailure "Expected compilation error for failed refinement"
        it "Should compile normally when struct has no refinement" $ do
            result <-
                compileWithErrors
                    [r|
                struct Person = (name: String)
                let main : IO = do
                    let p = Person{name: "Alice"}
                end
                |]
            case result of
                Left errors -> expectationFailure $ "Expected successful compilation, got errors: " ++ show errors
                Right _ -> return ()
        it "Should compile refinements with equality" $ do
            result <-
                compileWithErrors
                    [r|
                struct Person = (name: String) satisfies (name == "Alice")
                let main : IO = do
                    let p = Person{name: "Alice"}
                end
                |]
            case result of
                Left errors -> expectationFailure $ "Expected successful compilation, got errors: " ++ show errors
                Right _ -> return ()
        it "Should compile successfully when refinement passes with local bindings" $ do
            result <-
                compileWithErrors
                    [r|
                struct Age = (value: Int) satisfies (value >= 0)
                let main : IO = do
                    let ageValue = 25
                    let a = Age{value: ageValue}
                end
                |]
            case result of
                Left errors ->
                    let errorMessages = map BytecodeCompiler.errorMessage errors
                     in if any ("Refinement failed" `isInfixOf`) errorMessages
                            then expectationFailure $ "Expected successful compilation, got refinement error: " ++ show errors
                            else expectationFailure $ "Expected successful compilation, got errors: " ++ show errors
                Right _ -> return ()
        it "Should produce error when refinement fails with local bindings" $ do
            result <-
                compileWithErrors
                    [r|
                struct Age = (value: Int) satisfies (value >= 0)
                let main : IO = do
                    let ageValue = 0 - 5
                    let a = Age{value: ageValue}
                end
                |]
            case result of
                Left errors -> do
                    let errorMessages = map BytecodeCompiler.errorMessage errors
                    any ("Refinement failed" `isInfixOf`) errorMessages
                        `shouldBe` True
                Right _ -> expectationFailure "Expected compilation error for failed refinement with local bindings"
        it "Should compile successfully with multiple local bindings in refinement" $ do
            result <-
                compileWithErrors
                    [r|
                let notJohn (x: String): Boolean = x != "John"
                struct Teacher = (name: String, age: Int) satisfies ((notJohn name) && age > 18)
                let main : IO = do
                    let hisAge = 19
                    let otherName = "Johnny"
                    let t = Teacher { name: otherName, age: hisAge }
                end
                |]
            case result of
                Left errors ->
                    let errorMessages = map BytecodeCompiler.errorMessage errors
                     in if any ("Refinement failed" `isInfixOf`) errorMessages
                            then expectationFailure $ "Expected successful compilation, got refinement error: " ++ show errors
                            else expectationFailure $ "Expected successful compilation, got errors: " ++ show errors
                Right _ -> return ()
