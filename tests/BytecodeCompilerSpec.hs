module BytecodeCompilerSpec (spec) where

import BytecodeCompiler
import Control.Monad.State (evalStateT)
import Data.Text qualified
import Foreign
import Parser qualified
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic
import Text.Megaparsec (errorBundlePretty)
import Text.RawString.QQ (r)
import VM (Action (..), Data (..), Instruction (..))

-- import VM qualified

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
        Left err -> error $ errorBundlePretty err
        Right program -> do
            evalStateT (compileProgramBare program) (initCompilerState program)

spec :: Spec
spec = do
    describe "Addition" $ do
        it "Should compile 2+4" $ do
            compile [r|2+4|]
                `shouldReturn` [Label "main", Push 2, LStore "__op_a_0", Push 4, LStore "__op_b_0", LLoad "__op_a_0", LLoad "__op_b_0", Add, Exit]
        it "Should work properly with function calls" $ do
            compile
                [r|
                f x = x + 1
                let main => IO = unsafePrint (f 2) + 4|]
                `shouldReturn` [Label "f#0", LStore "x", LLoad "x", LStore "__op_a_1", Push 1, LStore "__op_b_1", LLoad "__op_a_1", LLoad "__op_b_1", Add, Ret, Label "main", Push 2, Call "f#0", LStore "__op_a_2", Push 4, LStore "__op_b_2", LLoad "__op_a_2", LLoad "__op_b_2", Add, Builtin Print, Exit]
    describe "Hello World" $ do
        it "Should print Hello, world!" $ do
            compile [r|let main => IO = unsafePrint "Hello, world!"|]
                `shouldReturn` [Label "main", Push $ DString "Hello, world!", Builtin Print, Exit]
    describe "Implicit casting" $ do
        it "Should cast from int to float" $ do
            compile [r|let main => IO = unsafePrint ^2 + 4.0|]
                `shouldReturn` [Label "main", Meta "flex", Push 2, LStore "__op_a_0", Push 4.0, LStore "__op_b_0", LLoad "__op_a_0", LLoad "__op_b_0", Cast, Push 4.0, Add, Builtin Print, Exit]
            compile [r|let main => IO = unsafePrint 2.0 + ^4|]
                `shouldReturn` [Label "main", Push 2.0, LStore "__op_a_0", Meta "flex", Push 4, LStore "__op_b_0", LLoad "__op_a_0", LLoad "__op_b_0", Swp, Cast, Push 2.0, Add, Builtin Print, Exit]
    describe "Explicit casting" $ do
        it "Can cast from int to float" $ do
            compile [r|2 as Float|]
                `shouldReturn` [Label "main", Push 2, Push 0.0, VM.Cast, Exit]
        it "Casts are compatible with binary operations" $ do
            compile [r|(2 as Float) + 4.0|]
                `shouldReturn` [Label "main", Push 2, Push 0.0, Cast, LStore "__op_a_0", Push 4.0, LStore "__op_b_0", LLoad "__op_a_0", LLoad "__op_b_0", Add, Exit]
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
