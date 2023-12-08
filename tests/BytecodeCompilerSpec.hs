module BytecodeCompilerSpec (spec) where

import BytecodeCompiler
import Control.Monad.State (evalStateT)
import Data.Text qualified
import Parser
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic
import Text.Megaparsec (errorBundlePretty)
import Text.RawString.QQ (r)
import VM (Action (..), Data (..), Instruction (..))
import VM qualified

instance Arbitrary Type where
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
    let p = parseProgram (Data.Text.pack prog) Parser.initCompilerFlags
    case p of
        Left err -> error $ errorBundlePretty err
        Right program -> do
            evalStateT (compileProgramBare program) (initCompilerState program)

spec :: Spec
spec = do
    describe "Addition" $ do
        it "Should compile 2+4" $ do
            compile [r|2+4|]
                `shouldReturn` [Label "main", Push $ DInt 2, Push $ DInt 4, VM.Add, Exit]
        it "Should work properly with function calls" $ do
            compile
                [r|
                f x = x + 1
                let main => IO = unsafePrint (f 2) + 4|]
                `shouldReturn` [Label "f#0", LStore "x", LLoad "x", Push 1, VM.Add, Ret, Label "main", Push 2, Call "f#0", Push 4, VM.Add, Builtin Print, Exit]
    describe "Hello World" $ do
        it "Should print Hello, world!" $ do
            compile [r|let main => IO = unsafePrint "Hello, world!"|]
                `shouldReturn` [Label "main", Push $ DString "Hello, world!", Builtin Print, Exit]
    describe "Implicit casting" $ do
        it "Should cast from int to float" $ do
            compile [r|let main => IO = unsafePrint ^2 + 4.0|]
                `shouldReturn` [Label "main", Meta "flex", Push 2, Push 4.0, VM.Cast, Push 4.0, VM.Add, Builtin Print, Exit]
            compile [r|let main => IO = unsafePrint 2.0 + ^4|]
                `shouldReturn` [Label "main", Push 2.0, Meta "flex", Push 4, Swp, VM.Cast, Push 2.0, VM.Add, Builtin Print, Exit]
    describe "Explicit casting" $ do
        it "Can cast from int to float" $ do
            compile [r|2 as Float|]
                `shouldReturn` [Label "main", Push 2, Push 0.0, VM.Cast, Exit]
        it "Casts are compatible with binary operations" $ do
            compile [r|(2 as Float) + 4.0|]
                `shouldReturn` [Label "main", Push 2, Push 0.0, VM.Cast, Push 4.0, VM.Add, Exit]
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
                    Any
                        `notElem` [t1, t2]
                        && t1
                            /= t2
                        ==> typesMatch f{types = [t1, t2]} [t2, t1]
                            `shouldBe` False
        it "Should be true for Any" $
            property $
                \t f ->
                    typesMatch f{types = [Any]} [t]
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
