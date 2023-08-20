module BytecodeCompilerSpec (spec) where

import BytecodeCompiler
import Parser
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic
import qualified VM

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

spec :: Spec
spec = do
    describe "typesMatch" $ do
        it "Should be true for exact matches" $
            property $
                \t f ->
                    typesMatch f {types = [t]} [t]
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
