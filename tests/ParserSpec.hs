module ParserSpec (spec) where

import GHC.Generics
import Parser
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic

instance Arbitrary Type where
    arbitrary = genericArbitrary
    shrink = genericShrink

isFn :: Type -> Bool
isFn (Parser.Fn _ _) = True
isFn _ = False

spec :: Spec
spec = do
    describe "compareTypes" $ do
        it "Should be true for exact matches" $
            property $
                \t -> compareTypes t t `shouldBe` True
        it "Should be false for non-exact matches (except for Any or Fn)" $
            property $ \t1 t2 ->
                notElem Any [t1, t2]
                && not (isFn t1 || isFn t2)
                    && t1
                    /= t2
                    ==> compareTypes t1 t2
                    `shouldBe` False
        it "Any type should be a subtype of Any" $
            property $
                \t -> compareTypes t Any `shouldBe` True
        it "Fn any any should be a subtype of Fn Any Any" $
            property $
                \t -> compareTypes (Parser.Fn [t] Any) (Parser.Fn [Any] Any) `shouldBe` True
        it "List types should be equal if their element types are equal" $
            property $
                \t -> compareTypes (List t) (List t) `shouldBe` True
        it "StructT types should be equal if their fields are equal" $
            property $
                \t -> compareTypes (StructT t) (StructT t) `shouldBe` True
    describe "Parser" $ do
        it "Should parse a simple program" $
            parseProgram "main => IO = print \"Hello, world!\"" CompilerFlags{verboseMode = False}
                `shouldBe` Right
                    (Program [Function{fdef = [FuncDef{fname = "main", fargs = [], fbody = FuncCall "print" [StringLit "Hello, world!"]}], fdec = FuncDec{fname = "main", ftypes = [IO]}}])
