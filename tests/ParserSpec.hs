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
    describe "Basic" $ do
        it "Should parse a simple program" $
            parseProgram "main => IO = print \"Hello, world!\"" CompilerFlags{verboseMode = False}
                `shouldBe` Right
                    (Program [Function{fdef = [FuncDef{fname = "main", fargs = [], fbody = FuncCall "print" [StringLit "Hello, world!"]}], fdec = FuncDec{fname = "main", ftypes = [IO]}}])
    describe "Struct" $ do
        xit "Member access" $ do
            parseProgram "bello!name" CompilerFlags{verboseMode = False}
                `shouldBe` Right
                    (Program [StructAccess (StructLit "bello" []) (Var "name")])
    describe "Traits" $ do
        it "Should parse a trait decleration" $
            parseProgram "trait Show = do\nshow :: Self -> String\nend" CompilerFlags{verboseMode = False}
                `shouldBe` Right
                    (Program [Trait{tname = "Show", tmethods = [FuncDec{fname = "show", ftypes = [Self, String]}]}])
        it "Should parse a trait declaration with multiple methods" $
            parseProgram "trait Show = do\nshow :: Self -> String\nshow2 :: Self -> String\nend" CompilerFlags{verboseMode = False}
                `shouldBe` Right
                    (Program [Trait{tname = "Show", tmethods = [FuncDec{fname = "show", ftypes = [Self, String]}, FuncDec{fname = "show2", ftypes = [Self, String]}]}])
        it "Should parse a trait implementation" $
            parseProgram "impl Show for Point = do\nshow point = \"Point {x: \" : show point.x : \", y: \" : show point.y : \"}\"\nend" CompilerFlags{verboseMode = False}
                `shouldBe` Right
                    (Program [Impl{itrait = "Show", ifor = "Point", imethods = [parseFreeUnsafe "show point = \"Point {x: \" : show point.x : \", y: \" : show point.y : \"}\""]}])
