module OptimizerSpec (spec) where

import Optimizer
import Test.Hspec
import VM

spec :: Spec
spec = do
    describe "Tree shaker" $ do
        it "Can tree shake a basic program" $
            do
                treeShake
                    [ Label "hello"
                    , Push $ DString "Hello, World!"
                    , Builtin Print
                    , Ret
                    , Label "unused"
                    , Push $ DInt 2
                    , Ret
                    , Label "main"
                    , Call "hello"
                    ]
                `shouldBe` [Label "hello", Push $ DString "Hello, World!", Builtin Print, Ret, Label "main", Call "hello"]
    describe "Side-stack elimination" $ do
        it "Removes StoreSideStack and ClearSideStack when no LoadSideStack exists" $
            do
                eliminateSideStack
                    [ Label "foo#1"
                    , StoreSideStack
                    , Push $ DInt 42
                    , ClearSideStack
                    , Ret
                    ]
                `shouldBe` [Label "foo#1", Push $ DInt 42, Ret]
        it "Preserves side stack instructions when LoadSideStack is present" $
            do
                eliminateSideStack
                    [ Label "bar#1"
                    , StoreSideStack
                    , Push $ DInt 1
                    , ClearSideStack
                    , Ret
                    , Label "bar#2"
                    , LoadSideStack
                    , Push $ DInt 2
                    , ClearSideStack
                    , Ret
                    ]
                `shouldBe` [ Label "bar#1"
                           , StoreSideStack
                           , Push $ DInt 1
                           , ClearSideStack
                           , Ret
                           , Label "bar#2"
                           , LoadSideStack
                           , Push $ DInt 2
                           , ClearSideStack
                           , Ret
                           ]
    describe "Strength reduction" $ do
        it "Replaces float pow 2.0 with dup and mul" $
            do
                strengthReduction [Push $ DFloat 2.0, Pow]
                `shouldBe` [Dup, Mul]
        it "Eliminates addition with zero" $
            do
                strengthReduction [Push $ DInt 0, Add]
                `shouldBe` []
        it "Folds constant addition" $
            do
                strengthReduction [Push $ DInt 2, Push $ DInt 3, Add]
                `shouldBe` [Push $ DInt 5]
    describe "Boolean & branch folding" $ do
        it "Simplifies Not followed by Jf into Jt" $
            do
                booleanBranchFolding [Not, Jf "target"]
                `shouldBe` [Jt "target"]
        it "Simplifies Not followed by Jt into Jf" $
            do
                booleanBranchFolding [Not, Jt "target"]
                `shouldBe` [Jf "target"]
        it "Eliminates double negation" $
            do
                booleanBranchFolding [Not, Not]
                `shouldBe` []
        it "Eliminates Dup followed by Pop" $
            do
                booleanBranchFolding [Dup, Pop]
                `shouldBe` []
        it "Eliminates redundant jump to immediate next label" $
            do
                booleanBranchFolding [Jmp "here", Label "here"]
                `shouldBe` [Label "here"]
