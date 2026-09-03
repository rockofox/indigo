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
