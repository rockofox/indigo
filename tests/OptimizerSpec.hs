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
