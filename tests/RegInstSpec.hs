module RegInstSpec where

import RegInst
import Test.Hspec
import VM

spec :: Spec
spec = do
    it "Empty should be empty" $
        toRegInst [] `shouldBe` []
    it "Should be able to convert a simple instruction" $
        toRegInst [Push (DInt 1)] `shouldBe` [RegInst (Mov 0 (DInt 1)) []]
    it "Should be able to convert multiple push instructions" $
        toRegInst [Push (DInt 1), Push (DInt 2)] `shouldBe` [RegInst (Mov 0 (DInt 1)) [], RegInst (Mov 1 (DInt 2)) []]
    it "Can add" $
        toRegInst [Push (DInt 1), Push (DInt 2), Add] `shouldBe` [RegInst (Mov 0 (DInt 1)) [], RegInst (Mov 1 (DInt 2)) [], RegInst Add [1, 0]]
    it "Can work with multiple labels" $
        toRegInst
            [ Label "main"
            , Push (DInt 1)
            , Push (DInt 2)
            , Add
            , Label "bla"
            , Push (DInt 3)
            , Push (DInt 4)
            , Add
            ]
            `shouldBe` [ RegInst (Label "main") []
                       , RegInst (Mov 0 (DInt 1)) []
                       , RegInst (Mov 1 (DInt 2)) []
                       , RegInst Add [1, 0]
                       , RegInst (Label "bla") []
                       , RegInst (Mov 0 (DInt 3)) []
                       , RegInst (Mov 1 (DInt 4)) []
                       , RegInst Add [1, 0]
                       ]
    it "Can work with functions" $
        toRegInst
            [ Label "main"
            , Push (DInt 1)
            , Push (DInt 2)
            , Call "bla"
            , Label "bla"
            , Add
            , Ret
            ]
            `shouldBe` [ RegInst (Label "main") []
                       , RegInst (Mov 0 (DInt 1)) []
                       , RegInst (Mov 1 (DInt 2)) []
                       , RegInst (Call "bla") []
                       , RegInst (Label "bla") []
                       , RegInst Add [1, 0]
                       , RegInst Ret []
                       ]
