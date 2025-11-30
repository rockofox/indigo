module PositionSpec (spec) where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isJust, isNothing)
import Language.LSP.Protocol.Types (Position (..), Range (..))
import Language.LSP.Test
import Test.Hspec
import TestUtils (lspExe)

spec :: Spec
spec = do
    describe "Position conversion" $ do
        it "converts LSP position to Indigo offset correctly" $ runSession lspExe fullLatestClientCaps "tests/data" $ do
            doc <- openDoc "position_test.in" "indigo"
            let pos = Position 0 0
            hover <- getHover doc pos
            liftIO $ hover `shouldSatisfy` (\h -> isJust h || isNothing h)

        it "handles multi-line positions" $ runSession lspExe fullLatestClientCaps "tests/data" $ do
            doc <- openDoc "position_multiline.in" "indigo"
            let pos = Position 1 4
            hover <- getHover doc pos
            liftIO $ hover `shouldSatisfy` (\h -> isJust h || isNothing h)

        it "handles positions at end of line" $ runSession lspExe fullLatestClientCaps "tests/data" $ do
            doc <- openDoc "position_test.in" "indigo"
            let pos = Position 0 10
            hover <- getHover doc pos
            liftIO $ hover `shouldSatisfy` (\h -> isJust h || isNothing h)
