{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HoverSpec (spec) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (isInfixOf)
import Language.LSP.Protocol.Types
import Language.LSP.Test
import Test.Hspec
import TestUtils (lspExe)

extractHoverContent :: Maybe Hover -> Maybe MarkupContent
extractHoverContent (Just (Hover contents _)) = case contents of
    InL mc -> Just mc
    InR _ -> Nothing
extractHoverContent Nothing = Nothing

spec :: Spec
spec = do
    describe "Hover" $ do
        it "shows hover for variables" $ runSession lspExe fullLatestClientCaps "tests/data" $ do
            doc <- openDoc "hover_var.in" "indigo"
            hover <- getHover doc (Position 1 8)
            liftIO $ case extractHoverContent hover of
                Just (MarkupContent _ content) -> "Variable" `isInfixOf` content `shouldBe` True
                Nothing -> expectationFailure "Expected hover content"

        it "shows hover for function calls" $ runSession lspExe fullLatestClientCaps "tests/data" $ do
            doc <- openDoc "hover_func.in" "indigo"
            hover <- getHover doc (Position 1 13)
            liftIO $ case extractHoverContent hover of
                Just (MarkupContent _ content) -> "add" `isInfixOf` content `shouldBe` True
                Nothing -> expectationFailure "Expected hover content"

        it "shows hover for literals" $ runSession lspExe fullLatestClientCaps "tests/data" $ do
            doc <- openDoc "hover_literal.in" "indigo"
            hover <- getHover doc (Position 0 8)
            liftIO $ case extractHoverContent hover of
                Just (MarkupContent _ content) -> "42" `isInfixOf` content `shouldBe` True
                Nothing -> expectationFailure "Expected hover content"

        it "shows hover for struct definitions" $ runSession lspExe fullLatestClientCaps "tests/data" $ do
            doc <- openDoc "hover_struct.in" "indigo"
            hover <- getHover doc (Position 0 7)
            liftIO $ case extractHoverContent hover of
                Just (MarkupContent _ content) -> "Person" `isInfixOf` content `shouldBe` True
                Nothing -> expectationFailure "Expected hover content"

        it "shows hover for qualified symbols" $ runSession lspExe fullLatestClientCaps "tests/data" $ do
            doc <- openDoc "hover_qualified.in" "indigo"
            hover <- getHover doc (Position 2 15)
            liftIO $ case extractHoverContent hover of
                Just (MarkupContent _ content) -> "add" `isInfixOf` content `shouldBe` True
                Nothing -> expectationFailure "Expected hover content for qualified symbol"

        it "returns Nothing for positions without expressions" $ runSession lspExe fullLatestClientCaps "tests/data" $ do
            doc <- openDoc "hover_empty.in" "indigo"
            hover <- getHover doc (Position 0 0)
            liftIO $ hover `shouldBe` Nothing
