module CompletionSpec (spec) where

import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Data.Text qualified as T
import Language.LSP.Protocol.Lens
import Language.LSP.Protocol.Types
    ( CompletionItemKind (..)
    , Position (..)
    )
import Language.LSP.Test
import Test.Hspec
import TestUtils (lspExe)

spec :: Spec
spec = do
    describe "Completion" $ do
        it "provides keyword completions" $ runSession lspExe fullLatestClientCaps "tests/data" $ do
            doc <- openDoc "completion_keywords.in" "indigo"
            completions <- getCompletions doc (Position 0 0)
            liftIO $ do
                let keywords = filter (\i -> i ^. kind == Just CompletionItemKind_Keyword) completions
                Prelude.length keywords `shouldSatisfy` (> 0)
                let keywordLabels = map (T.unpack . (^. label)) keywords
                keywordLabels `shouldContain` ["let"]
                keywordLabels `shouldContain` ["if"]

        it "provides function completions" $ runSession lspExe fullLatestClientCaps "tests/data" $ do
            doc <- openDoc "completion_keywords.in" "indigo"
            completions <- getCompletions doc (Position 0 0)
            liftIO $ do
                Prelude.length completions `shouldSatisfy` (> 0)

        it "provides variable completions" $ runSession lspExe fullLatestClientCaps "tests/data" $ do
            doc <- openDoc "completion_variables.in" "indigo"
            completions <- getCompletions doc (Position 1 4)
            liftIO $ do
                let variables = filter (\i -> i ^. kind == Just CompletionItemKind_Variable) completions
                Prelude.length variables `shouldSatisfy` (> 0)
                let varLabels = map (T.unpack . (^. label)) variables
                varLabels `shouldContain` ["x"]

        it "provides struct field completions after dot" $ runSession lspExe fullLatestClientCaps "tests/data" $ do
            doc <- openDoc "completion_struct_fields.in" "indigo"
            completions <- getCompletions doc (Position 2 10)
            liftIO $ do
                let fields = filter (\i -> i ^. kind == Just CompletionItemKind_Field) completions
                Prelude.length fields `shouldBe` 2
                let fieldLabels = map (T.unpack . (^. label)) fields
                fieldLabels `shouldContain` ["name"]
                fieldLabels `shouldContain` ["age"]

        it "provides field completions for prelude structs" $ runSession lspExe fullLatestClientCaps "tests/data" $ do
            doc <- openDoc "completion_prelude_struct_fields.in" "indigo"
            completions <- getCompletions doc (Position 1 12)
            liftIO $ do
                let fields = filter (\i -> i ^. kind == Just CompletionItemKind_Field) completions
                Prelude.length fields `shouldBe` 1
                let fieldLabels = map (T.unpack . (^. label)) fields
                fieldLabels `shouldContain` ["value"]

        it "provides field completions for variables inside do block" $ runSession lspExe fullLatestClientCaps "tests/data" $ do
            doc <- openDoc "completion_nested_struct_fields.in" "indigo"
            completions <- getCompletions doc (Position 2 16)
            liftIO $ do
                let fields = filter (\i -> i ^. kind == Just CompletionItemKind_Field) completions
                Prelude.length fields `shouldBe` 1
                let fieldLabels = map (T.unpack . (^. label)) fields
                fieldLabels `shouldContain` ["value"]

        it "provides struct completions" $ runSession lspExe fullLatestClientCaps "tests/data" $ do
            doc <- openDoc "completion_structs.in" "indigo"
            completions <- getCompletions doc (Position 1 4)
            liftIO $ do
                Prelude.length completions `shouldSatisfy` (> 0)

        it "resolves completion items" $ runSession lspExe fullLatestClientCaps "tests/data" $ do
            doc <- openDoc "completion_keywords.in" "indigo"
            completions <- getCompletions doc (Position 0 0)
            liftIO $ do
                Prelude.length completions `shouldSatisfy` (> 0)
                case completions of
                    (i : _) -> T.length (i ^. label) `shouldSatisfy` (> 0)
                    [] -> expectationFailure "Expected at least one completion"
