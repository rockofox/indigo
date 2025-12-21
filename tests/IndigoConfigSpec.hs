module IndigoConfigSpec (spec) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson qualified as Aeson
import Data.Maybe (isJust)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import IndigoConfig
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = do
    describe "IndigoConfig" $ do
        it "parses valid indigo.json" $ do
            let json = "{\"main\": \"examples/main.in\", \"sourceDirs\": [\"src\", \"lib\"]}"
            case Aeson.eitherDecodeStrict (encodeUtf8 $ T.pack json) of
                Right config -> do
                    mainFile config `shouldBe` Just "examples/main.in"
                    sourceDirs config `shouldBe` ["src", "lib"]
                Left err -> expectationFailure $ "Failed to parse: " ++ err

        it "parses indigo.json with only main" $ do
            let json = "{\"main\": \"examples/main.in\"}"
            case Aeson.eitherDecodeStrict (encodeUtf8 $ T.pack json) of
                Right config -> do
                    mainFile config `shouldBe` Just "examples/main.in"
                    sourceDirs config `shouldBe` []
                Left err -> expectationFailure $ "Failed to parse: " ++ err

        it "parses indigo.json with only sourceDirs" $ do
            let json = "{\"sourceDirs\": [\"src\"]}"
            case Aeson.eitherDecodeStrict (encodeUtf8 $ T.pack json) of
                Right config -> do
                    mainFile config `shouldBe` Nothing
                    sourceDirs config `shouldBe` ["src"]
                Left err -> expectationFailure $ "Failed to parse: " ++ err

        it "parses empty indigo.json" $ do
            let json = "{}"
            case Aeson.eitherDecodeStrict (encodeUtf8 $ T.pack json) of
                Right config -> do
                    mainFile config `shouldBe` Nothing
                    sourceDirs config `shouldBe` []
                Left err -> expectationFailure $ "Failed to parse: " ++ err

        it "finds indigo.json in current directory" $ withSystemTempDirectory "indigo-test" $ \tmpDir -> do
            let configFile = tmpDir </> "indigo.json"
            writeFile configFile "{\"main\": \"main.in\"}"
            maybeConfigFile <- findConfigFile tmpDir
            liftIO $ maybeConfigFile `shouldBe` Just configFile

        it "finds indigo.json in parent directory" $ withSystemTempDirectory "indigo-test" $ \tmpDir -> do
            let configFile = tmpDir </> "indigo.json"
            let subDir = tmpDir </> "subdir"
            createDirectoryIfMissing True subDir
            writeFile configFile "{\"main\": \"main.in\"}"
            maybeConfigFile <- findConfigFile subDir
            liftIO $ maybeConfigFile `shouldBe` Just configFile

        it "loads config from file" $ withSystemTempDirectory "indigo-test" $ \tmpDir -> do
            let configFile = tmpDir </> "indigo.json"
            writeFile configFile "{\"main\": \"main.in\", \"sourceDirs\": [\"src\"]}"
            maybeConfig <- loadConfig configFile
            liftIO $ do
                maybeConfig `shouldSatisfy` isJust
                case maybeConfig of
                    Just config -> do
                        mainFile config `shouldBe` Just "main.in"
                        sourceDirs config `shouldBe` ["src"]
                    Nothing -> expectationFailure "Failed to load config"

        it "returns Nothing for non-existent file" $ withSystemTempDirectory "indigo-test" $ \tmpDir -> do
            let configFile = tmpDir </> "nonexistent.json"
            maybeConfig <- loadConfig configFile
            liftIO $ maybeConfig `shouldBe` Nothing

        it "returns Nothing for invalid JSON" $ withSystemTempDirectory "indigo-test" $ \tmpDir -> do
            let configFile = tmpDir </> "indigo.json"
            writeFile configFile "{invalid json}"
            maybeConfig <- loadConfig configFile
            liftIO $ maybeConfig `shouldBe` Nothing

        it "returns Nothing for non-object JSON" $ withSystemTempDirectory "indigo-test" $ \tmpDir -> do
            let configFile = tmpDir </> "indigo.json"
            writeFile configFile "\"not an object\""
            maybeConfig <- loadConfig configFile
            liftIO $ maybeConfig `shouldBe` Nothing

        it "handles empty sourceDirs array" $ do
            let json = "{\"main\": \"main.in\", \"sourceDirs\": []}"
            case Aeson.eitherDecodeStrict (encodeUtf8 $ T.pack json) of
                Right config -> do
                    mainFile config `shouldBe` Just "main.in"
                    sourceDirs config `shouldBe` []
                Left err -> expectationFailure $ "Failed to parse: " ++ err

        it "handles multiple sourceDirs" $ do
            let json = "{\"sourceDirs\": [\"src\", \"lib\", \"tests\"]}"
            case Aeson.eitherDecodeStrict (encodeUtf8 $ T.pack json) of
                Right config -> do
                    sourceDirs config `shouldBe` ["src", "lib", "tests"]
                Left err -> expectationFailure $ "Failed to parse: " ++ err

        it "finds indigo.json in grandparent directory" $ withSystemTempDirectory "indigo-test" $ \tmpDir -> do
            let configFile = tmpDir </> "indigo.json"
            let subDir = tmpDir </> "subdir" </> "nested"
            createDirectoryIfMissing True subDir
            writeFile configFile "{\"main\": \"main.in\"}"
            maybeConfigFile <- findConfigFile subDir
            liftIO $ maybeConfigFile `shouldBe` Just configFile

        it "returns Nothing when no indigo.json found" $ withSystemTempDirectory "indigo-test" $ \tmpDir -> do
            maybeConfigFile <- findConfigFile tmpDir
            liftIO $ maybeConfigFile `shouldBe` Nothing

        it "defaultConfig has correct defaults" $ do
            mainFile defaultConfig `shouldBe` Nothing
            sourceDirs defaultConfig `shouldBe` []

        it "handles main file with path separators" $ do
            let json = "{\"main\": \"examples/subdir/main.in\"}"
            case Aeson.eitherDecodeStrict (encodeUtf8 $ T.pack json) of
                Right config -> do
                    mainFile config `shouldBe` Just "examples/subdir/main.in"
                Left err -> expectationFailure $ "Failed to parse: " ++ err

        it "handles sourceDirs with path separators" $ do
            let json = "{\"sourceDirs\": [\"src/main\", \"lib/utils\"]}"
            case Aeson.eitherDecodeStrict (encodeUtf8 $ T.pack json) of
                Right config -> do
                    sourceDirs config `shouldBe` ["src/main", "lib/utils"]
                Left err -> expectationFailure $ "Failed to parse: " ++ err

        it "stops at filesystem root when searching for config" $ do
            maybeConfigFile <- findConfigFile "/"
            liftIO $ case maybeConfigFile of
                Just path -> path `shouldBe` "/indigo.json"
                Nothing -> return ()





