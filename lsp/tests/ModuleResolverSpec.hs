module ModuleResolverSpec (spec) where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isJust)
import Data.Text qualified as T
import ModuleResolver
import Parser (Program (..), initCompilerFlags, needsMain, parseProgram)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = do
    describe "ModuleResolver" $ do
        it "finds modules in workspace" $ withSystemTempDirectory "indigo-test" $ \tmpDir -> do
            let moduleFile = tmpDir </> "Module2.in"
            writeFile moduleFile "module Module2\n\nlet add (a: Int b: Int) : Int = a + b\n"
            resolver <- initModuleResolver tmpDir
            let maybeModule = resolveModule resolver "Module2"
            liftIO $ do
                maybeModule `shouldSatisfy` isJust
                case maybeModule of
                    Just (Program exprs _, _) -> Prelude.length exprs `shouldSatisfy` (> 0)
                    Nothing -> expectationFailure "Expected to find Module2"

        it "builds module cache from source directories" $ withSystemTempDirectory "indigo-test" $ \tmpDir -> do
            let srcDir = tmpDir </> "src"
            createDirectoryIfMissing True srcDir
            let moduleFile = srcDir </> "Module2.in"
            writeFile moduleFile "module Module2\n\nlet add (a: Int b: Int) : Int = a + b\n"
            resolver <- initModuleResolver tmpDir
            let maybeModule = resolveModule resolver "Module2"
            liftIO $ do
                maybeModule `shouldSatisfy` isJust

        it "resolves modules from file path" $ withSystemTempDirectory "indigo-test" $ \tmpDir -> do
            let moduleFile = tmpDir </> "Module2.in"
            writeFile moduleFile "module Module2\n\nlet add (a: Int b: Int) : Int = a + b\n"
            let currentFile = tmpDir </> "main.in"
            writeFile currentFile "import qualified Module2\n"
            resolver <- initModuleResolver tmpDir
            maybeModule <- resolveModuleFromPath resolver "Module2" currentFile
            liftIO $ do
                maybeModule `shouldSatisfy` isJust

        it "extracts imports from AST" $ do
            let source = T.pack "import Module2\nimport qualified Module3\nlet x = 42\n"
            let program = parseProgram source initCompilerFlags{needsMain = False}
            case program of
                Right (Program exprs _) -> do
                    let imports = extractImports exprs
                    Prelude.length imports `shouldBe` 2
                Left _ -> expectationFailure "Failed to parse"




