{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module QBESpec where

import AST qualified
import BytecodeCompiler
import Control.Monad.State
import Data.Either (partitionEithers)
import Data.Functor
import Data.List.NonEmpty
import Data.Text
import Data.Text.IO qualified as Text
import Data.Text.Lazy qualified as Text
import Data.Vector qualified
import Language.QBE
import Optimizer (optimize)
import Parser qualified
import Prettyprinter
import Prettyprinter.Render.Text
import QBE
import RegInst
import System.Process
import Test.Hspec
import Text.Megaparsec
import Text.RawString.QQ (r)
import VM (printAssembly)
import VM qualified

compile :: String -> IO [VM.Instruction]
compile prog = do
    let p = Parser.parseProgram (Data.Text.pack prog) Parser.initCompilerFlags
    case p of
        Left err -> error $ errorBundlePretty err
        Right program ->
            evalStateT
                (compileProgram program <&> optimize)
                (initCompilerState program)

spec :: Spec
spec = it "TODO" $ do
    instructions <-
        compile
            [r|
                let main = 2 + 3
            |]
    let qbeCode = toQbe instructions
    putStrLn $ "Instructions:\n" ++ printAssembly (Data.Vector.fromList instructions) False
    putStrLn $ "QBE code: " ++ qbeCode
    -- result <- readProcess "qbe" ["-o", "-", "-"] qbeCode
    result <- readProcess "node" ["-"] qbeCode
    putStrLn $ "Result: " ++ result
    result `shouldNotBe` ""
