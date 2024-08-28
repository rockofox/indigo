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
                #let myhead ([]: [Any]) = 0
                #let myhead ((x:xs): [Any]) = x
                #let main = print myhead [1,2,3]

                #let add (a: Int b: Int) = a + b
                #let main = print add 2,3

                #let subt (a: Int b: Int) = (a) - b
                #let main = print subt 2,3
                let main = println map (\x->x+1), [1,2,3]
            |]
    putStrLn $ "Instructions:\n" ++ printAssembly (Data.Vector.fromList instructions) False
    let (qbeCode, regInst) = toQbe instructions
    putStrLn $ "Instructions 2:\n" ++ printRegInstProgram regInst
    prettierResult <- readProcess "prettier" ["--parser", "babel"] qbeCode
    batResult <- readProcess "/run/current-system/sw/bin/bat" ["-l", "javascript", "-n", "-f", "-"] prettierResult
    _ <- readProcess "pbcopy" [] prettierResult
    putStrLn $ "QBE code:\n" ++ batResult
    -- result <- readProcess "qbe" ["-o", "-", "-"] qbeCode

    result <- readProcess "node" ["-"] prettierResult
    putStrLn $ "Result: " ++ result
    -- result `shouldBe` "5\n"
    result `shouldNotBe` ""

-- print regInst
