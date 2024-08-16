{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module QBE where

import Data.Either (partitionEithers)
import Data.List.NonEmpty
import Data.Text
import Data.Text.IO qualified as Text
import Data.Text.Lazy qualified as Text
import Data.Text.Short as ShortText
import Debug.Trace
import Language.QBE
import Prettyprinter
import Prettyprinter.Render.Text
import RegInst
import Text.Printf
import VM qualified

-- tempIdent :: [Char] -> Val
-- tempIdent str = ValTemporary (Ident @'Temporary $ ShortText.pack str)
--
-- compileRegInst :: RegInst -> Either [Inst] [Block]
-- compileRegInst (RegInst (VM.Add) [a,b]) = Left [BinaryOp (Assignment "v0" Word) Add (tempIdent ("r"++show a)) (tempIdent ("r"++show b))]
-- compileRegInst (RegInst (VM.Mov a b) _) = Left [BinaryOp (Assignment "v0" Word) Alloc4 ""]
-- compileRegInst x = error $ "Not implemented: " ++ show x
--
-- render :: Pretty a => a -> Text
-- render = renderStrict . layoutPretty defaultLayoutOptions . pretty
--
-- block :: Block
-- block = Block "start" [] [] (Ret Nothing)
--
--
-- test :: IO ()
-- test = Text.putStrLn $ render $ Program [] [] [FuncDef [] Nothing "main" Nothing []  NoVariadic (fromList [block])]
--
--
-- toQbe :: [VM.Instruction] -> Text
-- toQbe is = do
--     let (insts, blocks) =  (partitionEithers (Prelude.map compileRegInst (toRegInst is)))
--     let (insts', blocks') = (Prelude.concat insts, Prelude.concat blocks)
--     render $ Program [] [] [FuncDef [] Nothing "main" Nothing []  NoVariadic (fromList blocks')]
--
--
--
convertLabelName :: String -> String
convertLabelName ('+' : xs) = "plus" ++ convertLabelName xs
convertLabelName ('#' : xs) = "_no_" ++ convertLabelName xs
convertLabelName (x : xs) = x : convertLabelName xs
convertLabelName [] = []

mkArgs :: (Show a) => [a] -> String
mkArgs [] = ""
mkArgs (x : xs) = "(" ++ show x ++ ")" ++ mkArgs xs

compileRegInst :: [RegInst] -> String
compileRegInst [] = ""
-- compileRegInst ((RegInst (VM.Label label) _):((RegInst (VM.PragmaMethodTypes types) _)):xs) = (if label == "main" then "export " else "") ++ "function w $" ++ convertLabelName label ++ "(w %a, w %b) {\n@start\n" ++ compileRegInst xs
-- compileRegInst ((RegInst (VM.Jmp label) _):xs)= "jmp @" ++ convertLabelName label ++ "\n"++ compileRegInst xs
-- compileRegInst ((RegInst (VM.Call label) _):xs)= "jmp @" ++ convertLabelName label ++ "\n"++ compileRegInst xs
-- compileRegInst ((RegInst VM.Ret _):xs)= "ret 0\n}\n"++ compileRegInst xs
-- compileRegInst ((RegInst VM.Exit _):xs)= "ret 0\n}\n"++  compileRegInst xs
-- compileRegInst ((RegInst (VM.Mov slot (VM.DInt value)) _):xs)= printf "%%v%d =d add d_0, d_%d\n" slot value ++ compileRegInst xs
-- compileRegInst ((RegInst VM.Add [a,b]):xs)= printf "%%v3 =l add %%v%d, %%v%d\n" a b++ compileRegInst xs
-- compileRegInst ((RegInst VM.Sub [a,b]):xs)= printf "%%v3 =l sub %%v%d, %%v%d\n" a b++ compileRegInst xs
-- compileRegInst x = error $ "Not implemented: " ++ show x

-- Compile to Javascript
compileRegInst ((RegInst (VM.Label label) _) : ((RegInst (VM.PragmaMethodTypes types) _)) : xs) = (if label == "main" then "" else "") ++ "function " ++ convertLabelName label ++ "() {\n" ++ compileRegInst xs
compileRegInst ((RegInst (VM.Jmp label) _) : xs) = printf "return %s()\n" (convertLabelName label) ++ compileRegInst xs
compileRegInst (((RegInst (VM.PragmaMethodTypes types) _)) : (RegInst (VM.Call label) args) : xs) = printf "%s%s\n" (convertLabelName label) (mkArgs $ Prelude.take (Prelude.length types) args) ++ compileRegInst xs
compileRegInst ((RegInst VM.Ret _) : xs) = "return \n}\n" ++ compileRegInst xs
compileRegInst ((RegInst VM.Exit _) : xs) = "return\n}\n" ++ compileRegInst xs
compileRegInst ((RegInst (VM.Mov slot (VM.DInt value)) _) : xs) = printf "v%d = %v\n" slot value ++ compileRegInst xs
compileRegInst ((RegInst VM.Add [a, b]) : xs) = printf "st = v%d + v%d\n" a b ++ compileRegInst xs
compileRegInst ((RegInst VM.Sub [a, b]) : xs) = printf "st = v%d - v%d\n" a b ++ compileRegInst xs
compileRegInst ((RegInst (VM.Builtin VM.Print) [a]) : xs) = printf "console.log(v%d)\n" a ++ compileRegInst xs
compileRegInst (x : xs) = compileRegInst xs

-- compileRegInst (RegInst (VM.Mov slot (VM.DInt value)) _) = printf "%%v%d =l alloc4 4\nstorew %d, %%v%d" slot value slot
-- compileRegInst (RegInst (VM.LStore name) _) = printf "%%v%s =l alloc4 4\nstorew %%v0, %%v%s" name name
-- compileRegInst (RegInst (VM.LLoad name) _) = printf "%%v0 =l loadw %%v%s" name
toQbe :: [VM.Instruction] -> [Char]
toQbe is =
    -- "export function w $main() {\n@start\n" ++
    -- Prelude.foldl (\acc x -> acc <> compileRegInst x <> "\n") "" (toRegInst is)
    -- Prelude.foldl (\acc x -> acc <> compileRegInst x <> "\n") "" (toRegInst is)

    -- Prelude.foldl (\acc x -> acc <> compileRegInst x <> "\n") "" (toRegInst is)
    compileRegInst (toRegInst is) ++ "\nmain()"

-- ++ "\nret\n}\n"
