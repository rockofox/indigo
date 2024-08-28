{-# LANGUAGE DataKinds #-}

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
convertLabelName ('-' : xs) = "minus" ++ convertLabelName xs
convertLabelName ('#' : xs) = "_no_" ++ convertLabelName xs
convertLabelName ('@' : xs) = "_at_" ++ convertLabelName xs
convertLabelName (x : xs) = x : convertLabelName xs
convertLabelName [] = []

mkArgs :: (Show a) => [a] -> String
mkArgs [] = ""
mkArgs (x : xs) = "(v" ++ show x ++ ")" ++ mkArgs xs

mkFormalArgs :: (Show a) => [a] -> String
mkFormalArgs [] = "() => "
mkFormalArgs x = mkFormalArgs' x
  where
    mkFormalArgs' :: (Show a) => [a] -> String
    mkFormalArgs' [] = ""
    mkFormalArgs' (xx : xs) = "(v" ++ show xx ++ ") => " ++ mkFormalArgs' xs

takeOrErr :: Int -> [a] -> [a]
takeOrErr n xs = if Prelude.length xs >= n then Prelude.take n xs else error "Not enough elements"

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
-- compileRegInst ((RegInst (VM.Label label) _) : ((RegInst (VM.PragmaMethodTypes types) _)) : xs) = (if label == "main" then "" else "") ++ "function " ++ convertLabelName label ++ "() {\n" ++ compileRegInst xs
compileRegInst ((RegInst (VM.Label label) vs _) : ((RegInst (VM.PragmaMethodTypes ts) _ _)) : xs) = "const " ++ convertLabelName label ++ " = " ++ formalArgs ++ " {\n" ++ compileRegInst xs
  where
    formalArgs = mkFormalArgs (takeOrErr (Prelude.length ts) vs)
compileRegInst ((RegInst (VM.Jmp label) (a : vxs) rr) : xs) = printf "{v%d,ret} = ({val:v%d,ret} = %s%s);if(ret===true){return}\n" rr a (convertLabelName label) (mkArgs vxs) ++ compileRegInst xs
compileRegInst ((RegInst (VM.Jt label) (a : vxs) rr) : xs) = printf "if(v%d) { ({val:v%d,ret} = %s%s);if(ret===true){return} }\n" rr a (convertLabelName label) (mkArgs vxs) ++ compileRegInst xs
compileRegInst ((RegInst (VM.Jf label) (a : vxs) rr) : xs) = printf "if(!v%d) { ({val:v%d,ret} = %s%s);if(ret===true){return} }\n" rr a (convertLabelName label) (mkArgs vxs) ++ compileRegInst xs
compileRegInst (((RegInst (VM.PragmaMethodTypes types) _ _)) : (RegInst (VM.Call label) args rr) : xs) = printf "v%d = %s%s.val\n" rr (convertLabelName label) (mkArgs $ takeOrErr (Prelude.length types) args) ++ compileRegInst xs
compileRegInst (((RegInst (VM.PragmaMethodTypes types) _ _)) : (RegInst VM.CallS (a : as) rr) : xs) = printf "v%d = v%d%s.val\n" rr a (mkArgs $ takeOrErr (Prelude.length types) as) ++ compileRegInst xs
compileRegInst ((RegInst (VM.PushPf label numArgs) args rr) : xs) = printf "v%d = %s%s\n" rr (convertLabelName label) (mkArgs $ takeOrErr numArgs args) ++ compileRegInst xs
compileRegInst ((RegInst VM.Ret [a] _) : xs) = printf "return {val:v%d,ret:true}\n}\n" a ++ compileRegInst xs
compileRegInst ((RegInst VM.Ret [] _) : xs) = printf "return {ret:true}\n}\n" ++ compileRegInst xs
compileRegInst ((RegInst VM.Exit _ _) : xs) = "return\n}\n" ++ compileRegInst xs
compileRegInst ((RegInst (VM.Mov slot (VM.Lit (VM.DInt value))) _ _) : xs) = printf "v%d = %d\n" slot value ++ compileRegInst xs
compileRegInst ((RegInst (VM.Mov slot (VM.Lit (VM.DString value))) _ _) : xs) = printf "v%d = %s\n" slot (show value) ++ compileRegInst xs
compileRegInst ((RegInst (VM.Mov slot (VM.Lit VM.DNone)) _ _) : xs) = printf "v%d = undefined\n" slot ++ compileRegInst xs
compileRegInst ((RegInst (VM.Mov slot (VM.Lit (VM.DList []))) _ _) : xs) = printf "v%d = []\n" slot ++ compileRegInst xs
compileRegInst ((RegInst (VM.Mov slot (VM.Reg reg)) _ _) : xs) = printf "v%d = _in_clone(v%d)\n" slot reg ++ compileRegInst xs
compileRegInst ((RegInst VM.Add [a, b] rr) : xs) = printf "v%d = v%d + v%d\n" rr b a ++ compileRegInst xs
compileRegInst ((RegInst VM.Sub [a, b] rr) : xs) = printf "v%d = v%d - v%d\n" rr b a ++ compileRegInst xs
compileRegInst ((RegInst VM.Mul [a, b] rr) : xs) = printf "v%d = v%d * v%d\n" rr b a ++ compileRegInst xs
compileRegInst ((RegInst VM.Div [a, b] rr) : xs) = printf "v%d = v%d / v%d\n" rr b a ++ compileRegInst xs
compileRegInst ((RegInst VM.Lt (a : b : _) rr) : xs) = printf "v%d = v%d < v%d\n" rr b a ++ compileRegInst xs -- TODO: ???
compileRegInst ((RegInst VM.Neq (a : b : _) rr) : xs) = printf "v%d = !_in_eq(v%d,v%d)\n" rr b a ++ compileRegInst xs
compileRegInst ((RegInst VM.Eq (a : b : _) rr) : xs) = printf "v%d = _in_eq(v%d,v%d)\n" rr b a ++ compileRegInst xs
compileRegInst ((RegInst VM.And (a : b : _) rr) : xs) = printf "v%d = v%d && v%d\n" rr b a ++ compileRegInst xs
compileRegInst ((RegInst VM.Length (a : _) rr) : xs) = printf "v%d = v%d.length\n" rr a ++ compileRegInst xs
compileRegInst ((RegInst VM.StackLength [a] rr) : xs) = printf "v%d = %d\n" rr a ++ compileRegInst xs
compileRegInst ((RegInst VM.Index (a : b : _) rr) : xs) = printf "v%d = v%d[v%d]\n" rr b a ++ compileRegInst xs
compileRegInst ((RegInst VM.Slice (a : b : _) rr) : xs) = printf "v%d = v%d.slice(v%d, v%d)\n" rr rr b a ++ compileRegInst xs
compileRegInst ((RegInst (VM.PackList listLength) regs rr) : xs) = printf ("v%d = " ++ "[" ++ Prelude.foldl (\acc x -> acc ++ "v" ++ show x ++ ", ") "" (takeOrErr listLength regs) ++ "]\n") rr ++ compileRegInst xs
compileRegInst ((RegInst (VM.Concat numElements) regs rr) : xs) = printf ("v%d = " ++ "[].concat(" ++ Prelude.foldl (\acc x -> acc ++ "v" ++ show x ++ ", ") "" (takeOrErr numElements regs) ++ ")\n") rr ++ compileRegInst xs -- TODO: handle strings
compileRegInst ((RegInst (VM.Builtin VM.Print) (a : _) _) : xs) = printf "console.log(v%d)\n" a ++ compileRegInst xs
compileRegInst ((RegInst (VM.Builtin VM.Print) [] _) : xs) = printf "console.log()\n" ++ compileRegInst xs
compileRegInst (x : _) = error ("Instruction " ++ show x ++ " not implemented in QBE module")

-- compileRegInst x = error "Instruction " ++ show x ++ " not implemented"

-- compileRegInst (RegInst (VM.Mov slot (VM.DInt value)) _) = printf "%%v%d =l alloc4 4\nstorew %d, %%v%d" slot value slot
-- compileRegInst (RegInst (VM.LStore name) _) = printf "%%v%s =l alloc4 4\nstorew %%v0, %%v%s" name name
-- compileRegInst (RegInst (VM.LLoad name) _) = printf "%%v0 =l loadw %%v%s" name
toQbe :: [VM.Instruction] -> ([Char], [RegInst])
toQbe is =
    -- "export function w $main() {\n@start\n" ++
    -- Prelude.foldl (\acc x -> acc <> compileRegInst x <> "\n") "" (toRegInst is)
    -- Prelude.foldl (\acc x -> acc <> compileRegInst x <> "\n") "" (toRegInst is)

    -- Prelude.foldl (\acc x -> acc <> compileRegInst x <> "\n") "" (toRegInst is)
    ("const _in_eq = (a,b) => {if(Array.isArray(a)&&a.length===0) { return b.length===0 } else if (Array.isArray(b)&&b.length===0) {return a.length===0} else { return a===b;}}\nconst _in_clone = (x) => {if(typeof x === 'function') {return x;} else {return structuredClone(x);}}\n" ++ compileRegInst (toRegInst is) ++ "\nmain()", toRegInst is)

-- ++ "\nret\n}\n"
