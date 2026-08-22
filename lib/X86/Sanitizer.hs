module X86.Sanitizer where

import Data.Char (isAlphaNum, ord)
import Numeric (showHex)

sanitizeLabel :: String -> String
sanitizeLabel name = "indigo_lbl_" ++ concatMap sanitizeChar name
  where
    sanitizeChar :: Char -> String
    sanitizeChar c
        | isAlphaNum c = [c]
        | c == '_' = "_"
        | c == '#' = "_hash_"
        | c == '@' = "_at_"
        | c == '+' = "_op_add_"
        | c == '-' = "_op_sub_"
        | c == '*' = "_op_mul_"
        | c == '/' = "_op_div_"
        | c == '%' = "_op_mod_"
        | c == '^' = "_op_pow_"
        | c == '<' = "_op_lt_"
        | c == '>' = "_op_gt_"
        | c == '=' = "_op_eq_"
        | c == '!' = "_op_bang_"
        | c == '&' = "_op_and_"
        | c == '|' = "_op_or_"
        | c == '~' = "_op_tilde_"
        | c == '.' = "_dot_"
        | c == ':' = "_colon_"
        | c == '$' = "_dollar_"
        | c == '?' = "_qmark_"
        | c == '\'' = "_prime_"
        | c == '[' = "_lbrack_"
        | c == ']' = "_rbrack_"
        | c == '(' = "_lparen_"
        | c == ')' = "_rparen_"
        | c == ',' = "_comma_"
        | c == ' ' = "_"
        | otherwise = "_u" ++ showHex (ord c) "_"

escapeCString :: String -> String
escapeCString [] = ""
escapeCString ('\\' : xs) = "\\\\" ++ escapeCString xs
escapeCString ('\"' : xs) = "\\\"" ++ escapeCString xs
escapeCString ('\n' : xs) = "\\n" ++ escapeCString xs
escapeCString ('\t' : xs) = "\\t" ++ escapeCString xs
escapeCString ('\r' : xs) = "\\r" ++ escapeCString xs
escapeCString ('\0' : xs) = "\\0" ++ escapeCString xs
escapeCString (c : xs)
    | c >= ' ' && c <= '~' = c : escapeCString xs
    | otherwise =
        let hex = showHex (ord c) ""
            padded = if length hex == 1 then "0" ++ hex else hex
         in "\\x" ++ padded ++ escapeCString xs
