module LLVM.Sanitizer
    ( sanitizeLabel
    , escapeLLVMString
    )
where

import Data.ByteString qualified as BS
import Data.Char (chr, isAlphaNum, ord, toUpper)
import Data.List (isPrefixOf)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word8)
import Numeric (showHex)

sanitizeLabel :: String -> String
sanitizeLabel name
    | "indigo_lbl_" `isPrefixOf` name = name
    | otherwise = "indigo_lbl_" ++ concatMap sanitizeChar name
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

escapeLLVMString :: String -> (String, Int)
escapeLLVMString str =
    let bs = TE.encodeUtf8 (T.pack str)
        bytes = BS.unpack bs
        escaped = concatMap escapeByte bytes ++ "\\00"
        totalLen = length bytes + 1
     in ("c\"" ++ escaped ++ "\"", totalLen)
  where
    escapeByte :: Word8 -> String
    escapeByte b
        | b == 34 = "\\22"
        | b == 92 = "\\5C"
        | b >= 32 && b <= 126 = [chr (fromIntegral b)]
        | otherwise =
            let hex = showHex b ""
                padded = if length hex == 1 then "0" ++ hex else hex
             in "\\" ++ map toUpper padded
