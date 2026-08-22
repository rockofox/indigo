module Position where

import AST qualified
import Data.Text (Text)
import Data.Text qualified as T
import Language.LSP.Protocol.Types (Position (..), Range (..))

indigoToLspPosition :: AST.Position -> Text -> Maybe Position
indigoToLspPosition (AST.Position (startOffset, _)) sourceText
    | startOffset < 0 = Nothing
    | null linesOfInput = Just $ Position 0 0
    | otherwise =
        let (line, column) = getLineAndColumn startOffset linesOfInput
            utf16Col = utf16Length $ T.take column $ linesOfInput !! (line - 1)
         in Just $ Position (fromIntegral (line - 1)) (fromIntegral utf16Col)
  where
    linesOfInput = T.lines sourceText

lspToIndigoOffset :: Position -> Text -> Int
lspToIndigoOffset (Position line col) sourceText =
    let linesOfInput = T.lines sourceText
        lineNum = fromIntegral line
        colNum = fromIntegral col
     in if lineNum >= 0 && lineNum < length linesOfInput
            then
                let precedingLines = take lineNum linesOfInput
                    currentLine = linesOfInput !! lineNum
                    precedingChars = sum (map ((+ 1) . T.length) precedingLines)
                 in precedingChars + codepointsForUtf16 colNum currentLine
            else T.length sourceText

indigoToLspRange :: AST.Position -> Text -> Maybe Range
indigoToLspRange (AST.Position (startOffset, endOffset)) sourceText = do
    startPos <- indigoToLspPosition (AST.Position (startOffset, startOffset)) sourceText
    endPos <- indigoToLspPosition (AST.Position (endOffset, endOffset)) sourceText
    Just $ Range startPos endPos

getLineAndColumn :: Int -> [Text] -> (Int, Int)
getLineAndColumn charPos linesOfInput =
    let go _ _ [] = (0, 0)
        go remaining pos [l] = (pos, min remaining (T.length l))
        go remaining pos (l : ls)
            | remaining <= T.length l = (pos, remaining)
            | otherwise = go (remaining - T.length l - 1) (pos + 1) ls
     in go (max 0 charPos) 1 linesOfInput

utf16Length :: Text -> Int
utf16Length = T.foldl' (\acc c -> acc + (if fromEnum c >= 0x10000 then 2 else 1)) 0

codepointsForUtf16 :: Int -> Text -> Int
codepointsForUtf16 target line = go 0 0 (T.unpack line)
  where
    go cps _ [] = cps
    go cps acc (c : cs)
        | acc >= target = cps
        | otherwise = go (cps + 1) (acc + (if fromEnum c >= 0x10000 then 2 else 1)) cs
