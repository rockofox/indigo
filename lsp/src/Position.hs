module Position where

import AST qualified
import Data.Text (Text)
import Data.Text qualified as T
import Language.LSP.Protocol.Types (Position (..), Range (..))

indigoToLspPosition :: AST.Position -> Text -> Maybe Position
indigoToLspPosition (AST.Position (startOffset, _)) sourceText =
    let linesOfInput = T.lines sourceText
        (line, column) = getLineAndColumn startOffset linesOfInput
     in if line > 0 && line <= length linesOfInput
            then Just $ Position (fromIntegral (line - 1)) (fromIntegral column)
            else Nothing

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
                 in precedingChars + colNum
            else 0

indigoToLspRange :: AST.Position -> Text -> Maybe Range
indigoToLspRange (AST.Position (startOffset, endOffset)) sourceText = do
    startPos <- indigoToLspPosition (AST.Position (startOffset, startOffset)) sourceText
    endPos <- indigoToLspPosition (AST.Position (endOffset, endOffset)) sourceText
    Just $ Range startPos endPos

getLineAndColumn :: Int -> [Text] -> (Int, Int)
getLineAndColumn charPos linesOfInput =
    let go _ _ [] = (0, 0)
        go remaining pos (l : ls)
            | remaining <= T.length l = (pos, remaining)
            | otherwise = go (remaining - T.length l - 1) (pos + 1) ls
     in go charPos 1 linesOfInput
