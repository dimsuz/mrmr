module Parse (
  decodeHunkHeader,
  encodeHunkHeader,
  decodeFileDiff,
) where

import Data.Maybe
import Data.Text
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L
import TextShow
import Types

type Parser = Parsec Void Text

decodeHunkHeader :: Text -> Either Text HunkHeader
decodeHunkHeader text = case parse hunkHeaderParser "" text of
  Left bundle -> Left (pack (errorBundlePretty bundle))
  Right header -> Right header

encodeHunkHeader :: HunkHeader -> Text
encodeHunkHeader (HunkHeader os oc ns nc t) =
  "@@ -"
    <> old
    <> " +"
    <> new
    <> " @@"
    <> maybe "" (\v -> v) t
 where
  old = if oc == 1 then showt os else showt os <> "," <> showt oc
  new = if nc == 1 then showt ns else showt ns <> "," <> showt nc

decodeHunk :: Text -> Either Text DiffHunk
decodeHunk text = case parse hunkParser "" text of
  Left bundle -> Left (pack (errorBundlePretty bundle))
  Right hunk -> Right hunk

decodeFileDiff :: Text -> Either Text [DiffHunk]
decodeFileDiff text = case parse fileDiffParser "" text of
  Left bundle -> Left (pack (errorBundlePretty bundle))
  Right result -> Right result

fileDiffParser :: Parser [DiffHunk]
fileDiffParser = some hunkParser

hunkParser :: Parser DiffHunk
hunkParser = do
  header <- hunkHeaderParser <* newline
  lines <- many lineParser
  pure $ DiffHunk header lines

hunkHeaderParser :: Parser HunkHeader
hunkHeaderParser = do
  os <- string "@@ -" *> L.decimal
  oc <- fromMaybe 1 <$> optional (char ',' *> L.decimal)
  ns <- string " +" *> L.decimal
  nc <- (fromMaybe 1 <$> optional (char ',' *> L.decimal)) <* " @@"
  HunkHeader os oc ns nc <$> optional (takeWhile1P Nothing (\t -> t /= '\n'))

lineParser :: Parser Text
lineParser =
  notFollowedBy hunkHeaderParser *> (takeWhile1P Nothing (/= '\n')) <* newline
