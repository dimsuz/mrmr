module Parse where

import Data.Text
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L
import TextShow
import Types

type Parser = Parsec Void Text

decodeHunkHeader :: Text -> Either Text HunkHeader
decodeHunkHeader text = case parse hunkHeader "" text of
  Left bundle -> Left (pack (errorBundlePretty bundle))
  Right header -> Right header
 where
  hunkHeader :: Parser HunkHeader
  hunkHeader =
    HunkHeader
      <$> (string "@@ -" *> L.decimal <* char ',')
      <*> L.decimal
      <*> (string " +" *> L.decimal <* char ',')
      <*> (L.decimal <* " @@ ")
      <*> (pack <$> many alphaNumChar)

encodeHunkHeader :: HunkHeader -> Text
encodeHunkHeader (HunkHeader os oc ns nc t) =
  "@@ -"
    <> showt os
    <> ","
    <> showt oc
    <> " +"
    <> showt ns
    <> ","
    <> showt nc
    <> " @@ "
    <> t
