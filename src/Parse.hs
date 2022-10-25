module Parse where

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
decodeHunkHeader text = case parse hunkHeader "" text of
  Left bundle -> Left (pack (errorBundlePretty bundle))
  Right header -> Right header
 where
  hunkHeader :: Parser HunkHeader
  hunkHeader = do
    os <- string "@@ -" *> L.decimal
    oc <- fromMaybe 1 <$> optional (char ',' *> L.decimal)
    ns <- string " +" *> L.decimal
    nc <- (fromMaybe 1 <$> optional (char ',' *> L.decimal)) <* " @@ "
    HunkHeader os oc ns nc <$> takeRest

encodeHunkHeader :: HunkHeader -> Text
encodeHunkHeader (HunkHeader os oc ns nc t) =
  "@@ -"
    <> old
    <> " +"
    <> new
    <> " @@ "
    <> t
 where
  old = if oc == 1 then showt os else showt os <> "," <> showt oc
  new = if nc == 1 then showt ns else showt ns <> "," <> showt nc
