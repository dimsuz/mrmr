module Parse where

import Data.Text
import TextShow
import Types

decodeHunkHeader :: Text -> HunkHeader
decodeHunkHeader text =
  HunkHeader
    { _oldStart = 23
    , _oldCount = 7
    , _newStart = 23
    , _newCount = 6
    , _text = ""
    }

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
