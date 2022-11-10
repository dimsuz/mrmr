module UiKit where

import Control.Lens
import Data.Default
import Data.Text (Text, isPrefixOf, pack)
import Monomer as M
import qualified Monomer.Lens as L
import Path (toFilePath)
import TextShow
import Types

borderColor = rgb 219 219 219
diffHeaderBgColor = rgb 250 250 250
hunkLineBgColor = rgb 250 250 250
hunkLineBgColorHover = rgb 225 216 242
hunkHeaderBgColor = rgb 240 240 240
hunkLineHeaderBgColor = rgb 219 219 219
diffRemoveLineColor = rgb 251 233 235
diffAddLineColor = rgb 236 253 240

titleText :: (M.WidgetModel s, M.WidgetEvent e) => Text -> M.WidgetNode s e
titleText text = M.label text `styleBasic` [textSize 24]

loadingOverlay text =
  vstack
    [ filler
    , hstack
        [ filler
        , label text `styleBasic` [textSize 20, padding 16, border 2 oliveDrab]
        , filler
        ]
    , filler
    ]

errorOverlay text =
  vstack
    [ filler
    , hstack
        [ filler
        , label text `styleBasic` [textSize 20, padding 16, border 2 coral]
        , filler
        ]
    , filler
    ]

mrListRow :: MrMrWenv -> MergeRequest -> MrMrNode
mrListRow wenv mr = row
 where
  rowBgColor = wenv ^. L.theme . L.userColorMap . at "rowBgColor" . non def
  rowContent mr =
    hstack
      [ label (mr ^. title) `styleBasic` [textSize 18]
      ]
  row = box_ cfg content
   where
    cfg = [expandContent, onClick (MrShowDetails (mr ^. iid))]
    content =
      rowContent mr
        `styleBasic` [height 60, padding 10, radius 5]
        `styleHover` [bgColor rowBgColor, cursorIcon CursorHand]

mrChanges :: MrMrWenv -> [DiffFile] -> MrMrNode
mrChanges wenv files = content
 where
  content =
    vstack_
      [childSpacing_ 20]
      [ button "Go back" ShowMrList
      , vscroll_ [wheelRate 30.0] changes
      ]
  changes = vstack_ [childSpacing_ 20] fileList `styleBasic` [paddingH 10]
  fileList = map (diffFile wenv) files

diffLineColor line
  | "+" `isPrefixOf` line = diffAddLineColor
  | "-" `isPrefixOf` line = diffRemoveLineColor
  | otherwise = white

fileHeaderText :: DiffFile -> Text
fileHeaderText file =
  if old /= new
    then pack $ (toFilePath old) <> " -> " <> (toFilePath new)
    else pack (toFilePath old)
 where
  old = file ^. oldFile
  new = file ^. newFile

hunkLineNumbers :: DiffHunk -> [(Text, Text)]
hunkLineNumbers hunk = map (\i -> ("10", "20")) (hunk ^. dhLines)

oldLineAt :: HunkHeader -> Text -> Int -> Int
oldLineAt header line index = start + count
 where
  isOld = ("-" `isPrefixOf` line) || (" " `isPrefixOf` line)
  start = header ^. oldStart
  count = header ^. oldCount

diffFile :: MrMrWenv -> DiffFile -> MrMrNode
diffFile wenv file = layout
 where
  header =
    label_ (fileHeaderText file) [multiline]
      `styleBasic` [minHeight 45, bgColor diffHeaderBgColor, borderB 1 borderColor, paddingH 16, paddingV 8]
  lineLabel text =
    box_ [alignRight] (label text)
      `styleBasic` [paddingV 6, paddingH 8, borderR 1 borderColor, bgColor hunkLineBgColor]
      `styleHover` [bgColor hunkLineBgColorHover, cursorIcon CursorHand]
  lineNumbersRow old new =
    hgrid [lineLabel old, lineLabel new]
  -- TODO use computeTextSize + padding to calculate height instead of hardcoding minHeight
  hunkLineHeader = spacer `styleBasic` [minHeight 28, bgColor hunkLineHeaderBgColor]
  hunkHeader = spacer `styleBasic` [minHeight 28, bgColor hunkHeaderBgColor]
  lineNumbers hunk =
    vstack $
      hunkLineHeader
        : map
          (\(old, new) -> lineNumbersRow old new)
          (hunkLineNumbers hunk)
  diffLine line =
    label line
      `styleBasic` [paddingV 6, bgColor (diffLineColor line)]
      `styleHover` [bgColor hunkLineBgColorHover]
  diffLines hunk =
    vstack_
      [sizeReqUpdater (fixedToExpandW 1.0)]
      (hunkHeader : (map diffLine (hunk ^. dhLines)))
  diffHunk hunk =
    hstack [lineNumbers hunk, diffLines hunk]
  diff = vstack (map diffHunk (file ^. hunks)) `styleBasic` [bgColor white]
  layout =
    vstack_
      [sizeReqUpdater clearExtra]
      [header, diff]
      `styleBasic` [border 1 borderColor, radius 5]
