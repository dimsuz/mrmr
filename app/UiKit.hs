module UiKit where

import Control.Lens
import Data.Default
import Data.Text (Text)
import Monomer as M
import qualified Monomer.Lens as L
import Types

borderColor = rgb 219 219 219
diffHeaderBgColor = rgb 250 250 250

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
      [ label (mr ^. title) `styleBasic` [textSize 24]
      ]
  row = box_ cfg content
   where
    cfg = [expandContent, onClick (MrShowDetails (mr ^. iid))]
    content =
      rowContent mr
        `styleBasic` [height 60, padding 10, radius 5]
        `styleHover` [bgColor rowBgColor, cursorIcon CursorHand]

mrChanges :: MrMrWenv -> [DiffFile] -> MrMrNode
mrChanges wenv files = vstack_ [childSpacing_ 20] items
 where
  items = (button "Go back" ShowMrList) : map (diffFile wenv) files

diffFile :: MrMrWenv -> DiffFile -> MrMrNode
diffFile wenv file = layout
 where
  header =
    hstack
      [label "Hello"]
      `styleBasic` [minHeight 45, bgColor diffHeaderBgColor, borderB 1 borderColor, paddingH 16, paddingV 8]
  diff = box (label "Diff") `styleBasic` [height 200, bgColor white]
  layout =
    vstack_
      [sizeReqUpdater clearExtra]
      [header, diff]
      `styleBasic` [border 1 borderColor, radius 5]
