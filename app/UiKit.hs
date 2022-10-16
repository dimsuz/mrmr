module UiKit where

import Control.Lens
import Data.Default
import Data.Text (Text)
import Monomer as M
import qualified Monomer.Lens as L
import Types

label :: (M.WidgetModel s, M.WidgetEvent e) => Text -> M.WidgetNode s e
label text = M.label text `styleBasic` [textSize 20]

titleText :: (M.WidgetModel s, M.WidgetEvent e) => Text -> M.WidgetNode s e
titleText text = M.label text `styleBasic` [textSize 24]

loadingOverlay text =
  vstack
    [ filler
    , hstack
        [ filler
        , M.label text `styleBasic` [textSize 20, padding 16, border 2 oliveDrab]
        , filler
        ]
    , filler
    ]

errorOverlay text =
  vstack
    [ filler
    , hstack
        [ filler
        , M.label text `styleBasic` [textSize 20, padding 16, border 2 coral]
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
      [ UiKit.label $ mr ^. title
      ]
  row = box_ cfg content
   where
    cfg = [expandContent, onClick (MrShowDetails (mr ^. iid))]
    content =
      rowContent mr
        `styleBasic` [height 80, padding 20, radius 5]
        `styleHover` [bgColor rowBgColor, cursorIcon CursorHand]
