module UiKit where

import Data.Text (Text)
import Monomer as M

label :: (M.WidgetModel s, M.WidgetEvent e) => Text -> M.WidgetNode s e
label text = M.label text `styleBasic` [textSize 20]

titleText :: (M.WidgetModel s, M.WidgetEvent e) => Text -> M.WidgetNode s e
titleText text = M.label text `styleBasic` [textSize 24]

loadingOverlay text = vstack [
  filler,
  hstack [
        filler,
        M.label text `styleBasic` [textSize 20, padding 16, border 2 oliveDrab],
        filler
  ],
  filler
  ]
