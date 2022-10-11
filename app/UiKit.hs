module UiKit where

import Data.Text (Text)
import Monomer as M

label :: (M.WidgetModel s, M.WidgetEvent e) => Text -> M.WidgetNode s e
label text = M.label text `styleBasic` [textSize 20]
