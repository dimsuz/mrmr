{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens
import Monomer hiding (label)
import UiKit
import Types

import qualified Monomer.Lens as L

makeLenses 'AppModel

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree where
  bindings = [("Esc", AppQuit)]
  widgetTree = keystroke bindings $ vstack [
    loadingOverlay "Loading MR list..."
    ] `styleBasic` [padding 10] `nodeFocusable` True
handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> []

mainMrmr :: IO ()
mainMrmr = do
  startApp model handleEvent buildUI config
  where
    config = [
      appWindowTitle "Mrmr",
      appWindowIcon "./assets/images/icon.png",
      appWindowState (MainWindowNormal (1200, 800)),
      appTheme lightTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      appInitEvent AppInit,
      appExitEvent AppQuit
      ]
    model = AppModel {
      _mrs = Nothing,
      _contentState = Ready,
      _selectedMr = Nothing
    }

main :: IO ()
main = mainMrmr
-- main = Tutorial02_Styling.main02
-- main = Todo.todoMain
