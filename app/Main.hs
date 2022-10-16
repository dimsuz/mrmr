{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe
import Monomer hiding (label)
import Types
import UiKit

import qualified Monomer.Lens as L

makeLenses 'AppModel

mockMrs =
  NonEmpty.fromList
    [ MergeRequest{_iid = Iid 33, _title = "Merge Request #1"}
    , MergeRequest{_iid = Iid 34, _title = "Merge Request #2"}
    , MergeRequest{_iid = Iid 35, _title = "Merge Request #3"}
    , MergeRequest{_iid = Iid 36, _title = "Merge Request #4"}
    , MergeRequest{_iid = Iid 37, _title = "Merge Request #5"}
    ]

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree
 where
  bindings = [("Esc", AppQuit)]
  rootWidget =
    if isJust (_mrs model)
      then loadingOverlay "MR List"
      else loadingOverlay "Loading MR list..."
  widgetTree = keystroke bindings $ rootWidget `styleBasic` [padding 10] `nodeFocusable` True
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
  config =
    [ appWindowTitle "Mrmr"
    , appWindowIcon "./assets/images/icon.png"
    , appWindowState (MainWindowNormal (1200, 800))
    , appTheme lightTheme
    , appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf"
    , appInitEvent AppInit
    , appExitEvent AppQuit
    ]
  model =
    AppModel
      { _mrs = Nothing
      , _contentState = Ready
      , _selectedMr = Nothing
      }

main :: IO ()
main = mainMrmr

-- main = Tutorial02_Styling.main02
-- main = Todo.todoMain
