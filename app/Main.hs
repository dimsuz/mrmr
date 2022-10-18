{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Lens
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe
import Monomer
import Network
import Network.Wreq.Session as Sess
import Path
import Types
import UiKit

import qualified Monomer.Lens as L

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree
 where
  bindings = [("Esc", AppQuit)]
  widgetTree = keystroke bindings $ (rootWidget wenv model) `styleBasic` [padding 10] `nodeFocusable` True

handleEvent
  :: Sess.Session
  -> MrMrWenv
  -> MrMrNode
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent sess wenv node model evt = case evt of
  AppInit -> [Event (MrShowDetails (Iid 33))] -- [Event FetchMrList]
  AppQuit -> [exitApplication]
  FetchMrList ->
    [ Model $
        model & contentState .~ Loading "Loading MR List..."
    , Task $ fetchMrList sess
    ]
  MrListResult mrList ->
    [ Model $
        model
          & mrs ?~ mrList
          & contentState .~ Ready
    ]
  MrListError err ->
    [ Model $
        model
          & contentState .~ (Error err)
    ]
  MrShowDetails iid ->
    [ Model $
        model
          & selectedMr .~ (Just iid)
          & contentState .~ Ready -- TODO remove, used only without FetchMrList stage
    ]
  ShowMrList ->
    [ Model $
        model
          & selectedMr .~ Nothing
    ]

mockDiffFiles =
  [ DiffFile
      { _oldFile = [absfile|/home/dima/foo.txt|]
      , _newFile = [absfile|/home/dima/foo.txt|]
      , _diff = ""
      }
  ]

rootWidget
  :: MrMrWenv
  -> AppModel
  -> MrMrNode
rootWidget wenv model =
  case model ^. contentState of
    Loading text -> loadingOverlay text
    Ready ->
      if has _Just $ model ^. selectedMr
        then mrChanges wenv mockDiffFiles
        else mrListWidget wenv (model ^. mrs . _Just)
    Error text -> errorOverlay text

mrListWidget wenv mrList =
  vscroll_
    [wheelRate 30.0]
    ( vstack (mrListRow wenv <$> mrList)
    )

mainMrmr :: IO ()
mainMrmr = do
  sess <- Sess.newSession
  startApp model (handleEvent sess) buildUI config
 where
  config =
    [ appWindowTitle "Mrmr"
    , appWindowIcon "./assets/images/icon.png"
    , appWindowState (MainWindowNormal (1200, 800))
    , appTheme lightTheme
    , appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf"
    , appInitEvent AppInit
    ]
  model =
    AppModel
      { _mrs = Nothing
      , _contentState = Loading "Loading MR list..."
      , _selectedMr = Nothing
      }

main :: IO ()
main = mainMrmr

-- main = Tutorial02_Styling.main02
-- main = Todo.todoMain
