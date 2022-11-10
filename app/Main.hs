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
import TextShow
import Types
import UiKit

import qualified Monomer.Lens as L

testMrIid = Nothing

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree
 where
  bindings = [("Esc", AppQuit)]
  widgetTree = keystroke bindings $ rootWidget wenv model `nodeFocusable` True

handleEvent
  :: Sess.Session
  -> MrMrWenv
  -> MrMrNode
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent sess wenv node model evt = case evt of
  AppInit -> case testMrIid of
    Just iid -> [Event (MrShowDetails (Iid 1129)), Task $ fetchMrChanges sess (Iid 1129)]
    Nothing -> [Event FetchMrList]
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
          & contentState .~ Error err
    ]
  MrShowDetails (Iid iid) ->
    [ Model $
        model & contentState .~ Loading ("Loading MR #" <> showt iid <> "...")
    , Task $ fetchMrChanges sess (Iid iid)
    ]
  MrDetailsFetched iid diffFiles ->
    [ Model $
        model
          & selectedMr .~ Just iid
          & selectedMrDiffs .~ diffFiles
          & contentState .~ Ready
    ]
  ShowMrList ->
    [ Model $
        model
          & selectedMr .~ Nothing
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
        then mrChanges wenv (model ^. selectedMrDiffs)
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
    , appFontDef "Regular" "./assets/fonts/JetBrainsMono-Regular.ttf"
    , appInitEvent AppInit
    ]
  model =
    AppModel
      { _mrs = Nothing
      , _contentState = Loading "Loading MR list..."
      , _selectedMr = Nothing
      , _selectedMrDiffs = []
      }

main :: IO ()
main = mainMrmr
