module Main where

import Control.Lens
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe
import Monomer
import Network
import Network.Wreq.Session as Sess
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
  AppInit -> [Event FetchMrList]
  FetchMrList ->
    [ Model $
        model & contentState .~ Loading "Loading MR List..."
    , Task $ fetchMrList sess
    ]
  MrListResult mrList ->
    [ Model $
        model
          & mrs .~ (Just mrList)
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
    ]

rootWidget
  :: MrMrWenv
  -> AppModel
  -> MrMrNode
rootWidget wenv model =
  case model ^. contentState of
    Loading text -> loadingOverlay text
    Ready -> mrListWidget wenv mockMrs
    Error text -> errorOverlay text

mrListWidget wenv mrList =
  vscroll
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
    , appExitEvent AppQuit
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
