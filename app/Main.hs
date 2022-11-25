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
usePlayground = True

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree
 where
  bindings = [("Esc", AppQuit)]
  root = if usePlayground then playgroundWidget wenv model else rootWidget wenv model `nodeFocusable` True
  widgetTree = keystroke bindings $ root

handleEvent
  :: Sess.Session
  -> MrMrWenv
  -> MrMrNode
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent sess wenv node model evt = case evt of
  AppInit ->
    if usePlayground
      then []
      else case testMrIid of
        Just iid -> [Event (MrShowDetails iid)]
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
    , Task $ fetchMrDetails sess (Iid iid)
    ]
  MrDetailsFetched iid details ->
    [ Model $
        model
          & selectedMr ?~ iid
          & selectedMrDiffs .~ (details ^. diffs)
          & contentState .~ Ready
    ]
  ShowMrList ->
    [ Model $
        model
          & selectedMr .~ Nothing
    ]

playgroundWidget
  :: MrMrWenv
  -> AppModel
  -> MrMrNode
playgroundWidget wenv _ = commentThread wenv mockComments
 where
  mockComments =
    [ Comment
        { _cmtOldFile = [relfile|file/a.txt|]
        , _cmtNewFile = [relfile|file/b.txt|]
        , _cmtOldLine = Nothing
        , _cmtNewLine = Nothing
        , _cmtText = "ничего от такой смены не поломается?"
        , _cmtAuthorName = "Valery Kolesnikov"
        }
    , Comment
        { _cmtOldFile = [relfile|file/a.txt|]
        , _cmtNewFile = [relfile|file/b.txt|]
        , _cmtOldLine = Nothing
        , _cmtNewLine = Nothing
        , _cmtText = "нет, там наоборот баг всплыл при открытии диплинка, всегда запускались букмарки"
        , _cmtAuthorName = "Roman Chetverikov"
        }
    , Comment
        { _cmtOldFile = [relfile|file/a.txt|]
        , _cmtNewFile = [relfile|file/b.txt|]
        , _cmtOldLine = Nothing
        , _cmtNewLine = Nothing
        , _cmtText = "так по идее ведь мы хотим как раз сначала обработать все специфические, а потом уже общие uri.\n\nМне кажется, это может всё же что-то ещё поломать.\n\nНадо чётко понять какое поведение нам надо и какой приоритет должен быть."
        , _cmtAuthorName = "Dmitry Suzdalev"
        }
    , Comment
        { _cmtOldFile = [relfile|file/a.txt|]
        , _cmtNewFile = [relfile|file/b.txt|]
        , _cmtOldLine = Nothing
        , _cmtNewLine = Nothing
        , _cmtText = "extractUriDeepLink() в любом случае пройдет мимо, там указаны конкретные значения диплинков. А поменял я потому, что в некоторых случаях (и в этом) ошибочно отрабатывает return if (!files.isNullOrEmpty()) DeepLink.ImportFavorites.Bookmarks(files) else null"
        , _cmtAuthorName = "Roman Chetverikov"
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
