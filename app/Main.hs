{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens
import Data.Text (Text)
import Data.List.NonEmpty
import Monomer hiding (label)
import UiKit

import qualified Monomer.Lens as L

data MergeRequest = MergeRequest {
  _iid :: Int,
  _title :: Text
} deriving (Eq, Show)

data AppModel = AppModel {
  _mrs :: Maybe (NonEmpty MergeRequest)
} deriving (Eq, Show)

data AppEvent
  = AppInit
  deriving (Eq, Show)

makeLenses 'AppModel

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree where
  widgetTree = vstack [
      label "Hello",
      label "World"
    ] `styleBasic` [padding 10]

handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> []

main_mrmr :: IO ()
main_mrmr = do
  startApp model handleEvent buildUI config
  where
    config = [
      appWindowTitle "Mrmr",
      appWindowIcon "./assets/images/icon.png",
      appTheme lightTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      appInitEvent AppInit
      ]
    model = AppModel {
      _mrs = Nothing
    }

main :: IO ()
main = main_mrmr
-- main = Tutorial02_Styling.main02
-- main = Todo.todoMain
