{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Lens
import Data.List.NonEmpty
import Data.Text (Text)
import Monomer

data MergeRequest = MergeRequest
  { _iid :: Iid
  , _title :: Text
  }
  deriving (Eq, Show)

newtype Iid = Iid Int
  deriving (Eq, Show)

data ContentLoadState = Loading Text | Ready | Error Text
  deriving (Eq, Show)

data AppModel = AppModel
  { _mrs :: Maybe [MergeRequest]
  , _contentState :: ContentLoadState
  , _selectedMr :: Maybe Iid
  }
  deriving (Eq, Show)

data AppEvent
  = AppInit
  | AppQuit
  | FetchMrList
  | MrListResult [MergeRequest]
  | MrListError Text
  | MrShowDetails Iid
  deriving (Eq, Show)

type MrMrWenv = WidgetEnv AppModel AppEvent
type MrMrNode = WidgetNode AppModel AppEvent

makeLenses 'MergeRequest
makeLenses 'AppModel
