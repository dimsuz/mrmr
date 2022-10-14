module Types where

import Data.Text (Text)
import Data.List.NonEmpty

data MergeRequest = MergeRequest {
  _iid :: Iid,
  _title :: Text
} deriving (Eq, Show)

newtype Iid = Iid Int
  deriving (Eq, Show)

data ContentLoadState = Loading | Ready | Error Text
  deriving (Eq, Show)

data AppModel = AppModel {
  _mrs :: Maybe (NonEmpty MergeRequest),
  _contentState :: ContentLoadState,
  _selectedMr :: Maybe Iid
} deriving (Eq, Show)

data AppEvent
  = AppInit | AppQuit
  deriving (Eq, Show)
