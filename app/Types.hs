module Types where

import Data.List.NonEmpty
import Data.Text (Text)

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
  { _mrs :: Maybe (NonEmpty MergeRequest)
  , _contentState :: ContentLoadState
  , _selectedMr :: Maybe Iid
  }
  deriving (Eq, Show)

data AppEvent
  = AppInit
  | AppQuit
  deriving (Eq, Show)
