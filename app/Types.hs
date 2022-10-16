{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Lens
import Data.Aeson
import Data.Foldable (toList)
import Data.Text (Text)
import Monomer

data MrListResponse = MrListResponse [MergeRequest]

data MergeRequest = MergeRequest
  { _iid :: Iid
  , _title :: Text
  }
  deriving (Eq, Show)

instance FromJSON MergeRequest where
  parseJSON = withObject "MergeRequest" $ \mr ->
    MergeRequest
      <$> (Iid <$> mr .: "iid")
      <*> mr
        .: "title"

instance FromJSON MrListResponse where
  parseJSON = withArray "MrList" $ \arr -> do
    mrs <- mapM parseJSON arr
    pure $ MrListResponse (toList mrs)

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
  | ShowMrList
  deriving (Eq, Show)

type MrMrWenv = WidgetEnv AppModel AppEvent
type MrMrNode = WidgetNode AppModel AppEvent

makeLenses 'MergeRequest
makeLenses 'AppModel
