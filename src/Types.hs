{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Lens
import Data.Aeson
import Data.Maybe
import Data.Text (Text)
import Monomer hiding (Path)
import Path

newtype MrListResponse = MrListResponse [MergeRequest]

data MergeRequest = MergeRequest
  { _iid :: Iid
  , _title :: Text
  }
  deriving (Eq, Show)

newtype MrChangesResponse = MrChangesResponse [DiffFile]

newtype MrCommentsResponse = MrCommentsResponse [Comment]

data MrDetails = MrDetails
  { _diffs :: [DiffFile]
  , _comments :: [Comment]
  }
  deriving (Eq, Show)

newtype Iid = Iid Int
  deriving (Eq, Show)

data ContentLoadState = Loading Text | Ready | Error Text
  deriving (Eq, Show)

data HunkHeader = HunkHeader
  { _oldStart :: Int
  , _oldCount :: Int
  , _newStart :: Int
  , _newCount :: Int
  , _text :: Maybe Text
  }
  deriving (Eq, Show)

data DiffHunk = DiffHunk
  { _dhHeader :: HunkHeader
  , _dhLines :: [Text]
  }
  deriving (Eq, Show)

data DiffFile = DiffFile
  { _oldFile :: Path Rel File
  , _newFile :: Path Rel File
  , _hunks :: [DiffHunk]
  }
  deriving (Eq, Show)

data Comment = Comment
  { _cmtOldFile :: Path Rel File
  , _cmtNewFile :: Path Rel File
  , _cmtOldLine :: Maybe Int
  , _cmtNewLine :: Maybe Int
  , _cmtText :: Text
  , _cmtAuthorName :: Text
  }
  deriving (Eq, Show)

data AppModel = AppModel
  { _mrs :: Maybe [MergeRequest]
  , _contentState :: ContentLoadState
  , _selectedMr :: Maybe Iid
  , _selectedMrDiffs :: [DiffFile]
  }
  deriving (Eq, Show)

data AppEvent
  = AppInit
  | AppQuit
  | FetchMrList
  | MrListResult [MergeRequest]
  | MrDetailsFetched Iid MrDetails
  | MrListError Text
  | MrShowDetails Iid
  | ShowMrList
  | EditComment
  deriving (Eq, Show)

type MrMrWenv = WidgetEnv AppModel AppEvent
type MrMrNode = WidgetNode AppModel AppEvent

makeLenses 'MergeRequest
makeLenses 'DiffFile
makeLenses 'AppModel
makeLenses 'DiffHunk
makeLenses 'HunkHeader
makeLenses 'MrDetails
makeLenses 'Comment
