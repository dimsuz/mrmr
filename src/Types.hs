{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Lens
import Data.Aeson
import Data.Foldable (toList)
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

newtype MrChangesResponse = MrChangesResponse [DiffFile]

instance FromJSON DiffFile where
  parseJSON = withObject "change" $ \change ->
    do
      oldPathStr <- change .: "old_path"
      oldPath <- maybe (fail "failed to parse old_path") pure (parseRelFile oldPathStr)
      newPathStr <- change .: "new_path"
      newPath <- maybe (fail "failed to parse new_path") pure (parseRelFile newPathStr)
      hunks <- parseHunks <$> change .: "diff"
      pure $ DiffFile oldPath newPath hunks

parseHunks :: Text -> [DiffHunk]
parseHunks = undefined

newtype Iid = Iid Int
  deriving (Eq, Show)

data ContentLoadState = Loading Text | Ready | Error Text
  deriving (Eq, Show)

data HunkHeader = HunkHeader
  { _oldStart :: Int
  , _oldCount :: Int
  , _newStart :: Int
  , _newCount :: Int
  , _text :: Text
  }
  deriving (Show)

data DiffHunk = DiffHunk
  { _dhHeader :: HunkHeader
  , _dhLines :: [Text]
  }

data DiffFile = DiffFile
  { _oldFile :: Path Rel File
  , _newFile :: Path Rel File
  , _hunks :: [DiffHunk]
  }

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
makeLenses 'DiffFile
makeLenses 'AppModel
makeLenses 'DiffHunk
makeLenses 'HunkHeader
