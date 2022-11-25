{-# LANGUAGE NumericUnderscores #-}

module Network where

import Control.Applicative (optional)
import Control.Lens
import Control.Monad (mzero, unless)
import Data.Aeson
import Data.Foldable (toList)
import Data.Maybe (catMaybes)
import Data.Text
import Network.Wreq as W
import Network.Wreq.Session as Sess
import Parse (decodeFileDiff)
import Path
import TextShow
import Types

projectId = "0" -- 39695842" -- no-commit
mrIid = "0" -- no-commit
privateToken = "" -- no-commit

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

instance FromJSON MrChangesResponse where
  parseJSON = withObject "MrChanges" $ \obj -> do
    changes <- obj .: "changes"
    diffFiles <- mapM parseJSON changes
    pure $ MrChangesResponse diffFiles

instance FromJSON MrCommentsResponse where
  parseJSON = withArray "MrComments" $ \arr -> do
    comments <- mapM (\v -> optional (parseJSON v)) arr
    pure $ MrCommentsResponse (catMaybes (toList comments))

instance FromJSON Comment where
  parseJSON = withObject "Comment" $ \c -> do
    isSystem <- c .: "system"
    if isSystem
      then mzero
      else do
        position <- c .: "position"
        oldLine <- position .:? "old_line"
        newLine <- position .:? "new_line"
        oldPathStr <- position .: "old_path"
        oldPath <- maybe (fail "failed to parse old_path") pure (parseRelFile oldPathStr)
        newPathStr <- position .: "new_path"
        newPath <- maybe (fail "failed to parse new_path") pure (parseRelFile newPathStr)
        text <- c .: "body"
        author <- c .: "author"
        name <- author .: "name"
        pure $
          Comment
            { _cmtOldFile = oldPath
            , _cmtNewFile = newPath
            , _cmtOldLine = oldLine
            , _cmtNewLine = newLine
            , _cmtText = text
            , _cmtAuthorName = name
            }

instance FromJSON DiffFile where
  parseJSON = withObject "change" $ \change ->
    do
      oldPathStr <- change .: "old_path"
      oldPath <- maybe (fail "failed to parse old_path") pure (parseRelFile oldPathStr)
      newPathStr <- change .: "new_path"
      newPath <- maybe (fail "failed to parse new_path") pure (parseRelFile newPathStr)
      hunksRaw <- change .: "diff"
      hunks <- parseHunks hunksRaw
      pure $ DiffFile oldPath newPath hunks

parseHunks :: MonadFail m => Text -> m [DiffHunk]
parseHunks text = case decodeFileDiff text of
  Left error -> fail (unpack error)
  Right hunks -> pure hunks

fetchMrList
  :: Sess.Session
  -> IO AppEvent
fetchMrList sess = do
  let url = "https://gitlab.com/api/v4/merge_requests?private_token=" <> privateToken
  resp <- Sess.get sess (unpack url) >>= W.asJSON
  pure $ MrListResult (resp ^. responseBody)

fetchMrDetails
  :: Sess.Session
  -> Iid
  -> IO AppEvent
fetchMrDetails sess iid = do
  MrChangesResponse diffs <- fetchMrChanges sess iid
  -- MrCommentsResponse comments <- fetchMrComments sess iid
  pure $ MrDetailsFetched iid (MrDetails diffs [])

fetchMrChanges
  :: Sess.Session
  -> Iid
  -> IO MrChangesResponse
fetchMrChanges sess (Iid iid) = do
  let url =
        "https://gitlab.com/api/v4/projects/"
          <> projectId
          <> "/merge_requests/"
          <> showt iid
          <> "/changes?private_token="
          <> privateToken
  resp <- (Sess.get sess (unpack url) >>= W.asJSON) :: IO (Response MrChangesResponse)
  pure (resp ^. responseBody)

fetchMrComments
  :: Sess.Session
  -> Iid
  -> IO MrCommentsResponse
fetchMrComments sess (Iid iid) = do
  let url =
        "https://gitlab.com/api/v4/projects/"
          <> projectId
          <> "/merge_requests/"
          <> showt iid
          -- TODO add proper pagination instead of maxing out at 100 comments
          <> "/notes?per_page=100&private_token="
          <> privateToken
  resp <- (Sess.get sess (unpack url) >>= W.asJSON) :: IO (Response MrCommentsResponse)
  pure (resp ^. responseBody)
