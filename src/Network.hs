{-# LANGUAGE NumericUnderscores #-}

module Network where

import Control.Lens
import Control.Monad (mzero, unless)
import Data.Aeson
import Data.Foldable (toList)
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
    comments <- mapM parseJSON arr
    pure $ MrCommentsResponse (toList comments)

instance FromJSON Comment where
  parseJSON = withObject "Comment" $ \c -> do
    isSystem <- c .: "system"
    if isSystem
      then mzero
      else do
        pure $ Comment Nothing Nothing 0 0 "foo" "bar"

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

mockMrs =
  [ MergeRequest{_iid = Iid 33, _title = "Merge Request #1"}
  , MergeRequest{_iid = Iid 34, _title = "Merge Request #2"}
  , MergeRequest{_iid = Iid 35, _title = "Merge Request #3"}
  , MergeRequest{_iid = Iid 36, _title = "Merge Request #4"}
  , MergeRequest{_iid = Iid 37, _title = "Merge Request #5"}
  ]

fetchMrList
  :: Sess.Session
  -> IO AppEvent
fetchMrList sess = do
  let url = "https://gitlab.com/api/v4/merge_requests?private_token=" <> privateToken
  resp <- Sess.get sess (unpack url) >>= W.asJSON
  pure $ MrListResult (resp ^. responseBody)

fetchMrChanges
  :: Sess.Session
  -> Iid
  -> IO AppEvent
fetchMrChanges sess (Iid iid) = do
  let url =
        "https://gitlab.com/api/v4/projects/"
          <> projectId
          <> "/merge_requests/"
          <> showt iid
          <> "/changes?private_token="
          <> privateToken
  resp <- (Sess.get sess (unpack url) >>= W.asJSON) :: IO (Response MrChangesResponse)
  let MrChangesResponse diff = (resp ^. responseBody)
  pure $ MrDetailsFetched (Iid iid) diff

fetchMrComments
  :: Sess.Session
  -> Iid
  -> IO AppEvent
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
  let MrCommentsResponse comments = (resp ^. responseBody)
  pure $ MrCommentsFetched (Iid iid) comments
