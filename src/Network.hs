{-# LANGUAGE NumericUnderscores #-}

module Network where

import Control.Lens
import Data.Aeson
import Network.Wreq as W
import Network.Wreq.Session as Sess
import Types

projectId = "0" -- 39695842" -- no-commit
mrIid = "0" -- no-commit
privateToken = "" -- no-commit

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
  resp <- Sess.get sess url >>= W.asJSON
  pure $ MrListResult (resp ^. responseBody)
