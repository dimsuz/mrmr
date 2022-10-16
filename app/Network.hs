{-# LANGUAGE NumericUnderscores #-}

module Network where

import Control.Concurrent
import Network.Wreq as W
import Network.Wreq.Session as Sess
import Types

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
fetchMrList _ = do
  threadDelay (1_000_000 `div` 2)
  pure $ MrListResult mockMrs
