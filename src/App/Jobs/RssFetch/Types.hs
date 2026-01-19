{- | RSS fetch job configuration

Configuration constants and shared types for the RSS fetch job.
-}
module App.Jobs.RssFetch.Types
  ( -- * Configuration
    maxConcurrentFetches
  )
where

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

{- | Maximum number of concurrent RSS fetches

Limits parallel HTTP requests to avoid overwhelming servers
and to be a good citizen on the network.
-}
maxConcurrentFetches :: Int
maxConcurrentFetches = 5
