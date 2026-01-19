{- | RSS fetch job definition

This module provides the job entry point for periodic RSS fetching.
The job runs every hour to check for new torrents across all enabled
RSS subscriptions.

== Usage

@
import App.Jobs.RssFetch (rssFetchJob)
import App.Jobs.Scheduler (runJobsConcurrently)

-- In your server startup:
let jobs = [rssFetchJob logger env]
runJobsConcurrently jobs
@
-}
module App.Jobs.RssFetch.Job
  ( -- * Job Definition
    rssFetchJob

    -- * On-Demand Fetch
  , triggerRssFetch
  )
where

import Control.Concurrent (forkIO)
import Effectful
import Effectful.Concurrent (runConcurrent)
import Effectful.Reader.Static qualified as Reader
import Log (Logger)

import Infra.Environment.Config (MoeConfig (..), toLogBaseLevel)

import App.Jobs.RssFetch.Fetch (fetchAllEnabledRss, fetchSingleRss)
import App.Jobs.Types (Job (..), hours)
import Infra.Database.Effect (runDBPool)
import Infra.Downloader.QBittorrent (runDownloader)
import Infra.Database.Repository.Rss qualified as RssRepo
import Infra.Environment.Env (MoeEnv (..))
import Infra.External.Rss.Effect (runRss)
import Infra.Logging (runLog)
import Moe.Setting (Settings (..))

--------------------------------------------------------------------------------
-- Job Definition
--------------------------------------------------------------------------------

{- | RSS fetch job - runs every 1 hour

This job queries all enabled RSS subscriptions and fetches their feeds.
Errors are logged but don't stop processing of other feeds.

Each job manages its own effect stack internally, including:
- Database connection pool
- RSS fetching
- Downloader (qBittorrent)
- Logging
- Concurrent operations
- Reader for MoeEnv
-}
rssFetchJob ::
  Logger ->
  MoeEnv ->
  Job
rssFetchJob logger env =
  Job
    { name = "rss-fetch"
    , interval = hours 1
    , action = runRssFetchAction logger env
    }

--------------------------------------------------------------------------------
-- Internal Action Runner
--------------------------------------------------------------------------------

{- | Run the RSS fetch action with all necessary effects

This function sets up the complete effect stack for RSS fetching:
1. Reader for MoeEnv (configuration access)
2. Database pool
3. RSS fetching
4. Downloader (qBittorrent)
5. Logging
6. Concurrent operations
-}
runRssFetchAction :: Logger -> MoeEnv -> IO ()
runRssFetchAction logger env = do
  -- Get downloader settings from current settings
  settings :: Settings <- readTVarIO env.settingsVar
  let downloaderSettings = settings.downloader

  -- Run the effect stack
  fetchAllEnabledRss
    & Reader.runReader env
    & runDBPool env.pool
    & runRss env.httpManager
    & runDownloader env.httpManager downloaderSettings
    & runLog env.environment (toLogBaseLevel env.config.logLevel) logger
    & runConcurrent
    & runEff

--------------------------------------------------------------------------------
-- On-Demand Fetch
--------------------------------------------------------------------------------

{- | Trigger an immediate RSS fetch for a specific RSS entry

This function can be called from route handlers to immediately fetch
a newly added RSS subscription, rather than waiting for the next
scheduled job cycle.

The fetch runs asynchronously in a new thread.
-}
triggerRssFetch :: MoeEnv -> Int -> IO ()
triggerRssFetch env rssId = void . forkIO $ do
  -- Get downloader settings from current settings
  settings :: Settings <- readTVarIO env.settingsVar
  let downloaderSettings = settings.downloader

  -- Look up the RSS entry and fetch it
  mRss <- RssRepo.getOne rssId
    & runDBPool env.pool
    & runEff

  case mRss of
    Nothing -> pass
    Just rss ->
      fetchSingleRss rss
        & Reader.runReader env
        & runDBPool env.pool
        & runRss env.httpManager
        & runDownloader env.httpManager downloaderSettings
        & runLog env.environment (toLogBaseLevel env.config.logLevel) defaultLogger
        & runEff
 where
  defaultLogger = mempty  -- Silent logger for on-demand fetch
