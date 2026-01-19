{- | File rename job definition

This module provides the job entry point for periodic file renaming.
The job runs every 30 minutes to process completed torrents tagged
with "rename".

== Usage

@
import App.Jobs.Rename (renameJob)
import App.Jobs.Scheduler (runJobsConcurrently)

-- In your server startup:
let jobs = [renameJob logger env]
runJobsConcurrently jobs
@
-}
module App.Jobs.Rename.Job
  ( -- * Job Definition
    renameJob
  )
where

import Effectful
import Effectful.Concurrent (runConcurrent)
import Effectful.Reader.Static qualified as Reader
import Log (Logger)

import Infra.Environment.Config (MoeConfig (..), toLogBaseLevel)

import App.Jobs.Rename.Rename (renameAllTaggedTorrents)
import App.Jobs.Types (Job (..), minutes)
import Infra.Database.Effect (runDBPool)
import Infra.Downloader.QBittorrent (runDownloader)
import Infra.Environment.Env (MoeEnv (..))
import Infra.Logging (runLog)
import Infra.Notification.Telegram (runNotification)
import Moe.Setting (NotificationSettings (..), Settings (..))

--------------------------------------------------------------------------------
-- Job Definition
--------------------------------------------------------------------------------

{- | File rename job - runs every 30 minutes

Processes torrents tagged with "rename":

- Only completed downloads (progress >= 1.0)
- Single video file torrents: Renamed to torrent name + extension
- Multi-video torrents (collections): Scraped via TMDB, batch renamed to S{season}E{episode} format
- Removes "rename" tag after successful rename

Each job manages its own effect stack internally, including:

- Downloader (qBittorrent)
- Database (for Bangumi creation from collections)
- Reader (environment access for TMDB)
- Logging
- Concurrent operations
-}
renameJob ::
  Logger ->
  MoeEnv ->
  Job
renameJob logger env =
  Job
    { name = "file-rename"
    , interval = minutes 30
    , action = runRenameAction logger env
    }

--------------------------------------------------------------------------------
-- Internal Action Runner
--------------------------------------------------------------------------------

{- | Run the rename action with all necessary effects

This function sets up the complete effect stack for file renaming:
1. Reader (MoeEnv for TMDB access)
2. Database (for Bangumi creation)
3. Downloader (qBittorrent)
4. Logging
5. Concurrent operations
-}
runRenameAction :: Logger -> MoeEnv -> IO ()
runRenameAction logger env = do
  -- Get downloader settings from current settings
  settings :: Settings <- readTVarIO env.settingsVar
  let downloaderSettings = settings.downloader
      telegramConfig = settings.notification.telegram

  -- Run the effect stack
  renameAllTaggedTorrents
    & Reader.runReader env
    & runDBPool env.pool
    & runDownloader env.httpManager downloaderSettings
    & runNotification env.httpManager telegramConfig
    & runLog env.environment (toLogBaseLevel env.config.logLevel) logger
    & runConcurrent
    & runEff
