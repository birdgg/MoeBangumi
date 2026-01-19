{- | Log cleanup job for periodic old log file removal

This module provides scheduled log file cleanup:
- Runs daily to remove log files older than 7 days
- Logs cleanup statistics for monitoring

== Usage

@
import App.Jobs.LogCleanup (logCleanupJob)
import App.Jobs.Scheduler (runJobsConcurrently)

-- In your server startup:
let jobs = [logCleanupJob logger env.dataFolder]
runJobsConcurrently jobs
@
-}
module App.Jobs.LogCleanup
  ( -- * Job Definition
    logCleanupJob
  )
where

import Data.Aeson (object, (.=))
import Effectful
import Effectful.Log (Log)
import Effectful.Log qualified as Log
import Log (Logger)

import App.Jobs.Types (Job (..), days)
import Infra.Environment.Config (DeploymentEnv)
import Infra.Logging (FileLogConfig (..), cleanupOldLogs, defaultFileLogConfig, runLog)
import Log.Data qualified as LogBase

--------------------------------------------------------------------------------
-- Job Definition
--------------------------------------------------------------------------------

{- | Log cleanup job - runs every day

Deletes log files older than the configured retention period (default: 7 days).
Errors are logged but don't stop the job scheduler.

Each job manages its own effect stack internally.
-}
logCleanupJob ::
  Logger ->
  DeploymentEnv ->
  LogBase.LogLevel ->
  FilePath ->
  -- ^ Data folder path
  Job
logCleanupJob logger env logLevel dataFolder =
  Job
    { name = "log-cleanup"
    , interval = days 1
    , action = runLogCleanupAction logger env logLevel dataFolder
    }

--------------------------------------------------------------------------------
-- Internal Action Runner
--------------------------------------------------------------------------------

{- | Run the log cleanup action with all necessary effects

This function sets up the effect stack and runs the cleanup logic.
-}
runLogCleanupAction :: Logger -> DeploymentEnv -> LogBase.LogLevel -> FilePath -> IO ()
runLogCleanupAction logger env logLevel dataFolder =
  performLogCleanup (defaultFileLogConfig dataFolder)
    & runLog env logLevel logger
    & runEff

--------------------------------------------------------------------------------
-- Core Logic
--------------------------------------------------------------------------------

{- | Perform log cleanup and log the result

Deletes old log files based on the configured retention period.
Always logs the outcome (even if no files were deleted).
-}
performLogCleanup ::
  (IOE :> es, Log :> es) =>
  FileLogConfig ->
  Eff es ()
performLogCleanup config = do
  Log.logInfo "Starting log cleanup" $
    object
      [ "log_dir" .= config.logDir
      , "retention_days" .= config.retentionDays
      ]

  deletedCount <- cleanupOldLogs config

  Log.logInfo "Log cleanup completed" $
    object
      [ "deleted_files" .= deletedCount
      , "retention_days" .= config.retentionDays
      ]
