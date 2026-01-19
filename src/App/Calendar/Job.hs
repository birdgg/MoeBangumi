{- | Calendar sync job

Periodic job to sync seasonal calendar data from Mikan.
-}
module App.Calendar.Job (
  -- * Job Definition
  calendarSyncJob,
) where

import Effectful
import Effectful.Concurrent (runConcurrent)
import Effectful.Reader.Static qualified as Reader
import Log (Logger)

import Infra.Environment.Config (MoeConfig (..), toLogBaseLevel)

import App.Calendar.Sync (getCurrentSeason, syncCalendar)
import App.Jobs.Types (Job (..), hours)
import Infra.Database.Effect (runDBPool)
import Infra.Environment.Env (MoeEnv (..))
import Infra.Logging (runLog)

--------------------------------------------------------------------------------
-- Job Definition
--------------------------------------------------------------------------------

{- | Calendar sync job - runs every 6 hours

This job fetches the current season's calendar from Mikan
and updates the local database.
-}
calendarSyncJob ::
  Logger ->
  MoeEnv ->
  Job
calendarSyncJob logger env =
  Job
    { name = "calendar-sync"
    , interval = hours 6
    , action = runCalendarSyncAction logger env
    }

--------------------------------------------------------------------------------
-- Internal Action Runner
--------------------------------------------------------------------------------

-- | Run the calendar sync action with all necessary effects
runCalendarSyncAction :: Logger -> MoeEnv -> IO ()
runCalendarSyncAction logger env = do
  -- Get current year and season
  (year, season) <- getCurrentSeason

  -- Run the effect stack
  void $
    syncCalendar year season
      & Reader.runReader env
      & runDBPool env.pool
      & runLog env.environment (toLogBaseLevel env.config.logLevel) logger
      & runConcurrent
      & runEff
