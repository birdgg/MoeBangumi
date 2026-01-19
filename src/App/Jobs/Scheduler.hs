{- | Minimal job scheduler using IO-based jobs

Runs jobs at fixed intervals. Each job manages its own effect stack.

== Usage

@
import App.Jobs.Scheduler
import App.Jobs.Types

myJobs :: [Job]
myJobs =
  [ someJob logger env
  , anotherJob logger env
  ]

-- Run all jobs in background threads
runJobsConcurrently myJobs
@
-}
module App.Jobs.Scheduler (
  -- * Running Jobs
  runJobsConcurrently,
  runJobLoop,
)
where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (catch)
import Data.Time (NominalDiffTime)

import App.Jobs.Types

--------------------------------------------------------------------------------
-- Running Jobs
--------------------------------------------------------------------------------

{- | Run all jobs concurrently in background threads

Forks a thread for each job. Jobs run independently.
Returns immediately after forking all jobs.
-}
runJobsConcurrently :: [Job] -> IO ()
runJobsConcurrently jobs =
  forM_ jobs $ \job ->
    void $ forkIO $ void $ runJobLoop job

--------------------------------------------------------------------------------
-- Single Job Loop
--------------------------------------------------------------------------------

{- | Run a single job in an infinite loop

Executes the job action, waits for the interval, repeats.
Errors are caught to prevent the loop from dying.
-}
runJobLoop :: Job -> IO Void
runJobLoop job = do
  putStrLn $ "[Job] Started: " <> toString job.name <> " (interval: " <> show (nominalToSeconds job.interval) <> "s)"
  infinitely $ do
    runJobSafe job
    sleepFor job.interval

{- | Run job action with basic error handling

If the action throws, log and continue.
-}
runJobSafe :: Job -> IO ()
runJobSafe job = do
  job.action `catch` \(e :: SomeException) ->
    putStrLn $ "[Job] " <> toString job.name <> " failed: " <> show e

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Sleep for a duration
sleepFor :: NominalDiffTime -> IO ()
sleepFor duration = threadDelay (nominalToMicros duration)

-- | Convert NominalDiffTime to microseconds
nominalToMicros :: NominalDiffTime -> Int
nominalToMicros ndt = round (ndt * 1_000_000)

-- | Convert NominalDiffTime to seconds (for display)
nominalToSeconds :: NominalDiffTime -> Int
nominalToSeconds = round
