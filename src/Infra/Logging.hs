module Infra.Logging (
  -- * Standard Logging
  makeLogger,
  runLog,
  timeAction,

  -- * File Logging (re-exports)
  module Infra.Logging.Types,
  module Infra.Logging.FileLog,
)
where

import Data.Aeson qualified as Aeson
import Data.Text.Display (display)
import Data.Time (getCurrentTime, utctDay)
import Data.Time.Clock as Time (NominalDiffTime, diffUTCTime)
import Effectful
import Effectful.Log (Log)
import Effectful.Log qualified as Log
import Effectful.Time (Time)
import Effectful.Time qualified as Time
import Log (Logger, mkLogger)
import Log.Backend.StandardOutput qualified as StdOut
import Log.Data (LogMessage (..), lmTime)
import Log.Data qualified as LogBase
import Log.Internal.Logger (withLogger)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import UnliftIO.STM (TChan)
import UnliftIO.STM qualified as STM

import Infra.Environment.Config (DeploymentEnv)
import Infra.Logging.FileLog
import Infra.Logging.Types

--------------------------------------------------------------------------------
-- Logger Creation
--------------------------------------------------------------------------------

{- | Create a combined logger that writes to both stdout and daily log files

The file logger writes JSON Lines format to {dataFolder}/logs/{YYYY-MM-DD}.jsonl
with automatic daily rotation based on log entry timestamps.

When a broadcast channel is provided, also broadcasts log entries for SSE streaming.
-}
makeLogger ::
  (IOE :> es) =>
  -- | Data folder path for log files
  FilePath ->
  -- | Optional broadcast channel for SSE streaming
  Maybe (TChan LogEntry) ->
  (Logger -> Eff es a) ->
  Eff es a
makeLogger dataFolder mBroadcastChan action = do
  let logDir = dataFolder </> "logs"
  -- Ensure log directory exists
  liftIO $ createDirectoryIfMissing True logDir

  -- Create stdout logger
  StdOut.withJsonStdOutLogger $ \stdoutLogger ->
    -- Create file logger (with optional broadcast)
    withJSONFileBackend logDir mBroadcastChan $ \fileLogger ->
      -- Combine both loggers (Logger is a Monoid)
      action (stdoutLogger <> fileLogger)

{- | Create a file-based JSON logger backend

Writes each log message as a JSON line to a daily log file.
File path: {logDir}/{YYYY-MM-DD}.jsonl

When a broadcast channel is provided, also broadcasts log entries for SSE streaming
(only for today's logs).
-}
withJSONFileBackend ::
  (IOE :> es) =>
  -- | Log directory
  FilePath ->
  -- | Optional broadcast channel
  Maybe (TChan LogEntry) ->
  (Logger -> Eff es a) ->
  Eff es a
withJSONFileBackend logDir mBroadcastChan action = withRunInIO $ \unlift -> do
  logger <- mkLogger "file-json" $ \msg -> do
    -- Extract timestamp from message to determine file
    let day = utctDay $ lmTime msg
    let filePath = getLogFilePath logDir day

    -- Write to file
    appendFileBS filePath (toStrict $ Aeson.encode msg <> "\n")

    -- Broadcast to channel if provided (only for today's logs)
    whenJust mBroadcastChan $ \chan -> do
      today <- utctDay <$> getCurrentTime
      when (day == today) $ do
        let entry = logMessageToEntry msg
        STM.atomically $ STM.writeTChan chan entry

  withLogger logger $ \logger' ->
    unlift $ action logger'

-- | Convert log-base LogMessage to our LogEntry type
logMessageToEntry :: LogMessage -> LogEntry
logMessageToEntry msg =
  LogEntry
    { timestamp = lmTime msg
    , level = convertLogLevel (lmLevel msg)
    , message = lmMessage msg
    , component = lmComponent msg
    , metadata = case lmData msg of
        Aeson.Null -> Nothing
        v -> Just v
    }

-- | Convert log-base LogLevel to our LogLevel
convertLogLevel :: LogBase.LogLevel -> LogLevel
convertLogLevel = \case
  LogBase.LogAttention -> Error
  LogBase.LogInfo -> Info
  LogBase.LogTrace -> Info

--------------------------------------------------------------------------------
-- Log Runner
--------------------------------------------------------------------------------

-- | Wrapper around 'Log.runLogT' with necessary metadata
runLog ::
  forall (es :: [Effect]) (a :: Type).
  (IOE :> es) =>
  DeploymentEnv ->
  LogBase.LogLevel ->
  Logger ->
  Eff (Log : es) a ->
  Eff es a
runLog env level logger = Log.runLog ("moe-" <> suffix) logger level
 where
  suffix = display env

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

timeAction ::
  forall (es :: [Effect]) (a :: Type).
  (Time :> es) =>
  Eff es a ->
  Eff es (a, NominalDiffTime)
timeAction action = do
  start <- Time.currentTime
  result <- action
  end <- Time.currentTime
  pure (result, Time.diffUTCTime end start)
