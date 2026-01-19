{- | Logs API handler implementation

Implements the REST API handlers for log queries and SSE streaming.
-}
module Web.API.Server.Logs
  ( logsServer
  )
where

import Data.Time (Day)
import Effectful qualified
import Effectful.Reader.Static qualified as Reader
import Servant
import Servant.Types.SourceT (SourceT (..), StepT (..))
import UnliftIO.STM qualified as STM

import Infra.Environment.Env (MoeEnv (..))
import Infra.Logging (LogEntry, LogLevel, LogQuery (..), LogsResponse, defaultFileLogConfig, queryLogs)
import Moe.Monad (MoeM)
import RequireCallStack
import Web.API.Routes.Logs qualified as Logs
import Web.Types (MoeEff)

-- | Logs API server implementation
logsServer :: RequireCallStack => ServerT Logs.API MoeEff
logsServer =
  Logs.Routes'
    { getLogs = getLogsHandler
    , streamLogs = streamLogsHandler
    }

-- | Handler for GET /api/logs
--
-- Query parameters:
-- - date: Required, the date to query (YYYY-MM-DD)
-- - level: Optional, filter by log level (info, warning, error)
-- - page: Optional, page number (default: 1)
-- - pageSize: Optional, items per page (default: 100, max: 1000)
getLogsHandler ::
  (Reader.Reader MoeEnv Effectful.:> es, Effectful.IOE Effectful.:> es) =>
  Day ->
  Maybe LogLevel ->
  Maybe Int ->
  Maybe Int ->
  MoeM es LogsResponse
getLogsHandler date mLevel mPage mPageSize = do
  env <- Reader.ask
  let config = defaultFileLogConfig env.dataFolder
  -- Ensure page >= 1 and pageSize is within bounds
  let page = max 1 (fromMaybe 1 mPage)
  let pageSize = max 1 $ min 1000 (fromMaybe 100 mPageSize)

  let query =
        LogQuery
          { date = date
          , levelFilter = mLevel
          , page = page
          , pageSize = pageSize
          }

  queryLogs config query

-- | Handler for GET /api/logs/stream (SSE)
--
-- Streams real-time logs from the broadcast channel.
-- Only emits logs that occur after client connection.
-- Client should filter by level if needed.
streamLogsHandler ::
  (Reader.Reader MoeEnv Effectful.:> es, Effectful.IOE Effectful.:> es) =>
  MoeM es (SourceT IO LogEntry)
streamLogsHandler = do
  env <- Reader.ask

  -- Clone the broadcast channel for this SSE connection
  -- Each client gets its own read-only view
  clientChan <- liftIO $ STM.atomically $ STM.dupTChan env.logBroadcast

  -- Create infinite stream from TChan
  pure $ tchanToSourceT clientChan

-- | Convert a TChan to an infinite SourceT
--
-- Reads from the channel and yields each log entry.
-- Blocks when the channel is empty, resuming when new entries arrive.
tchanToSourceT :: STM.TChan LogEntry -> SourceT IO LogEntry
tchanToSourceT chan = SourceT $ \k -> k go
 where
  go :: StepT IO LogEntry
  go = Effect $ do
    entry <- STM.atomically $ STM.readTChan chan
    pure $ Yield entry go
