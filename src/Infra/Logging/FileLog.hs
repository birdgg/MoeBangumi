{- | File-based JSON Lines log reading and cleanup

This module provides log file operations:
- Reading and filtering logs by date and level
- Cleanup for old log files
- Utilities for log file paths

Note: Writing is handled by the Logger backend in Infra.Logging
-}
module Infra.Logging.FileLog (
  -- * Configuration
  FileLogConfig (..),
  defaultFileLogConfig,

  -- * Reading
  readLogEntries,
  queryLogs,

  -- * Cleanup
  cleanupOldLogs,

  -- * Utilities
  getLogFilePath,
  formatDay,
)
where

import Control.Exception (catch)
import Control.Monad (foldM)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Data.Time (Day, addDays, getCurrentTime, utctDay)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import Effectful
import System.Directory (createDirectoryIfMissing, doesFileExist, listDirectory, removeFile)
import System.FilePath (takeBaseName, (</>))

import Infra.Logging.Types

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Configuration for file logging
data FileLogConfig = FileLogConfig
  { logDir :: FilePath
  -- ^ Directory for log files
  , retentionDays :: Int
  -- ^ Number of days to retain logs (default: 7)
  }
  deriving stock (Show, Eq, Generic)

-- | Create default config from data folder
defaultFileLogConfig :: FilePath -> FileLogConfig
defaultFileLogConfig dataFolder =
  FileLogConfig
    { logDir = dataFolder </> "logs"
    , retentionDays = 7
    }

--------------------------------------------------------------------------------
-- Reading
--------------------------------------------------------------------------------

-- | Read all log entries for a specific day
readLogEntries ::
  (IOE :> es) =>
  FileLogConfig ->
  Day ->
  Eff es [LogEntry]
readLogEntries config day = liftIO $ do
  let filePath = getLogFilePath config.logDir day
  exists <- doesFileExist filePath

  if not exists
    then pure []
    else do
      content <- readFileBS filePath
      let textContent = decodeUtf8 content
      let jsonLines = T.lines textContent
      pure $ mapMaybe (Aeson.decode . BL.fromStrict . encodeUtf8) jsonLines

-- | Query logs with filtering and pagination
queryLogs ::
  (IOE :> es) =>
  FileLogConfig ->
  LogQuery ->
  Eff es LogsResponse
queryLogs config query = do
  allEntries <- readLogEntries config query.date

  -- Filter by level if specified
  let filtered = case query.levelFilter of
        Nothing -> allEntries
        Just lvl -> filter (\e -> e.level == lvl) allEntries

  -- Reverse to show newest first (logs are appended chronologically)
  let reversed = reverse filtered

  -- Apply pagination
  let total = length reversed
  let offset = (query.page - 1) * query.pageSize
  let paginated = take query.pageSize $ drop offset reversed

  pure $
    LogsResponse
      { entries = paginated
      , total = total
      , page = query.page
      , pageSize = query.pageSize
      }

--------------------------------------------------------------------------------
-- Cleanup
--------------------------------------------------------------------------------

{- | Delete log files older than retention period

Returns the number of files deleted
-}
cleanupOldLogs ::
  (IOE :> es) =>
  FileLogConfig ->
  Eff es Int
cleanupOldLogs config = liftIO $ do
  now <- getCurrentTime
  let cutoffDay = addDays (negate $ fromIntegral config.retentionDays) (utctDay now)

  -- Ensure directory exists before listing
  createDirectoryIfMissing True config.logDir

  files <- listDirectory config.logDir
  let logFiles = filter (T.isSuffixOf ".jsonl" . toText) files

  foldM (deleteIfOld cutoffDay) 0 logFiles
 where
  deleteIfOld :: Day -> Int -> FilePath -> IO Int
  deleteIfOld cutoff count file = do
    let day = parseLogFileName file
    case day of
      Nothing -> pure count
      Just d ->
        if d < cutoff
          then do
            catch
              (removeFile (config.logDir </> file) >> pure (count + 1))
              (\(_ :: SomeException) -> pure count)
          else pure count

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Get log file path for a specific day
getLogFilePath :: FilePath -> Day -> FilePath
getLogFilePath logDir day =
  logDir </> formatDay day <> ".jsonl"

-- | Format Day as YYYY-MM-DD
formatDay :: Day -> FilePath
formatDay = formatTime defaultTimeLocale "%Y-%m-%d"

-- | Parse log file name to Day (e.g., "2026-01-16.jsonl" -> Day)
parseLogFileName :: FilePath -> Maybe Day
parseLogFileName fileName =
  let baseName = takeBaseName fileName
   in parseTimeM True defaultTimeLocale "%Y-%m-%d" baseName
