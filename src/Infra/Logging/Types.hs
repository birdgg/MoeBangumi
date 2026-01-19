{- | Log types for file-based logging system

This module defines the core types for the JSON Lines log format:
- LogLevel: info, warning, error
- LogEntry: structured log entry with timestamp and metadata
- LogQuery: query parameters for log API
- LogsResponse: paginated response
-}
module Infra.Logging.Types
  ( -- * Log Level
    LogLevel (..)
  , logLevelToText
  , parseLogLevel

    -- * Log Entry
  , LogEntry (..)

    -- * API Types
  , LogQuery (..)
  , LogsResponse (..)
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), Value, withText, (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.OpenApi (ToParamSchema (..), ToSchema (..))
import Data.OpenApi qualified as OpenApi
import Data.Text qualified as T
import Data.Time (Day, UTCTime)
import Optics.Core ((?~))
import Servant (FromHttpApiData (..))

--------------------------------------------------------------------------------
-- Log Level
--------------------------------------------------------------------------------

-- | Log severity level
data LogLevel
  = Info
  | Warning
  | Error
  deriving stock (Show, Eq, Ord, Generic, Bounded, Enum)
  deriving anyclass (ToSchema)

instance ToJSON LogLevel where
  toJSON = toJSON . logLevelToText

instance FromJSON LogLevel where
  parseJSON = withText "LogLevel" $ \t ->
    case parseLogLevel t of
      Just level -> pure level
      Nothing -> fail $ "Invalid log level: " <> toString t

instance FromHttpApiData LogLevel where
  parseQueryParam t =
    case parseLogLevel t of
      Just level -> Right level
      Nothing -> Left $ "Invalid log level: " <> t

instance ToParamSchema LogLevel where
  toParamSchema _ = mempty

-- | Convert log level to text representation
logLevelToText :: LogLevel -> Text
logLevelToText = \case
  Info -> "info"
  Warning -> "warning"
  Error -> "error"

-- | Parse text to log level (case-insensitive)
-- Also handles log-base's "attention" level (maps to Error)
parseLogLevel :: Text -> Maybe LogLevel
parseLogLevel t = case T.toLower t of
  "info" -> Just Info
  "warning" -> Just Warning
  "error" -> Just Error
  "attention" -> Just Error  -- log-base uses "attention" as highest level
  "trace" -> Just Info       -- log-base trace level
  _ -> Nothing

--------------------------------------------------------------------------------
-- Log Entry
--------------------------------------------------------------------------------

-- | Single log entry in JSON Lines format
data LogEntry = LogEntry
  { timestamp :: UTCTime
  -- ^ When the log was created (JSON field: "time")
  , level :: LogLevel
  -- ^ Severity level
  , message :: Text
  -- ^ Log message
  , component :: Text
  -- ^ Component/module that generated the log
  , metadata :: Maybe Value
  -- ^ Optional structured metadata (JSON field: "data")
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON LogEntry where
  toJSON entry =
    Aeson.object
      [ "time" .= entry.timestamp
      , "level" .= entry.level
      , "message" .= entry.message
      , "component" .= entry.component
      , "data" .= entry.metadata
      ]

instance FromJSON LogEntry where
  parseJSON = Aeson.withObject "LogEntry" $ \o ->
    LogEntry
      <$> o .: "time"
      <*> o .: "level"
      <*> o .: "message"
      <*> o .: "component"
      <*> o .:? "data"

-- Manual ToSchema instance because Value doesn't have ToSchema
instance ToSchema LogEntry where
  declareNamedSchema _ =
    pure $
      OpenApi.NamedSchema (Just "LogEntry") $
        mempty
          & #type ?~ OpenApi.OpenApiObject
          & #description ?~ "A structured log entry with timestamp, level, message, component, and optional metadata"

--------------------------------------------------------------------------------
-- API Types
--------------------------------------------------------------------------------

-- | Query parameters for log API endpoint
data LogQuery = LogQuery
  { date :: Day
  -- ^ Date to query (YYYY-MM-DD)
  , levelFilter :: Maybe LogLevel
  -- ^ Optional level filter
  , page :: Int
  -- ^ Page number (1-based, default: 1)
  , pageSize :: Int
  -- ^ Items per page (default: 100, max: 1000)
  }
  deriving stock (Show, Eq, Generic)

-- | Paginated log response
data LogsResponse = LogsResponse
  { entries :: [LogEntry]
  -- ^ Log entries for current page
  , total :: Int
  -- ^ Total number of matching entries
  , page :: Int
  -- ^ Current page number
  , pageSize :: Int
  -- ^ Items per page
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
