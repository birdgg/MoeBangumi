{- | Logs API route definitions

Provides REST API for querying log entries:
- GET /api/logs - Query logs by date with optional level filter and pagination
- GET /api/logs/stream - SSE real-time log stream (today's logs only)
-}
module Web.API.Routes.Logs where

import Data.Aeson qualified as Aeson
import Data.ByteString.Builder (byteString, toLazyByteString)
import Data.Time (Day)
import Network.HTTP.Media ((//))
import Servant

import Infra.Logging.Types (LogEntry, LogLevel, LogsResponse)

--------------------------------------------------------------------------------
-- SSE Content Type
--------------------------------------------------------------------------------

-- | Server-Sent Events content type
data EventStream

instance Accept EventStream where
  contentType _ = "text" // "event-stream"

-- | Encode LogEntry as SSE data line: "data: <json>\n\n"
instance MimeRender EventStream LogEntry where
  mimeRender _ entry =
    toLazyByteString $
      byteString "data: "
        <> byteString (toStrict $ Aeson.encode entry)
        <> byteString "\n\n"

--------------------------------------------------------------------------------
-- API Routes
--------------------------------------------------------------------------------

type API = NamedRoutes Routes'

data Routes' mode = Routes'
  { getLogs
      :: mode
        :- Summary "Get logs by date"
          :> Description "Query log entries for a specific date with optional level filtering and pagination"
          :> QueryParam' '[Required, Strict] "date" Day
          :> QueryParam "level" LogLevel
          :> QueryParam "page" Int
          :> QueryParam "pageSize" Int
          :> Get '[JSON] LogsResponse
  , streamLogs
      :: mode
        :- Summary "Stream real-time logs"
          :> Description "SSE endpoint for real-time log streaming (today's logs only). Client should filter by level."
          :> "stream"
          :> StreamGet NoFraming EventStream (SourceIO LogEntry)
  }
  deriving stock (Generic)
