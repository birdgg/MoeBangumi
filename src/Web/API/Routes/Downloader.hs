{- | Downloader API routes

Provides endpoints for testing downloader connection.
-}
module Web.API.Routes.Downloader (
  API,
  Routes' (..),
  TestDownloaderRequest (..),
  TestDownloaderResponse (..),
)
where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Servant

import Moe.Setting (DownloaderSettings)

type API = NamedRoutes Routes'

-- | Request body for testing downloader connection
newtype TestDownloaderRequest = TestDownloaderRequest
  { settings :: DownloaderSettings
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- | Response for downloader connection test
data TestDownloaderResponse = TestDownloaderResponse
  { success :: Bool
  , message :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data Routes' mode = Routes'
  { test ::
      mode
        :- Summary "Test qBittorrent connection"
          :> Description "Test connection and authentication to qBittorrent with provided settings"
          :> "test"
          :> ReqBody '[JSON] TestDownloaderRequest
          :> Post '[JSON] TestDownloaderResponse
  }
  deriving stock (Generic)
