{- | Emby API routes

Provides endpoints for importing Bangumi from Emby.
-}
module Web.API.Routes.Emby (
  API,
  Routes' (..),
  TestEmbyRequest (..),
  TestEmbyResponse (..),
  EmbyLibraryInfo (..),
  ImportEmbyRequest (..),
)
where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Servant

import App.Emby.Sync (ImportResult)
import Moe.Setting (EmbySettings)

type API = NamedRoutes Routes'

-- | Request body for testing Emby connection
newtype TestEmbyRequest = TestEmbyRequest
  { settings :: EmbySettings
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- | Response for Emby connection test
data TestEmbyResponse = TestEmbyResponse
  { success :: Bool
  , message :: Text
  , libraryCount :: Maybe Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- | Emby library info for selection
data EmbyLibraryInfo = EmbyLibraryInfo
  { id :: Text
  , name :: Text
  , collectionType :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- | Request body for importing from Emby
newtype ImportEmbyRequest = ImportEmbyRequest
  { libraryIds :: Maybe [Text]
  -- ^ Optional list of library IDs to import from. If empty/null, imports from all TV libraries.
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data Routes' mode = Routes'
  { libraries ::
      mode
        :- Summary "Get Emby libraries"
          :> Description "Get list of TV show libraries from Emby"
          :> "libraries"
          :> Get '[JSON] [EmbyLibraryInfo]
  , import_ ::
      mode
        :- Summary "Import Bangumi from Emby media library"
          :> Description "Import anime from Emby. Optionally specify library IDs to import from."
          :> "import"
          :> ReqBody '[JSON] ImportEmbyRequest
          :> Post '[JSON] ImportResult
  , test ::
      mode
        :- Summary "Test Emby connection"
          :> Description "Test connection to Emby server with provided settings"
          :> "test"
          :> ReqBody '[JSON] TestEmbyRequest
          :> Post '[JSON] TestEmbyResponse
  }
  deriving stock (Generic)
