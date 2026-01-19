{- | Emby API definition using Servant

This module defines the Emby REST API types for servant-client.
-}
module Infra.External.Emby.API
  ( -- * API Types
    EmbyAPI
  , EmbyRoutes (..)

    -- * Configuration
  , mkEmbyBaseUrl
  )
where

import Data.Text qualified as T
import Infra.External.Emby.Types (EmbyItemsResponse, EmbyLibrary)
import Moe.Setting (EmbySettings (..))
import Servant.API
import Servant.Client (BaseUrl (..), Scheme (..))

-- | Emby API
type EmbyAPI = NamedRoutes EmbyRoutes

-- | Emby API routes
data EmbyRoutes mode = EmbyRoutes
  { getLibraries
      :: mode
        :- "Library"
          :> "VirtualFolders"
          :> Header' '[Required, Strict] "X-Emby-Token" Text
          :> Get '[JSON] [EmbyLibrary]
  , getItems
      :: mode
        :- "Items"
          :> Header' '[Required, Strict] "X-Emby-Token" Text
          :> QueryParam "ParentId" Text
          :> QueryParam "IncludeItemTypes" Text
          :> QueryParam "Fields" Text
          :> QueryParam "Recursive" Bool
          :> Get '[JSON] EmbyItemsResponse
  , getSeasons
      :: mode
        :- "Shows"
          :> Capture "seriesId" Text
          :> "Seasons"
          :> Header' '[Required, Strict] "X-Emby-Token" Text
          :> QueryParam "Fields" Text
          :> Get '[JSON] EmbyItemsResponse
  }
  deriving stock (Generic)

-- | Build base URL from Emby settings
mkEmbyBaseUrl :: EmbySettings -> BaseUrl
mkEmbyBaseUrl cfg =
  let urlText = T.dropWhileEnd (== '/') cfg.url
      (schemeText, rest) = T.breakOn "://" urlText
      hostPort = T.drop 3 rest
      (host, portText) = T.breakOn ":" hostPort
      scheme = if schemeText == "https" then Https else Http
      defaultPort = if scheme == Https then 443 else 8096
      port =
        if T.null portText
          then defaultPort
          else fromMaybe defaultPort (readMaybe $ T.unpack $ T.drop 1 portText)
   in BaseUrl scheme (T.unpack host) port ""
