{- | Emby API client functions

This module provides servant-client functions for Emby API.
-}
module Infra.External.Emby.Client
  ( -- * Client Functions
    getLibraries
  , getItems
  , getSeriesItems
  , getSeasons
  , getEpisodeCount

    -- * Re-exports
  , mkEmbyBaseUrl
  )
where

import Servant.Client (ClientM)
import Servant.Client.Generic (AsClientT, genericClient)

import Infra.External.Emby.API (EmbyRoutes, mkEmbyBaseUrl)
import Infra.External.Emby.API qualified as API
import Infra.External.Emby.Types (EmbyItemsResponse (..), EmbyLibrary)

--------------------------------------------------------------------------------
-- Client
--------------------------------------------------------------------------------

embyClient :: EmbyRoutes (AsClientT ClientM)
embyClient = genericClient

--------------------------------------------------------------------------------
-- Operations
--------------------------------------------------------------------------------

-- | Get all virtual folders (libraries)
getLibraries :: Text -> ClientM [EmbyLibrary]
getLibraries apiKey = API.getLibraries embyClient apiKey

-- | Get items from a library
getItems ::
  Text ->
  -- | API Key
  Maybe Text ->
  -- | Parent ID (library ID)
  Maybe Text ->
  -- | Item types filter (e.g., "Series")
  ClientM EmbyItemsResponse
getItems apiKey parentId itemTypes =
  API.getItems embyClient apiKey parentId itemTypes (Just defaultFields) (Just True)

-- | Get all Series items from a library
getSeriesItems :: Text -> Text -> ClientM EmbyItemsResponse
getSeriesItems apiKey libraryId =
  API.getItems embyClient apiKey (Just libraryId) (Just "Series") (Just defaultFields) (Just True)

-- | Get seasons for a series
getSeasons :: Text -> Text -> ClientM EmbyItemsResponse
getSeasons apiKey seriesId =
  API.getSeasons embyClient seriesId apiKey (Just defaultFields)

-- | Get episode count for a series
-- Returns the total number of episode files in Emby for this series
getEpisodeCount :: Text -> Text -> ClientM Int
getEpisodeCount apiKey seriesId = do
  resp <- API.getItems embyClient apiKey (Just seriesId) (Just "Episode") Nothing (Just True)
  pure (totalRecordCount resp)

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Default fields to request from Emby API
defaultFields :: Text
defaultFields = "ProviderIds,Overview,PremiereDate,ProductionYear,ChildCount,ImageTags"
