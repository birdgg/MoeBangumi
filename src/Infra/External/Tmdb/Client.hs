{- | TMDB API client using servant-client

This module provides ClientM functions for interacting with the TMDB API.
-}
module Infra.External.Tmdb.Client (
  -- * API Functions
  discoverBangumi,
  searchTv,
  searchMulti,
  getTvDetail,

  -- * Re-exports
  module Infra.External.Tmdb.Types,
  tmdbBaseUrl,
  defaultLanguage,
)
where

import Infra.External.Tmdb.API (TmdbRoutes, defaultLanguage, tmdbBaseUrl)
import Infra.External.Tmdb.API qualified as API
import Infra.External.Tmdb.Types
import Servant.Client (ClientM)
import Servant.Client.Generic (AsClientT, genericClient)

-- | Generated client functions record (internal)
client' :: TmdbRoutes (AsClientT ClientM)
client' = genericClient

-- | Discover anime (uses genre 16: Animation)
discoverBangumi :: Text -> DiscoverBangumiParams -> ClientM (PaginatedResponse TvShow)
discoverBangumi apiKey params =
  API.discoverTv client' apiKey defaultLanguage ("16" :: Text) params.withTextQuery

-- | Search TV shows
searchTv :: Text -> Text -> ClientM (PaginatedResponse TvShow)
searchTv apiKey = API.searchTv client' apiKey defaultLanguage

-- | Search multi (movies, TV shows, and people)
searchMulti :: Text -> Text -> ClientM (PaginatedResponse MultiSearchResult)
searchMulti apiKey = API.searchMulti client' apiKey defaultLanguage

-- | Get TV detail
getTvDetail :: Text -> Int64 -> ClientM TvDetail
getTvDetail apiKey tvId =
  API.getTvDetail client' tvId apiKey defaultLanguage
