-- | TMDB API definition for servant-client
module Infra.External.Tmdb.API
  ( -- * API Types
    TmdbAPI
  , TmdbRoutes (..)

    -- * Configuration
  , tmdbBaseUrl
  , defaultLanguage
  )
where

import Servant.API
import Servant.Client (BaseUrl (..), Scheme (..))
import Infra.External.Tmdb.Types

-- | TMDB API v3
type TmdbAPI = "3" :> NamedRoutes TmdbRoutes

-- | TMDB API routes using NamedRoutes pattern
data TmdbRoutes mode = TmdbRoutes
  { discoverTv
      :: mode
        :- "discover"
          :> "tv"
          :> QueryParam' '[Required, Strict] "api_key" Text
          :> QueryParam' '[Required, Strict] "language" Text
          :> QueryParam' '[Required, Strict] "with_genres" Text
          :> QueryParam "with_text_query" Text
          :> Get '[JSON] (PaginatedResponse TvShow)
  , searchTv
      :: mode
        :- "search"
          :> "tv"
          :> QueryParam' '[Required, Strict] "api_key" Text
          :> QueryParam' '[Required, Strict] "language" Text
          :> QueryParam' '[Required, Strict] "query" Text
          :> Get '[JSON] (PaginatedResponse TvShow)
  , searchMulti
      :: mode
        :- "search"
          :> "multi"
          :> QueryParam' '[Required, Strict] "api_key" Text
          :> QueryParam' '[Required, Strict] "language" Text
          :> QueryParam' '[Required, Strict] "query" Text
          :> Get '[JSON] (PaginatedResponse MultiSearchResult)
  , getTvDetail
      :: mode
        :- "tv"
          :> Capture "tv_id" Int64
          :> QueryParam' '[Required, Strict] "api_key" Text
          :> QueryParam' '[Required, Strict] "language" Text
          :> Get '[JSON] TvDetail
  }
  deriving stock (Generic)

-- | Base URL for TMDB API
tmdbBaseUrl :: BaseUrl
tmdbBaseUrl = BaseUrl Https "api.themoviedb.org" 443 "/3"

-- | Default language for TMDB API responses
defaultLanguage :: Text
defaultLanguage = "zh-CN"
