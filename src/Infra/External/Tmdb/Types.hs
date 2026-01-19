-- | Types for TMDB API
module Infra.External.Tmdb.Types
  ( -- * TV Show
    TvShow (..)
  , TvDetail (..)

    -- * Multi Search
  , MultiSearchResult (..)
  , MediaType (..)

    -- * Pagination
  , PaginatedResponse (..)

    -- * Request Parameters
  , DiscoverBangumiParams (..)

    -- * Errors
  , TmdbError (..)
  )
where

import Data.Aeson

-- | TV Show from TMDB API (discover/search results)
data TvShow = TvShow
  { id :: Int64
  , name :: Text
  , originalName :: Text
  , overview :: Text
  , posterPath :: Maybe Text
  , backdropPath :: Maybe Text
  , firstAirDate :: Maybe Text
  , voteAverage :: Double
  , voteCount :: Int64
  , popularity :: Double
  , genreIds :: [Int64]
  , originCountry :: [Text]
  , originalLanguage :: Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON TvShow where
  parseJSON = withObject "TvShow" $ \o ->
    TvShow
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "original_name"
      <*> o .: "overview"
      <*> o .:? "poster_path"
      <*> o .:? "backdrop_path"
      <*> o .:? "first_air_date"
      <*> o .: "vote_average"
      <*> o .: "vote_count"
      <*> o .: "popularity"
      <*> o .:? "genre_ids" .!= []
      <*> o .:? "origin_country" .!= []
      <*> o .: "original_language"

-- | TV Detail from TMDB API (tv/{id} endpoint)
data TvDetail = TvDetail
  { id :: Int64
  , name :: Text
  , originalName :: Text
  , overview :: Text
  , posterPath :: Maybe Text
  , backdropPath :: Maybe Text
  , firstAirDate :: Maybe Text
  , lastAirDate :: Maybe Text
  , voteAverage :: Double
  , voteCount :: Int64
  , popularity :: Double
  , originCountry :: [Text]
  , originalLanguage :: Text
  , status :: Text
  , numberOfSeasons :: Int64
  , numberOfEpisodes :: Int64
  , homepage :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON TvDetail where
  parseJSON = withObject "TvDetail" $ \o ->
    TvDetail
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "original_name"
      <*> o .: "overview"
      <*> o .:? "poster_path"
      <*> o .:? "backdrop_path"
      <*> o .:? "first_air_date"
      <*> o .:? "last_air_date"
      <*> o .: "vote_average"
      <*> o .: "vote_count"
      <*> o .: "popularity"
      <*> o .:? "origin_country" .!= []
      <*> o .: "original_language"
      <*> o .: "status"
      <*> o .: "number_of_seasons"
      <*> o .: "number_of_episodes"
      <*> o .:? "homepage"

-- | Paginated response from TMDB API
data PaginatedResponse a = PaginatedResponse
  { page :: Int64
  , results :: [a]
  , totalPages :: Int64
  , totalResults :: Int64
  }
  deriving stock (Show, Eq, Generic)

instance (FromJSON a) => FromJSON (PaginatedResponse a) where
  parseJSON = withObject "PaginatedResponse" $ \o ->
    PaginatedResponse
      <$> o .: "page"
      <*> o .: "results"
      <*> o .: "total_pages"
      <*> o .: "total_results"

-- | Parameters for discover bangumi endpoint
data DiscoverBangumiParams = DiscoverBangumiParams
  { withTextQuery :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

-- | Media type for multi search results
data MediaType = Movie | Tv | Person
  deriving stock (Show, Eq, Generic)

instance FromJSON MediaType where
  parseJSON = withText "MediaType" $ \case
    "movie" -> pure Movie
    "tv" -> pure Tv
    "person" -> pure Person
    other -> fail $ "Unknown media_type: " <> toString other

-- | Multi search result from TMDB API
-- This is a union type that can represent movie, tv, or person results
data MultiSearchResult = MultiSearchResult
  { id :: Int64
  , mediaType :: MediaType
  , name :: Maybe Text
  -- ^ For TV shows
  , title :: Maybe Text
  -- ^ For movies
  , originalName :: Maybe Text
  -- ^ For TV shows
  , originalTitle :: Maybe Text
  -- ^ For movies
  , posterPath :: Maybe Text
  , firstAirDate :: Maybe Text
  -- ^ For TV shows
  , releaseDate :: Maybe Text
  -- ^ For movies
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON MultiSearchResult where
  parseJSON = withObject "MultiSearchResult" $ \o ->
    MultiSearchResult
      <$> o .: "id"
      <*> o .: "media_type"
      <*> o .:? "name"
      <*> o .:? "title"
      <*> o .:? "original_name"
      <*> o .:? "original_title"
      <*> o .:? "poster_path"
      <*> o .:? "first_air_date"
      <*> o .:? "release_date"

-- | Errors that can occur during TMDB API operations
data TmdbError
  = -- | Network request failed
    NetworkError Text
  | -- | Failed to parse JSON response
    ParseError Text
  | -- | API returned an error status (status code, message)
    ApiError Int Text
  deriving stock (Show, Eq)
