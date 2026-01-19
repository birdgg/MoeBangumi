{- | Metadata types for BGM.tv and TMDB data

This module defines metadata types for external sources (BGM.tv and TMDB).
-}
module Moe.Metadata (
  -- * Search Results
  BgmtvSearchItem (..),
  TmdbSearchItem (..),
  TmdbMediaType (..),

  -- * Mikan Types
  MikanSearchResult (..),
  MikanBangumiDetail (..),
  MikanSubgroup (..),
  MikanEpisode (..),

  -- * Episode Types
  EpisodeInfo (..),
) where

import Data.Aeson (FromJSON (..), ToJSON (..), withText)
import Data.OpenApi (ToSchema)

--------------------------------------------------------------------------------
-- Search Results
--------------------------------------------------------------------------------

-- | BGM.tv search result item
--
-- Uses parseBgmtvName to extract titleChinese, titleJapanese, and season
-- from the raw API response
data BgmtvSearchItem = BgmtvSearchItem
  { id :: Int64
  , titleChinese :: Text
  -- ^ Chinese title (parsed from nameCn, season suffix removed)
  , titleJapanese :: Text
  -- ^ Japanese title (parsed from name, season suffix removed)
  , posterUrl :: Text
  -- ^ Cover image URL
  , airDate :: Text
  -- ^ First air date
  , totalEpisodes :: Int64
  -- ^ Total episodes
  , season :: Int
  -- ^ Season number (extracted by parser)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- | TMDB media type
data TmdbMediaType = TmdbMovie | TmdbTv
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToSchema)

instance ToJSON TmdbMediaType where
  toJSON TmdbMovie = "movie"
  toJSON TmdbTv = "tv"

instance FromJSON TmdbMediaType where
  parseJSON = withText "TmdbMediaType" $ \case
    "movie" -> pure TmdbMovie
    "tv" -> pure TmdbTv
    other -> fail $ "Unknown media type: " <> toString other

-- | TMDB search result item (simplified)
data TmdbSearchItem = TmdbSearchItem
  { id :: Int64
  , name :: Text
  -- ^ Display name (name for TV, title for movie)
  , mediaType :: TmdbMediaType
  -- ^ Media type (movie or tv)
  , airDate :: Maybe Text
  -- ^ First air date (TV) or release date (movie)
  , posterPath :: Maybe Text
  -- ^ Poster image path
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

--------------------------------------------------------------------------------
-- Mikan Types
--------------------------------------------------------------------------------

-- | Mikan search result item
data MikanSearchResult = MikanSearchResult
  { id :: Text
  -- ^ Mikan bangumi ID
  , name :: Text
  -- ^ Bangumi name
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- | Mikan episode info
data MikanEpisode = MikanEpisode
  { name :: Text
  -- ^ Episode title/filename
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- | Mikan subgroup info with RSS URL
data MikanSubgroup = MikanSubgroup
  { id :: Text
  -- ^ Subgroup ID
  , name :: Maybe Text
  -- ^ Subgroup name
  , rssUrl :: Text
  -- ^ RSS feed URL for this subgroup
  , episodes :: [MikanEpisode]
  -- ^ Recent episodes from this subgroup
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- | Mikan bangumi detail with subgroups
data MikanBangumiDetail = MikanBangumiDetail
  { id :: Text
  -- ^ Mikan bangumi ID
  , name :: Text
  -- ^ Bangumi name
  , subgroups :: [MikanSubgroup]
  -- ^ Available subgroups with RSS
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

--------------------------------------------------------------------------------
-- Episode Types
--------------------------------------------------------------------------------

-- | Episode info from BGM.tv
data EpisodeInfo = EpisodeInfo
  { id :: Int64
  -- ^ Episode ID
  , sort :: Double
  -- ^ Sort order (absolute episode number)
  , ep :: Maybe Double
  -- ^ Episode number within season
  , name :: Text
  -- ^ Japanese title
  , nameCn :: Text
  -- ^ Chinese title
  , airdate :: Text
  -- ^ Air date
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
