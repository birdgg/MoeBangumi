{- | Bangumi domain models

This module defines the core data types for anime (bangumi) metadata:

- 'Platform': Type of anime (TV, Movie, OVA)
- 'Bangumi': Core entity for anime metadata (what an anime IS)
- 'BangumiWithRss': Bangumi with associated RSS entries (for subscribed bangumi)
-}
module Moe.Bangumi (
  -- * Enums
  Platform (..),
  SourceType (..),

  -- * Core Entity
  Bangumi (..),

  -- * Composite Types
  BangumiWithRss (..),

  -- * Utilities
  getYear,
)
where

import Data.Aeson (FromJSON (..), ToJSON (..), withObject, (.!=), (.:), (.:?))
import Data.OpenApi (ToSchema (..))
import Data.OpenApi qualified as OpenApi
import Data.OpenApi.Optics ()
import Data.Text qualified as T
import Data.Time (UTCTime)
import Database.SQLite.Simple (FromRow (..), field)
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField (ToField (..))
import Moe.Rss (Rss)
import Optics.Core ((?~))

--------------------------------------------------------------------------------
-- Platform
--------------------------------------------------------------------------------

-- | Platform type for bangumi (TV, Movie, OVA)
data Platform
  = TV
  | Movie
  | OVA
  deriving stock (Show, Eq, Generic)

parsePlatform :: Text -> Platform
parsePlatform t = case T.toLower t of
  "movie" -> Movie
  "ova" -> OVA
  _ -> TV

platformToText :: Platform -> Text
platformToText = \case
  TV -> "tv"
  Movie -> "movie"
  OVA -> "ova"

instance ToJSON Platform where
  toJSON = toJSON . platformToText

instance FromJSON Platform where
  parseJSON v = parsePlatform <$> parseJSON v

instance FromField Platform where
  fromField f = parsePlatform <$> fromField f

instance ToField Platform where
  toField = toField . platformToText

instance ToSchema Platform where
  declareNamedSchema _ =
    pure $
      OpenApi.NamedSchema (Just "Platform") $
        mempty
          & #type
          ?~ OpenApi.OpenApiString
          & #enum
          ?~ map toJSON [TV, Movie, OVA]
          & #description
          ?~ "Platform type for bangumi (tv, movie, ova)"

--------------------------------------------------------------------------------
-- SourceType
--------------------------------------------------------------------------------

-- | Source type for bangumi (WebRip, BDRip)
data SourceType
  = BDRip
  | Other
  deriving stock (Show, Eq, Generic)

parseSourceType :: Text -> SourceType
parseSourceType t = case T.toLower t of
  "bdrip" -> BDRip
  _ -> Other

sourceTypeToText :: SourceType -> Text
sourceTypeToText = \case
  BDRip -> "bdrip"
  Other -> "other"

instance ToJSON SourceType where
  toJSON = toJSON . sourceTypeToText

instance FromJSON SourceType where
  parseJSON v = parseSourceType <$> parseJSON v

instance FromField SourceType where
  fromField f = parseSourceType <$> fromField f

instance ToField SourceType where
  toField = toField . sourceTypeToText

instance ToSchema SourceType where
  declareNamedSchema _ =
    pure $
      OpenApi.NamedSchema (Just "SourceType") $
        mempty
          & #type
          ?~ OpenApi.OpenApiString
          & #enum
          ?~ map toJSON [BDRip, Other]
          & #description
          ?~ "Source type for bangumi (bdrip, other)"

--------------------------------------------------------------------------------
-- Bangumi
--------------------------------------------------------------------------------

-- | Bangumi metadata (what an anime IS)
data Bangumi = Bangumi
  { id :: Int
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , -- External service IDs
    mikanId :: Maybe Text
  -- ^ Mikan bangumi ID
  , bgmtvId :: Maybe Int
  -- ^ BGM.tv subject ID
  , tmdbId :: Maybe Int
  -- ^ TMDB ID
  , titleChinese :: Text
  -- ^ Chinese title (primary display)
  , titleJapanese :: Maybe Text
  -- ^ Japanese original name
  , season :: Int
  -- ^ Season number, 0 for movie
  , platform :: Platform
  -- ^ Platform type (TV, Movie, OVA)
  , totalEpisodes :: Int
  -- ^ Total episodes (0=unknown)
  , posterUrl :: Maybe Text
  -- ^ Poster image URL
  , airDate :: Maybe Text
  -- ^ First air date (YYYY-MM-DD format)
  , airWeek :: Int
  -- ^ Day of week when new episodes air (0=Sunday ~ 6=Saturday)
  , tmdbLookupAt :: Maybe UTCTime
  -- ^ Last TMDB lookup attempt timestamp
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, ToSchema)

instance FromJSON Bangumi where
  parseJSON = withObject "Bangumi" $ \o ->
    Bangumi
      <$> o .: "id"
      <*> o .: "createdAt"
      <*> o .: "updatedAt"
      <*> o .:? "mikanId"
      <*> o .:? "bgmtvId"
      <*> o .:? "tmdbId"
      <*> o .: "titleChinese"
      <*> o .:? "titleJapanese"
      <*> o .:? "season" .!= 1
      <*> o .:? "platform" .!= TV
      <*> o .:? "totalEpisodes" .!= 0
      <*> o .:? "posterUrl"
      <*> o .:? "airDate"
      <*> o .:? "airWeek" .!= 0
      <*> o .:? "tmdbLookupAt"

instance FromRow Bangumi where
  fromRow =
    Bangumi
      <$> field -- id
      <*> field -- created_at
      <*> field -- updated_at
      <*> field -- mikan_id
      <*> field -- bgmtv_id
      <*> field -- tmdb_id
      <*> field -- title_chinese
      <*> field -- title_japanese
      <*> field -- season
      <*> field -- platform
      <*> field -- total_episodes
      <*> field -- poster_url
      <*> field -- air_date
      <*> field -- air_week
      <*> field -- tmdb_lookup_at

--------------------------------------------------------------------------------
-- BangumiWithRss
--------------------------------------------------------------------------------

-- | Bangumi with associated RSS subscriptions (for subscribed bangumi)
data BangumiWithRss = BangumiWithRss
  { bangumi :: Bangumi
  , rssEntries :: [Rss]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, ToSchema)

instance FromJSON BangumiWithRss where
  parseJSON = withObject "BangumiWithRss" $ \o ->
    BangumiWithRss
      <$> o .: "bangumi"
      <*> o .: "rssEntries"

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

{- | Extract year from a Bangumi's airDate

The airDate is expected to be in YYYY-MM-DD format.
Returns Nothing if airDate is not set or has invalid format.

>>> getYear bangumi { airDate = Just "2024-04-01" }
Just "2024"

>>> getYear bangumi { airDate = Nothing }
Nothing
-}
getYear :: Bangumi -> Maybe Text
getYear bangumi = extractYear bangumi.airDate
 where
  extractYear :: Maybe Text -> Maybe Text
  extractYear Nothing = Nothing
  extractYear (Just airDate) =
    case T.splitOn "-" airDate of
      (year : _) | T.length year == 4 -> Just year
      _ -> Nothing
