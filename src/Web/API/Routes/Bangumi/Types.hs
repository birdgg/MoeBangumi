-- | Bangumi API DTO types
module Web.API.Routes.Bangumi.Types
  ( CreateBangumiDTO (..)
  , UpdateBangumiDTO (..)
  , SubscribeDTO (..)
  , UpdateSubscriptionDTO (..)
  , QuickSubscribeDTO (..)
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), withObject, (.:), (.:?))
import Data.OpenApi (ToSchema)
import Moe.Bangumi (Platform (..), SourceType (..))

--------------------------------------------------------------------------------
-- Bangumi Create DTO
--------------------------------------------------------------------------------

-- | Request body for creating a new bangumi (metadata only)
data CreateBangumiDTO = CreateBangumiDTO
  { mikanId :: Maybe Text
  -- ^ Mikan bangumi ID
  , bgmtvId :: Maybe Int
  -- ^ BGM.tv subject ID
  , tmdbId :: Maybe Int
  -- ^ TMDB ID
  , titleChinese :: Text
  -- ^ Chinese title (required)
  , titleJapanese :: Maybe Text
  -- ^ Japanese original name
  , season :: Maybe Int
  -- ^ Season number, defaults to 1
  , platform :: Maybe Platform
  -- ^ Platform type (TV, Movie, OVA), defaults to TV
  , totalEpisodes :: Maybe Int
  -- ^ Total episodes, defaults to 0 (unknown)
  , posterUrl :: Maybe Text
  -- ^ Poster image URL
  , airDate :: Maybe Text
  -- ^ First air date (YYYY-MM-DD format)
  , airWeek :: Maybe Int
  -- ^ Day of week when new episodes air (0=Sunday ~ 6=Saturday)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, ToSchema)

instance FromJSON CreateBangumiDTO where
  parseJSON = withObject "CreateBangumiDTO" $ \o ->
    CreateBangumiDTO
      <$> o .:? "mikanId"
      <*> o .:? "bgmtvId"
      <*> o .:? "tmdbId"
      <*> o .: "titleChinese"
      <*> o .:? "titleJapanese"
      <*> o .:? "season"
      <*> o .:? "platform"
      <*> o .:? "totalEpisodes"
      <*> o .:? "posterUrl"
      <*> o .:? "airDate"
      <*> o .:? "airWeek"

--------------------------------------------------------------------------------
-- Bangumi Update DTO
--------------------------------------------------------------------------------

-- | Request body for updating an existing bangumi
-- All fields are optional - only provided fields will be updated
data UpdateBangumiDTO = UpdateBangumiDTO
  { mikanId :: Maybe Text
  , bgmtvId :: Maybe Int
  , tmdbId :: Maybe Int
  , titleChinese :: Maybe Text
  , titleJapanese :: Maybe Text
  , season :: Maybe Int
  , platform :: Maybe Platform
  , totalEpisodes :: Maybe Int
  , posterUrl :: Maybe Text
  , airDate :: Maybe Text
  , airWeek :: Maybe Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

--------------------------------------------------------------------------------
-- Subscribe DTO
--------------------------------------------------------------------------------

-- | Request body for subscribing to a bangumi
data SubscribeDTO = SubscribeDTO
  { currentEpisode :: Maybe Int
  -- ^ Starting episode, defaults to 0
  , autoComplete :: Maybe Bool
  -- ^ Only download first matching episode per RSS check, defaults to True
  , savePath :: Maybe Text
  -- ^ Save path (required for downloading)
  , sourceType :: Maybe SourceType
  -- ^ Source type: other or bdrip, defaults to Other
  , episodeOffset :: Maybe Int
  -- ^ Episode offset for season-relative numbering, defaults to 0
  , rssUrl :: Maybe Text
  -- ^ Optional RSS feed URL - if provided, creates an RSS subscription
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

--------------------------------------------------------------------------------
-- Update Subscription DTO
--------------------------------------------------------------------------------

-- | Request body for updating a subscription
-- All fields are optional - only provided fields will be updated
data UpdateSubscriptionDTO = UpdateSubscriptionDTO
  { currentEpisode :: Maybe Int
  , autoComplete :: Maybe Bool
  , savePath :: Maybe Text
  , sourceType :: Maybe SourceType
  , episodeOffset :: Maybe Int
  , embyId :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

--------------------------------------------------------------------------------
-- Quick Subscribe DTO
--------------------------------------------------------------------------------

-- | Request body for quick subscribing from calendar
-- Contains all metadata needed to create bangumi and subscription in one call
data QuickSubscribeDTO = QuickSubscribeDTO
  { mikanId :: Text
  -- ^ Mikan bangumi ID (required for RSS URL generation)
  , bgmtvId :: Maybe Int
  -- ^ BGM.tv subject ID (used for episode offset calculation)
  , titleChinese :: Text
  -- ^ Chinese title
  , titleJapanese :: Maybe Text
  -- ^ Japanese title
  , posterUrl :: Maybe Text
  -- ^ Poster image URL
  , airDate :: Maybe Text
  -- ^ First air date (YYYY-MM-DD format)
  , airWeek :: Int
  -- ^ Day of week when new episodes air (1=Monday ~ 7=Sunday)
  , totalEpisodes :: Int
  -- ^ Total episodes
  , season :: Int
  -- ^ Season number
  , platform :: Platform
  -- ^ Platform type (TV, Movie, OVA)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
