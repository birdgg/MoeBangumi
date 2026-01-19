{- | Calendar domain models

This module defines the data types for seasonal calendar:

- 'Season': Anime season (Winter, Spring, Summer, Fall)
- 'Calendar': Calendar entry linking bangumi to a season
- 'CalendarDay': Grouped calendar entries by weekday
- 'CalendarSubject': Unified subject for calendar display
-}
module Moe.Calendar (
  -- * Season
  Season (..),
  seasonToText,
  parseSeasonText,

  -- * Calendar Entity
  Calendar (..),

  -- * Calendar Display Types
  CalendarDay (..),
  CalendarSubject (..),
  Weekday (..),
) where

import Data.Aeson (FromJSON (..), ToJSON (..), withText)
import Data.OpenApi (ToParamSchema, ToSchema (..))
import Data.OpenApi qualified as OpenApi
import Data.Text qualified as T
import Data.Time (UTCTime)
import Database.SQLite.Simple (FromRow (..), field)
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField (ToField (..))
import Moe.Bangumi (Platform)
import Optics.Core ((?~))
import Servant (FromHttpApiData (..))

--------------------------------------------------------------------------------
-- Season
--------------------------------------------------------------------------------

-- | Anime broadcasting season
data Season
  = -- | January - March
    Winter
  | -- | April - June
    Spring
  | -- | July - September
    Summer
  | -- | October - December
    Fall
  deriving stock (Show, Eq, Generic, Enum, Bounded)

seasonToText :: Season -> Text
seasonToText = \case
  Winter -> "winter"
  Spring -> "spring"
  Summer -> "summer"
  Fall -> "fall"

parseSeasonText :: Text -> Maybe Season
parseSeasonText t = case T.toLower t of
  "winter" -> Just Winter
  "spring" -> Just Spring
  "summer" -> Just Summer
  "fall" -> Just Fall
  _ -> Nothing

instance ToJSON Season where
  toJSON = toJSON . seasonToText

instance FromJSON Season where
  parseJSON = withText "Season" $ \t ->
    case parseSeasonText t of
      Just s -> pure s
      Nothing -> fail $ "Invalid season: " <> toString t

instance FromField Season where
  fromField f = do
    t <- fromField f
    case parseSeasonText t of
      Just s -> pure s
      Nothing -> fail $ "Invalid season in database: " <> T.unpack t

instance ToField Season where
  toField = toField . seasonToText

instance FromHttpApiData Season where
  parseQueryParam t = case parseSeasonText t of
    Just s -> Right s
    Nothing -> Left $ "Invalid season: " <> t

instance ToSchema Season where
  declareNamedSchema _ =
    pure $
      OpenApi.NamedSchema (Just "Season") $
        mempty
          & #type
          ?~ OpenApi.OpenApiString
          & #enum
          ?~ map toJSON [Winter, Spring, Summer, Fall]
          & #description
          ?~ "Anime broadcasting season"

instance ToParamSchema Season where
  toParamSchema _ =
    mempty
      & #type
      ?~ OpenApi.OpenApiString
      & #enum
      ?~ map toJSON [Winter, Spring, Summer, Fall]

--------------------------------------------------------------------------------
-- Calendar
--------------------------------------------------------------------------------

-- | Calendar entry linking bangumi to a season
data Calendar = Calendar
  { id :: Int
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , year :: Int
  , season :: Season
  , bangumiId :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance FromRow Calendar where
  fromRow =
    Calendar
      <$> field -- id
      <*> field -- created_at
      <*> field -- updated_at
      <*> field -- year
      <*> field -- season
      <*> field -- bangumi_id

--------------------------------------------------------------------------------
-- Calendar Display Types
--------------------------------------------------------------------------------

-- | Weekday for calendar display
data Weekday = Weekday
  { id :: Int
  -- ^ 1=Monday, 7=Sunday
  , name :: Text
  -- ^ e.g., "Monday"
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

{- | Unified subject for calendar display
Contains metadata from bangumi table plus subscription status
-}
data CalendarSubject = CalendarSubject
  { mikanId :: Maybe Text
  , bgmtvId :: Maybe Int
  , tmdbId :: Maybe Int
  , titleChinese :: Text
  , titleJapanese :: Maybe Text
  , posterUrl :: Maybe Text
  , airDate :: Maybe Text
  , airWeek :: Int
  , totalEpisodes :: Int
  , season :: Int
  , platform :: Platform
  , subscribed :: Bool
  -- ^ Whether user has subscribed to this bangumi
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- | Calendar entries grouped by weekday
data CalendarDay = CalendarDay
  { weekday :: Weekday
  , items :: [CalendarSubject]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
