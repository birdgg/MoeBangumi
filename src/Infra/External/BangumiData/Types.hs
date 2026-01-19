{- | Bangumi-data types for parsing GitHub JSON data

This module defines types for parsing the bangumi-data JSON format
from https://github.com/bangumi-data/bangumi-data
-}
module Infra.External.BangumiData.Types (
  -- * Errors
  BangumiDataError (..),

  -- * Raw JSON Types
  BangumiDataItem (..),
  TitleTranslate (..),
  Site (..),

  -- * Extracted Calendar Item
  CalendarItem (..),
) where

import Data.Aeson (FromJSON (..), ToJSON (..), withObject, (.:), (.:?))
import Data.OpenApi (ToSchema)
import Data.Text.Display (Display (..))

--------------------------------------------------------------------------------
-- Errors
--------------------------------------------------------------------------------

-- | Errors that can occur when fetching from bangumi-data
data BangumiDataError
  = NetworkError Text
  | JsonParseError Text
  deriving stock (Show, Eq)

instance Display BangumiDataError where
  displayBuilder = \case
    NetworkError msg -> "Bangumi-data network error: " <> displayBuilder msg
    JsonParseError msg -> "Bangumi-data JSON parse error: " <> displayBuilder msg

--------------------------------------------------------------------------------
-- Raw JSON Types
--------------------------------------------------------------------------------

-- | Title translations in different languages
data TitleTranslate = TitleTranslate
  { en :: Maybe [Text]
  , zhHans :: Maybe [Text]
  , zhHant :: Maybe [Text]
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON TitleTranslate where
  parseJSON = withObject "TitleTranslate" $ \o ->
    TitleTranslate
      <$> o .:? "en"
      <*> o .:? "zh-Hans"
      <*> o .:? "zh-Hant"

-- | Site entry from bangumi-data
data Site = Site
  { site :: Text
  -- ^ Site name (e.g., "mikan", "bangumi", "tmdb")
  , siteId :: Text
  -- ^ Site-specific ID
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON Site where
  parseJSON = withObject "Site" $ \o ->
    Site
      <$> o .: "site"
      <*> o .: "id"

-- | Raw item from bangumi-data JSON
data BangumiDataItem = BangumiDataItem
  { title :: Text
  -- ^ Japanese title
  , titleTranslate :: TitleTranslate
  -- ^ Translations
  , itemType :: Text
  -- ^ Type (tv, movie, ova, web)
  , lang :: Text
  -- ^ Original language
  , officialSite :: Maybe Text
  -- ^ Official website URL
  , begin :: Text
  -- ^ Start date (ISO 8601)
  , end :: Text
  -- ^ End date (ISO 8601, empty string if ongoing)
  , broadcast :: Maybe Text
  -- ^ Broadcast schedule (RFC 5545 recurrence rule)
  , comment :: Text
  -- ^ Additional notes
  , sites :: [Site]
  -- ^ Available sites
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON BangumiDataItem where
  parseJSON = withObject "BangumiDataItem" $ \o ->
    BangumiDataItem
      <$> o .: "title"
      <*> o .: "titleTranslate"
      <*> o .: "type"
      <*> o .: "lang"
      <*> o .:? "officialSite"
      <*> o .: "begin"
      <*> o .: "end"
      <*> o .:? "broadcast"
      <*> o .: "comment"
      <*> o .: "sites"

--------------------------------------------------------------------------------
-- Calendar Item
--------------------------------------------------------------------------------

-- | Extracted calendar item with normalized IDs
data CalendarItem = CalendarItem
  { mikanId :: Maybe Text
  -- ^ Mikan ID (from sites array)
  , bgmtvId :: Maybe Int
  -- ^ BGM.tv subject ID (from sites array)
  , tmdbId :: Maybe Int
  -- ^ TMDB ID (extracted from "tv/xxx" format)
  , titleChinese :: Text
  -- ^ Chinese title (zh-Hans preferred)
  , titleJapanese :: Text
  -- ^ Japanese title
  , officialSite :: Maybe Text
  -- ^ Official website URL
  , airDate :: Maybe Text
  -- ^ First air date (YYYY-MM-DD)
  , airWeekday :: Int
  -- ^ Day of week (1=Monday, 7=Sunday)
  , itemType :: Text
  -- ^ Type (tv, movie, ova, web)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
