-- | Series domain models
--
-- This module defines the core data types for anime series:
--
-- - 'Series': A collection of related bangumi (TV seasons, movies, OVAs)
--
-- A Series represents an IP/franchise that contains multiple Bangumi entries.
-- For example, "Attack on Titan" series contains S1, S2, S3, movies, and OVAs.
module Moe.Series
  ( -- * Core Entity
    Series (..)
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), withObject, (.:), (.:?))
import Data.OpenApi (ToSchema)
import Data.Time (UTCTime)
import Database.SQLite.Simple (FromRow (..), field)

--------------------------------------------------------------------------------
-- Series
--------------------------------------------------------------------------------

-- | Series represents a collection of related bangumi
--
-- A series groups multiple bangumi entries that belong to the same IP/franchise.
-- Each bangumi can optionally reference a series via 'seriesId'.
data Series = Series
  { id :: Int
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  -- Titles
  , titleChinese :: Text
  -- ^ Chinese title (primary display)
  , titleJapanese :: Maybe Text
  -- ^ Japanese original name
  -- Metadata
  , posterUrl :: Maybe Text
  -- ^ Series poster image URL
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, ToSchema)

instance FromJSON Series where
  parseJSON = withObject "Series" $ \o ->
    Series
      <$> o .: "id"
      <*> o .: "createdAt"
      <*> o .: "updatedAt"
      <*> o .: "titleChinese"
      <*> o .:? "titleJapanese"
      <*> o .:? "posterUrl"

instance FromRow Series where
  fromRow =
    Series
      <$> field -- id
      <*> field -- created_at
      <*> field -- updated_at
      <*> field -- title_chinese
      <*> field -- title_japanese
      <*> field -- poster_url
