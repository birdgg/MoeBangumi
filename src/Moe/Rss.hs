-- | RSS subscription models
--
-- This module defines the core data types for RSS subscriptions:
--
-- - 'Rss': RSS feed subscription linked to a subscription
module Moe.Rss
  ( -- * Core Entity
    Rss (..)
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), withObject, (.:), (.:?), (.!=))
import Data.OpenApi (ToSchema)
import Data.Time (UTCTime)
import Database.SQLite.Simple (FromRow (..), field)

--------------------------------------------------------------------------------
-- Rss
--------------------------------------------------------------------------------

-- | RSS subscription for tracking bangumi releases
data Rss = Rss
  { id :: Int
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , subscriptionId :: Int
  , url :: Text
  , enabled :: Bool
  , lastProcessedPubDate :: Maybe Text
  -- ^ The pubDate of the last processed RSS item (RFC 822 date string),
  -- used for time-based filtering of already processed items
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, ToSchema)

instance FromJSON Rss where
  parseJSON = withObject "Rss" $ \o ->
    Rss
      <$> o .: "id"
      <*> o .: "createdAt"
      <*> o .: "updatedAt"
      <*> o .: "subscriptionId"
      <*> o .: "url"
      <*> o .:? "enabled" .!= True
      <*> o .:? "lastProcessedPubDate"

instance FromRow Rss where
  fromRow =
    Rss
      <$> field -- id
      <*> field -- created_at
      <*> field -- updated_at
      <*> field -- subscription_id
      <*> field -- url
      <*> ((/= (0 :: Int)) <$> field) -- enabled (SQLite stores as INTEGER)
      <*> field -- last_processed_pub_date
