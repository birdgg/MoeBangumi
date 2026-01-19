{- | Torrent models

This module defines the core data types for torrent tracking:

- 'Torrent': Torrent metadata for bangumi episodes (single-episode from RSS)
-}
module Moe.Torrent (
  -- * Core Entity
  Torrent (..),
)
where

import Data.Aeson (FromJSON (..), ToJSON (..), eitherDecodeStrict, withObject, (.:), (.:?))
import Data.OpenApi (ToSchema)
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime)
import Database.SQLite.Simple (FromRow (..), field)
import Database.SQLite.Simple.FromField (FromField (..))
import Moe.Subtitle (Subtitle)

--------------------------------------------------------------------------------
-- Torrent
--------------------------------------------------------------------------------

-- | Torrent metadata for bangumi episodes (single-episode from RSS)
data Torrent = Torrent
  { id :: Int
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , subscriptionId :: Int
  -- ^ Associated subscription ID
  , rssId :: Maybe Int
  -- ^ Optional reference to source RSS
  , infoHash :: Text
  -- ^ BitTorrent info hash
  , torrentUrl :: Text
  -- ^ Torrent URL or magnet link
  , episodeNumber :: Maybe Int
  -- ^ Episode number (parsed from filename)
  , group :: Maybe Text
  -- ^ Subtitle group name
  , languages :: [Subtitle]
  -- ^ Subtitle languages (JSON array)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, ToSchema)

instance FromJSON Torrent where
  parseJSON = withObject "Torrent" $ \o ->
    Torrent
      <$> o .: "id"
      <*> o .: "createdAt"
      <*> o .: "updatedAt"
      <*> o .: "subscriptionId"
      <*> o .:? "rssId"
      <*> o .: "infoHash"
      <*> o .: "torrentUrl"
      <*> o .:? "episodeNumber"
      <*> o .:? "group"
      <*> o .: "languages"

-- | Parse JSON array from SQLite TEXT field (handles NULL as empty list)
newtype JsonList a = JsonList {unJsonList :: [a]}

instance (FromJSON a) => FromField (JsonList a) where
  fromField f = do
    mTxt <- fromField f
    case mTxt of
      Nothing -> pure (JsonList [])
      Just txt -> case eitherDecodeStrict (TE.encodeUtf8 txt) of
        Left err -> fail err
        Right xs -> pure (JsonList xs)

instance FromRow Torrent where
  fromRow =
    Torrent
      <$> field -- id
      <*> field -- created_at
      <*> field -- updated_at
      <*> field -- subscription_id
      <*> field -- rss_id
      <*> field -- info_hash
      <*> field -- torrent_url
      <*> field -- episode_number
      <*> field -- group
      <*> (unJsonList <$> field) -- languages
