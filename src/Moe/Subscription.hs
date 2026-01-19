{- | Subscription domain models

This module defines the core data types for subscription tracking:

- 'Subscription': User's tracking state for a bangumi (MY tracking progress)
- 'SubscribedBangumi': Full subscription data with bangumi metadata and RSS entries
-}
module Moe.Subscription (
  -- * Core Entity
  Subscription (..),

  -- * Composite Types
  SubscribedBangumi (..),

  -- * Utilities
  isBDRip,
)
where

import Data.Aeson (FromJSON (..), ToJSON (..), withObject, (.!=), (.:), (.:?))
import Data.OpenApi (ToSchema)
import Data.Time (UTCTime)
import Database.SQLite.Simple (FromRow (..), field)
import Moe.Bangumi (Bangumi (..), SourceType (..))
import Moe.Rss (Rss)

--------------------------------------------------------------------------------
-- Subscription
--------------------------------------------------------------------------------

-- | User's tracking state for a bangumi
data Subscription = Subscription
  { id :: Int
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , bangumiId :: Int
  -- ^ Reference to the bangumi being tracked
  , currentEpisode :: Int
  -- ^ Current downloaded episode
  , autoComplete :: Bool
  -- ^ Only download first matching episode per RSS check
  , savePath :: Maybe Text
  -- ^ Save path (required for downloading)
  , sourceType :: SourceType
  -- ^ Source type: other or bdrip
  , episodeOffset :: Int
  -- ^ Episode offset for season-relative numbering
  , embyId :: Maybe Text
  -- ^ Emby item ID for library sync
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, ToSchema)

instance FromJSON Subscription where
  parseJSON = withObject "Subscription" $ \o ->
    Subscription
      <$> o .: "id"
      <*> o .: "createdAt"
      <*> o .: "updatedAt"
      <*> o .: "bangumiId"
      <*> o .:? "currentEpisode" .!= 0
      <*> o .:? "autoComplete" .!= True
      <*> o .:? "savePath"
      <*> o .:? "sourceType" .!= Other
      <*> o .:? "episodeOffset" .!= 0
      <*> o .:? "embyId"

instance FromRow Subscription where
  fromRow =
    Subscription
      <$> field -- id
      <*> field -- created_at
      <*> field -- updated_at
      <*> field -- bangumi_id
      <*> field -- current_episode
      <*> ((/= (0 :: Int)) <$> field) -- auto_complete
      <*> field -- save_path
      <*> field -- source_type
      <*> field -- episode_offset
      <*> field -- emby_id

--------------------------------------------------------------------------------
-- SubscribedBangumi
--------------------------------------------------------------------------------

-- | Full subscription data with bangumi metadata and RSS entries
data SubscribedBangumi = SubscribedBangumi
  { subscription :: Subscription
  , bangumi :: Bangumi
  , rssEntries :: [Rss]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, ToSchema)

instance FromJSON SubscribedBangumi where
  parseJSON = withObject "SubscribedBangumi" $ \o ->
    SubscribedBangumi
      <$> o .: "subscription"
      <*> o .: "bangumi"
      <*> o .: "rssEntries"

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Check if subscription is for BDRip source
isBDRip :: Subscription -> Bool
isBDRip sub = sub.sourceType == BDRip
