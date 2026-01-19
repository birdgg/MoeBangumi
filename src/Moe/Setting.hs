{- | Application settings models

This module defines the configuration data types:

- 'DownloaderSettings': Downloader configuration (qBittorrent)
- 'FilterSettings': Global RSS filter patterns
- 'TelegramConfig': Telegram notification configuration
- 'NotificationSettings': Notification settings
- 'PrioritySettings': Subtitle group and language priorities
- 'TmdbSettings': TMDB API configuration
- 'EmbySettings': Emby server configuration
- 'Settings': Main application settings
-}
module Moe.Setting (
  -- * Downloader
  DownloaderSettings (..),

  -- * Filter
  FilterSettings (..),

  -- * Notification
  TelegramConfig (..),
  NotificationSettings (..),

  -- * Priority
  PrioritySettings (..),

  -- * TMDB
  TmdbSettings (..),

  -- * Emby
  EmbySettings (..),
  isEmbyEnabled,

  -- * Main Settings
  Settings (..),
)
where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.OpenApi (ToSchema)
import Data.Text qualified as T
import Moe.Subtitle (SubtitlePattern)

--------------------------------------------------------------------------------
-- Downloader Settings
--------------------------------------------------------------------------------

-- | Downloader configuration (qBittorrent)
data DownloaderSettings = DownloaderSettings
  { url :: Text
  -- ^ qBittorrent Web UI URL (e.g., http://localhost:8080)
  , username :: Text
  -- ^ qBittorrent username
  , password :: Text
  -- ^ qBittorrent password
  , savePath :: Text
  -- ^ Default save path for downloads
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

--------------------------------------------------------------------------------
-- Filter Settings
--------------------------------------------------------------------------------

-- | Filter configuration for RSS
data FilterSettings = FilterSettings
  { globalRssFilters :: [Text]
  -- ^ Global RSS filters (regex patterns to exclude)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

--------------------------------------------------------------------------------
-- Telegram Config
--------------------------------------------------------------------------------

-- | Telegram notification configuration
data TelegramConfig = TelegramConfig
  { botToken :: Text
  -- ^ Telegram Bot API token
  , chatId :: Text
  -- ^ Telegram chat ID to send notifications to
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

--------------------------------------------------------------------------------
-- Notification Settings
--------------------------------------------------------------------------------

-- | Notification configuration
data NotificationSettings = NotificationSettings
  { telegram :: TelegramConfig
  -- ^ Telegram configuration
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

--------------------------------------------------------------------------------
-- Priority Settings
--------------------------------------------------------------------------------

-- | Priority configuration for torrent selection and washing
data PrioritySettings = PrioritySettings
  { groups :: [Text]
  -- ^ Subtitle groups in priority order (first = highest priority)
  , languages :: [SubtitlePattern]
  -- ^ Subtitle language patterns in priority order
  -- Each pattern is a list like ["CHS", "JAP"] for 简日
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

--------------------------------------------------------------------------------
-- TMDB Settings
--------------------------------------------------------------------------------

-- | TMDB API configuration
data TmdbSettings = TmdbSettings
  { apiKey :: Text
  -- ^ TMDB API key
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

--------------------------------------------------------------------------------
-- Emby Settings
--------------------------------------------------------------------------------

-- | Emby server configuration
--
-- Emby is considered enabled when both 'url' and 'apiKey' are non-empty.
data EmbySettings = EmbySettings
  { url :: Text
  -- ^ Emby server URL (e.g., http://localhost:8096)
  , apiKey :: Text
  -- ^ Emby API key for authentication
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- | Check if Emby is enabled (both url and apiKey are non-empty)
isEmbyEnabled :: EmbySettings -> Bool
isEmbyEnabled settings =
  not (T.null settings.url) && not (T.null settings.apiKey)

--------------------------------------------------------------------------------
-- Main Settings
--------------------------------------------------------------------------------

-- | Application settings
data Settings = Settings
  { downloader :: DownloaderSettings
  -- ^ Downloader configuration
  , filter_ :: FilterSettings
  -- ^ Filter configuration
  , notification :: NotificationSettings
  -- ^ Notification configuration
  , priority :: PrioritySettings
  -- ^ Priority configuration for torrent selection
  , tmdb :: TmdbSettings
  -- ^ TMDB API configuration
  , emby :: EmbySettings
  -- ^ Emby server configuration
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
