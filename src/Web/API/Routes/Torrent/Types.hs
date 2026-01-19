{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Torrent API DTO types
module Web.API.Routes.Torrent.Types
  ( AddTorrentDTO (..)
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)

--------------------------------------------------------------------------------
-- Add Torrent DTO
--------------------------------------------------------------------------------

-- | Request body for manually adding a torrent download
data AddTorrentDTO = AddTorrentDTO
  { torrentUrl :: Text
  -- ^ Magnet link (required)
  -- Example: "magnet:?xt=urn:btih:abc123..."
  , subscriptionId :: Int
  -- ^ Subscription ID to associate with (required, must exist)
  , episodeNumber :: Int
  -- ^ Episode number (required)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
