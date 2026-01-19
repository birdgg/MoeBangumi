{- | Washing (torrent upgrade) types

This module defines types for torrent quality comparison:

- 'Resolution': Video resolution levels
- 'VideoCodec': Video encoding formats
- 'AudioCodec': Audio encoding formats
- 'TorrentQuality': Parsed quality information from torrent title
- 'CompareResult': Result of quality comparison
-}
module Moe.Washing.Types (
  -- * Resolution
  Resolution (..),

  -- * Video Codec
  VideoCodec (..),

  -- * Audio Codec
  AudioCodec (..),

  -- * Torrent Quality
  TorrentQuality (..),

  -- * Compare Result
  CompareResult (..),
)
where

import Data.Aeson (FromJSON, ToJSON)
import Moe.Subtitle (Subtitle)

--------------------------------------------------------------------------------
-- Resolution
--------------------------------------------------------------------------------

-- | Video resolution levels, ordered from lowest to highest
data Resolution
  = R480p
  | R720p
  | R1080p
  | R2160p -- 4K
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

--------------------------------------------------------------------------------
-- Video Codec
--------------------------------------------------------------------------------

-- | Video encoding formats, ordered by preference (newer/better = higher)
data VideoCodec
  = H264 -- AVC
  | H265 -- HEVC
  | AV1
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

--------------------------------------------------------------------------------
-- Audio Codec
--------------------------------------------------------------------------------

-- | Audio encoding formats
data AudioCodec
  = AAC
  | FLAC
  | DTS
  | TrueHD
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

--------------------------------------------------------------------------------
-- Torrent Quality
--------------------------------------------------------------------------------

-- | Parsed quality information from torrent title
data TorrentQuality = TorrentQuality
  { group :: Maybe Text
  -- ^ Subtitle group name (e.g., "Lilith-Raws", "ANi")
  , resolution :: Maybe Resolution
  -- ^ Video resolution
  , videoCodec :: Maybe VideoCodec
  -- ^ Video encoding format
  , audioCodec :: Maybe AudioCodec
  -- ^ Audio encoding format
  , subtitles :: [Subtitle]
  -- ^ Available subtitle languages
  , isBDRip :: Bool
  -- ^ Whether this is a Blu-ray rip (higher quality source)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

--------------------------------------------------------------------------------
-- Compare Result
--------------------------------------------------------------------------------

-- | Result of comparing two torrent qualities
data CompareResult
  = -- | New torrent is better, should upgrade
    Better
  | -- | New torrent is worse, keep current
    Worse
  | -- | Same quality, no upgrade needed
    Equal
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)
