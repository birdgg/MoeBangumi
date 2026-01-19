{- | Downloader abstraction types

This module provides downloader-agnostic types that can work with
any BitTorrent client implementation (qBittorrent, Transmission, Aria2, etc.).

= Type Overview

- 'TorrentSource': Where the torrent comes from (magnet link or file)
- 'AddTorrentOptions': Options when adding a torrent
- 'RenameOperation': A single file rename request
- 'DownloaderError': Errors that can occur during operations
-}
module Infra.Downloader.Types (
  -- * Torrent Source
  TorrentSource (..),

  -- * Add Options
  AddTorrentOptions (..),
  defaultAddOptions,

  -- * Rename
  RenameOperation (..),

  -- * Torrent Info (simplified, downloader-agnostic)
  SimpleTorrentInfo (..),
  SimpleTorrentFile (..),

  -- * Filters
  TorrentStatusFilter (..),

  -- * Errors
  DownloaderError (..),

  -- * Tags
  TorrentTag (..),
)
where

import Data.Text.Display (Display (..))

--------------------------------------------------------------------------------
-- Torrent Source
--------------------------------------------------------------------------------

-- | Source of a torrent to add
data TorrentSource
  = -- | Magnet link (magnet:?xt=urn:btih:...)
    MagnetLink Text
  | -- | Raw .torrent file content
    TorrentFileContent ByteString
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------
-- Add Torrent Options
--------------------------------------------------------------------------------

-- | Options for adding a torrent
data AddTorrentOptions = AddTorrentOptions
  { savePath :: Maybe Text
  -- ^ Custom save path (uses downloader default if Nothing)
  , category :: Maybe Text
  -- ^ Category/label for organization
  , tags :: [Text]
  -- ^ Tags for filtering
  , rename :: Maybe Text
  -- ^ Rename the torrent
  }
  deriving stock (Show, Eq, Generic)

-- | Default options with all fields empty/Nothing
defaultAddOptions :: AddTorrentOptions
defaultAddOptions =
  AddTorrentOptions
    { savePath = Nothing
    , category = Nothing
    , tags = []
    , rename = Nothing
    }

--------------------------------------------------------------------------------
-- Rename Operations
--------------------------------------------------------------------------------

-- | A single file rename operation within a torrent
data RenameOperation = RenameOperation
  { torrentHash :: Text
  -- ^ Hash of the torrent containing the file
  , oldPath :: Text
  -- ^ Current file path within the torrent
  , newPath :: Text
  -- ^ New file path
  }
  deriving stock (Show, Eq, Generic)

--------------------------------------------------------------------------------
-- Simplified Torrent Info (downloader-agnostic)
--------------------------------------------------------------------------------

{- | Simplified torrent information

This is a downloader-agnostic view of torrent status.
Specific implementations map their native types to this.
-}
data SimpleTorrentInfo = SimpleTorrentInfo
  { hash :: Text
  , name :: Text
  , state :: Text
  , progress :: Double
  -- ^ Download progress (0.0 to 1.0)
  , savePath :: Text
  , tags :: [Text]
  }
  deriving stock (Show, Eq, Generic)

-- | Simplified file information within a torrent
data SimpleTorrentFile = SimpleTorrentFile
  { index :: Int
  , name :: Text
  , size :: Int64
  , progress :: Double
  -- ^ Download progress (0.0 to 1.0)
  }
  deriving stock (Show, Eq, Generic)

--------------------------------------------------------------------------------
-- Filters
--------------------------------------------------------------------------------

-- | Generic torrent status filter
data TorrentStatusFilter
  = FilterAll
  | FilterDownloading
  | FilterCompleted
  | FilterPaused
  deriving stock (Show, Eq, Generic)

--------------------------------------------------------------------------------
-- Errors
--------------------------------------------------------------------------------

-- | Errors that can occur during downloader operations
data DownloaderError
  = -- | Network/connection error
    ConnectionError Text
  | -- | Authentication failed
    AuthenticationError Text
  | -- | Torrent not found
    TorrentNotFound Text
  | -- | Invalid input (magnet link, file path, etc.)
    InvalidInput Text
  | -- | Underlying downloader client error
    ClientError Text
  | -- | Torrent already exists
    DuplicateTorrent Text
  deriving stock (Show, Eq)

-- | Display instance for errors (required by project guidelines)
instance Display DownloaderError where
  displayBuilder (ConnectionError msg) =
    "Downloader connection error: " <> displayBuilder msg
  displayBuilder (AuthenticationError msg) =
    "Downloader authentication failed: " <> displayBuilder msg
  displayBuilder (TorrentNotFound hash) =
    "Torrent not found: " <> displayBuilder hash
  displayBuilder (InvalidInput msg) =
    "Invalid input: " <> displayBuilder msg
  displayBuilder (ClientError msg) =
    "Downloader client error: " <> displayBuilder msg
  displayBuilder (DuplicateTorrent hash) =
    "Torrent already exists: " <> displayBuilder hash

data TorrentTag
  = MoeTag
  | RenameTag
  deriving stock (Show, Eq)

instance Display TorrentTag where
  displayBuilder MoeTag = "moe"
  displayBuilder RenameTag = "rename"