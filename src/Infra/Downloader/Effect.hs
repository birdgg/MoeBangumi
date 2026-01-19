{-# LANGUAGE TemplateHaskell #-}

{- | Downloader Effect - Abstract interface for torrent downloaders

This module provides a downloader-agnostic effectful interface.
Authentication is handled transparently by the implementation.

= Supported Operations

- Add torrents via magnet links or .torrent files
- Batch rename files within torrents
- Query torrent status
- Pause/resume/delete torrents

= Authentication

The effect handlers manage authentication transparently:

- Auto-login on first operation
- Session persistence via cookies
- Auto-retry on auth failure

Callers never need to worry about login/logout.

= Usage

@
import Infra.Downloader.Effect

addAnime :: (Downloader :> es) => Text -> Eff es ()
addAnime magnetLink = do
  -- No login needed - handled automatically
  result <- addTorrent (MagnetLink magnetLink) defaultAddOptions
  case result of
    Right () -> liftIO $ putStrLn "Added successfully"
    Left err -> liftIO $ print (display err)
@
-}
module Infra.Downloader.Effect
  ( -- * Effect
    Downloader (..)

    -- * Operations
  , addTorrent
  , getTorrents
  , getTorrentFiles
  , batchRenameFiles
  , pauseTorrents
  , resumeTorrents
  , deleteTorrents
  , removeTags

    -- * Re-exports
  , module Infra.Downloader.Types
  )
where

import Effectful
import Effectful.TH
import Infra.Downloader.Types

-- | Downloader effect for torrent management
--
-- All operations return @Either DownloaderError a@ for consistent error handling.
-- Authentication is managed transparently by the handler.
data Downloader :: Effect where
  -- | Add a torrent from magnet link or .torrent file
  AddTorrent
    :: TorrentSource
    -> AddTorrentOptions
    -> Downloader m (Either DownloaderError ())
  -- | Get list of torrents with optional filter
  GetTorrents
    :: Maybe TorrentStatusFilter
    -> Downloader m (Either DownloaderError [SimpleTorrentInfo])
  -- | Get files within a specific torrent
  GetTorrentFiles
    :: Text
    -- ^ Torrent hash
    -> Downloader m (Either DownloaderError [SimpleTorrentFile])
  -- | Batch rename multiple files
  BatchRenameFiles
    :: [RenameOperation]
    -> Downloader m (Either DownloaderError ())
  -- | Pause torrents by hash
  PauseTorrents
    :: [Text]
    -- ^ Torrent hashes
    -> Downloader m (Either DownloaderError ())
  -- | Resume paused torrents
  ResumeTorrents
    :: [Text]
    -- ^ Torrent hashes
    -> Downloader m (Either DownloaderError ())
  -- | Delete torrents
  DeleteTorrents
    :: [Text]
    -- ^ Torrent hashes
    -> Bool
    -- ^ Delete files from disk?
    -> Downloader m (Either DownloaderError ())
  -- | Remove tags from torrents
  RemoveTags
    :: [Text]
    -- ^ Torrent hashes
    -> [Text]
    -- ^ Tags to remove
    -> Downloader m (Either DownloaderError ())

type instance DispatchOf Downloader = Dynamic

makeEffect ''Downloader
