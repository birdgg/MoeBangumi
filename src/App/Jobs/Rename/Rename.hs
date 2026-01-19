{- | Core rename logic for torrent files

This module handles the orchestration of file renaming:
1. Query torrents with "rename" tag
2. Filter to completed downloads
3. For single-video torrents, delegate to Single module
4. For multi-video torrents (collections), delegate to Batch module

== Module Structure

- "App.Jobs.Rename.Utils" - Path manipulation utilities
- "App.Jobs.Rename.Single" - Single file rename operations
- "App.Jobs.Rename.Batch" - Batch rename for collections
-}
module App.Jobs.Rename.Rename (
  -- * Main Entry Point
  renameAllTaggedTorrents,

  -- * Re-exports for convenience
  module App.Jobs.Rename.Utils,

  -- * Re-exports from Moe.Subtitle
  extractSubtitleSuffix,

  -- * Re-exports from Moe.Video
)
where

import Data.Aeson (object, (.=))
import Data.Text.Display (display)
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Log (Log)
import Effectful.Log qualified as Log
import Effectful.Reader.Static qualified as Reader

import App.Jobs.Rename.Batch (performBatchRename)
import App.Jobs.Rename.Single (performRename)
import App.Jobs.Rename.Utils
import Infra.Database.Effect (DB)
import Infra.Downloader.Effect
import Infra.Environment.Env (MoeEnv)
import Infra.Notification.Effect (Notification)
import Moe.Subtitle (extractSubtitleSuffix)

--------------------------------------------------------------------------------
-- Main Logic
--------------------------------------------------------------------------------

{- | Process all torrents with "rename" tag

Queries all torrents, filters by tag and completion status,
then renames eligible torrents (single-video or collections).
-}
renameAllTaggedTorrents ::
  ( Downloader :> es
  , Notification :> es
  , Log :> es
  , IOE :> es
  , Reader.Reader MoeEnv :> es
  , DB :> es
  , Concurrent :> es
  ) =>
  Eff es ()
renameAllTaggedTorrents = do
  Log.logInfo "Starting file rename job" (object [])

  -- Get all torrents
  result <- getTorrents Nothing

  case result of
    Left err ->
      Log.logAttention "Failed to get torrents" $
        object ["error" .= display err]
    Right allTorrents -> do
      -- Filter torrents with "rename" tag and completed
      let taggedTorrents = filter hasRenameTag allTorrents
          completedTorrents = filter isCompleted taggedTorrents

      Log.logInfo "Found torrents for renaming" $
        object
          [ "tagged_count" .= length taggedTorrents
          , "completed_count" .= length completedTorrents
          ]

      -- Process each completed torrent
      forM_ completedTorrents renameSingleTorrent

--------------------------------------------------------------------------------
-- Filtering
--------------------------------------------------------------------------------

-- | Check if torrent has "rename" tag
hasRenameTag :: SimpleTorrentInfo -> Bool
hasRenameTag torrent = display RenameTag `elem` torrent.tags

-- | Check if torrent is completed (progress >= 1.0)
isCompleted :: SimpleTorrentInfo -> Bool
isCompleted torrent = torrent.progress >= 1.0

--------------------------------------------------------------------------------
-- Single Torrent Processing
--------------------------------------------------------------------------------

{- | Rename files in a single torrent

Strategy:
1. Get file list
2. Filter to video files only
3. If exactly one video file -> rename it (single episode mode)
4. If multiple video files -> batch rename (collection mode)
5. Remove "rename" tag on success
-}
renameSingleTorrent ::
  ( Downloader :> es
  , Notification :> es
  , Log :> es
  , IOE :> es
  , Reader.Reader MoeEnv :> es
  , DB :> es
  , Concurrent :> es
  ) =>
  SimpleTorrentInfo ->
  Eff es ()
renameSingleTorrent torrent = do
  Log.logTrace "Processing torrent for rename" $
    object
      [ "hash" .= torrent.hash
      , "name" .= torrent.name
      ]

  -- Get files in torrent
  filesResult <- getTorrentFiles torrent.hash

  case filesResult of
    Left err ->
      Log.logAttention "Failed to get torrent files" $
        object
          [ "hash" .= torrent.hash
          , "error" .= display err
          ]
    Right files -> do
      -- Filter to video files only
      let videoFiles = filter isVideoFile files

      case videoFiles of
        [] ->
          Log.logTrace "No video files found, skipping" $
            object ["hash" .= torrent.hash]
        [singleVideo] ->
          -- Single video file - proceed with rename (pass all files for subtitle renaming)
          performRename torrent singleVideo files
        multipleVideos -> do
          -- Multiple video files - batch rename (collection mode)
          Log.logInfo "Multiple video files found, attempting batch rename" $
            object
              [ "hash" .= torrent.hash
              , "count" .= length multipleVideos
              ]
          performBatchRename torrent multipleVideos files
