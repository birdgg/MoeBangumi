{- | File rename job for completed torrents

Automatically renames single-video-file torrents based on torrent name.
Runs every 30 minutes, processing torrents tagged with "rename".

== Workflow

1. Query all torrents with "rename" tag
2. Filter to completed downloads (progress >= 1.0)
3. For each torrent with single video file:
   - Rename video to: {torrent_name}.{video_extension}
   - Rename subtitles to: {torrent_name}.{lang}.{subtitle_extension}
   - Remove "rename" tag on success
4. For each torrent with multiple video files (collection):
   - Search TMDB for metadata
   - Create Bangumi entry
   - Rename to S{season}E{episode} format
   - Remove "rename" tag on success

== Module Structure

- "App.Jobs.Rename.Job" - Job definition entry point
- "App.Jobs.Rename.Rename" - Core orchestration logic
- "App.Jobs.Rename.Utils" - Path manipulation utilities
- "App.Jobs.Rename.Single" - Single file rename operations
- "App.Jobs.Rename.Batch" - Batch rename for collections

== Usage

@
import App.Jobs.Rename (renameJob)

let jobs = [renameJob logger env]
runJobsConcurrently jobs
@
-}
module App.Jobs.Rename (
  -- * Job Definition
  renameJob,

  -- * Core Logic (exported for testing)
  renameAllTaggedTorrents,

  -- * Utilities (exported for testing)
  isVideoFile,
  isSubtitleFile,
  extractExtension,
  extractSubtitleSuffix,
  buildNewPath,
  buildEpisodePath,

  -- * Tags (re-exported from Downloader.Types)
  TorrentTag (..),
)
where

import App.Jobs.Rename.Job (renameJob)
import App.Jobs.Rename.Rename (
  buildEpisodePath,
  buildNewPath,
  extractExtension,
  extractSubtitleSuffix,
  isSubtitleFile,
  isVideoFile,
  renameAllTaggedTorrents,
 )
import Infra.Downloader.Types (TorrentTag (..))
