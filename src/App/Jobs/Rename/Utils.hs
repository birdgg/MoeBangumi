{- | Utility functions for file renaming

This module provides helper functions for path manipulation,
extension extraction, and file type detection.
-}
module App.Jobs.Rename.Utils
  ( -- * File Type Detection
    isVideoFile
  , isSubtitleFile

    -- * Path Manipulation
  , extractExtension
  , buildNewPath
  , buildEpisodePath
  , getFileName
  , getDirectory
  )
where

import Data.Text qualified as T
import Text.Printf (printf)

import Infra.Downloader.Effect (SimpleTorrentFile (..))
import Moe.Subtitle (isSubtitleExtension)
import Moe.Video (isVideoExtension)

--------------------------------------------------------------------------------
-- File Type Detection
--------------------------------------------------------------------------------

-- | Check if a file is a video file based on extension
isVideoFile :: SimpleTorrentFile -> Bool
isVideoFile file =
  case extractExtension file.name of
    Nothing -> False
    Just ext -> isVideoExtension ext

-- | Check if a file is a subtitle file based on extension
isSubtitleFile :: SimpleTorrentFile -> Bool
isSubtitleFile file =
  case extractExtension file.name of
    Nothing -> False
    Just ext -> isSubtitleExtension ext

--------------------------------------------------------------------------------
-- Path Manipulation
--------------------------------------------------------------------------------

{- | Extract file extension from filename

Examples:
- "video.mkv" -> Just ".mkv"
- "video.en.srt" -> Just ".srt"
- "noextension" -> Nothing
-}
extractExtension :: Text -> Maybe Text
extractExtension filename =
  case T.breakOnEnd "." filename of
    ("", _) -> Nothing
    (_, ext) | T.null ext -> Nothing
    (_, ext) -> Just $ "." <> ext

{- | Build new path preserving directory structure

If the original file is in a subdirectory, keep it there.
Only replace the filename, not the directory.

Examples:
- ("video.mkv", "New Name", ".mkv") -> "New Name.mkv"
- ("Season 1/video.mkv", "New Name", ".mkv") -> "Season 1/New Name.mkv"
- ("Sub/Dir/video.mkv", "New Name", ".mkv") -> "Sub/Dir/New Name.mkv"
-}
buildNewPath :: Text -> Text -> Text -> Text
buildNewPath oldPath newBaseName ext =
  case T.breakOnEnd "/" oldPath of
    ("", _) -> newBaseName <> ext  -- No directory, just filename
    (dir, _) -> dir <> newBaseName <> ext  -- Preserve directory path

{- | Build episode path with standard naming convention

Format: "{Title} S{season}E{episode}"

Examples:
- buildEpisodePath "葬送的芙莉莲" 1 5 -> "葬送的芙莉莲 S01E05"
- buildEpisodePath "Re Zero" 2 51 -> "Re Zero S02E51"
- buildEpisodePath "Title" 0 1 -> "Title S00E01" (specials)
-}
buildEpisodePath :: Text -> Int -> Int -> Text
buildEpisodePath title seasonNum episodeNum =
  title <> " " <> T.pack (printf "S%02dE%02d" seasonNum episodeNum)

-- | Extract just the filename from a path
getFileName :: Text -> Text
getFileName path =
  case T.breakOnEnd "/" path of
    ("", name) -> name
    (_, name) -> name

-- | Extract directory from a path (without trailing /)
getDirectory :: Text -> Text
getDirectory path =
  case T.breakOnEnd "/" path of
    ("", _) -> ""
    (dir, _) -> T.dropEnd 1 dir  -- Remove trailing /
