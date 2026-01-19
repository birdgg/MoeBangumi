module Moe.Video
  ( videoExtensions
  , isVideoExtension
  )
where

import Data.Text qualified as T

--------------------------------------------------------------------------------
-- Video Extensions
--------------------------------------------------------------------------------

-- | Supported video file extensions (lowercase, with dot)
videoExtensions :: [Text]
videoExtensions =
  [ ".mkv"
  , ".mp4"
  , ".avi"
  , ".ts"
  , ".m2ts"
  , ".flv"
  , ".wmv"
  , ".webm"
  , ".mov"
  ]

-- | Check if an extension is a video extension
isVideoExtension :: Text -> Bool
isVideoExtension ext = T.toLower ext `elem` videoExtensions
