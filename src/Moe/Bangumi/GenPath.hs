-- | Generate Plex-compatible paths for Bangumi
--
-- This module generates folder paths following Plex naming conventions:
--
-- == TV Shows / OVA
-- @
-- {Title} ({Year}) {tmdb-xxxxx}/Season 01/
-- @
--
-- == Movies
-- @
-- {Title} ({Year}) {tmdb-xxxxx}/
-- @
--
-- See: https://support.plex.tv/articles/naming-and-organizing-your-tv-show-files/
-- See: https://support.plex.tv/articles/naming-and-organizing-your-movie-media-files/
module Moe.Bangumi.GenPath
  ( genPath
  , genPathFrom
  , genShowFolder
  , genSeasonFolder
  , genEpisodeFilename
  , extractYear
  )
where

import Data.Text qualified as T
import Text.Printf (printf)

import Moe.Bangumi (Bangumi (..), Platform (..), getYear)

-- | Generate complete Plex-compatible path for a Bangumi
--
-- For TV/OVA: @{Title} ({Year}) {tmdb-xxxxx}/Season XX/@
-- For Movie:  @{Title} ({Year}) {tmdb-xxxxx}/@
genPath :: Bangumi -> Text
genPath bangumi =
  genPathFrom
    bangumi.titleChinese
    (getYear bangumi)
    bangumi.tmdbId
    bangumi.platform
    bangumi.season

-- | Generate Plex-compatible path from basic info
--
-- This is useful when creating a Bangumi and you don't have the full entity yet.
--
-- For TV/OVA: @{Title} ({Year}) {tmdb-xxxxx}/Season XX/@
-- For Movie:  @{Title} ({Year}) {tmdb-xxxxx}/@
genPathFrom
  :: Text
  -- ^ Title
  -> Maybe Text
  -- ^ Year (extracted from airDate)
  -> Maybe Int
  -- ^ TMDB ID
  -> Platform
  -- ^ Platform (TV, Movie, OVA)
  -> Int
  -- ^ Season number
  -> Text
genPathFrom title year tmdbId platform seasonNum =
  case platform of
    Movie -> showFolder
    _ -> showFolder <> genSeasonFolder seasonNum
 where
  showFolder = sanitizePath title <> yearPart <> tmdbPart <> "/"

  yearPart = case year of
    Just y -> " (" <> y <> ")"
    Nothing -> ""

  tmdbPart = case tmdbId of
    Just tmdb -> " {tmdb-" <> T.pack (show tmdb) <> "}"
    Nothing -> ""

-- | Generate the show/movie folder name
--
-- Format: @{Title} ({Year}) {tmdb-xxxxx}/@
genShowFolder :: Bangumi -> Text
genShowFolder bangumi =
  sanitizePath bangumi.titleChinese <> yearPart <> tmdbPart <> "/"
 where
  yearPart = case getYear bangumi of
    Just year -> " (" <> year <> ")"
    Nothing -> ""

  tmdbPart = case bangumi.tmdbId of
    Just tmdb -> " {tmdb-" <> T.pack (show tmdb) <> "}"
    Nothing -> ""

-- | Generate the season folder name
--
-- Format: @Season XX/@
--
-- Season number is zero-padded to 2 digits.
genSeasonFolder :: Int -> Text
genSeasonFolder seasonNum =
  "Season " <> T.pack (printf "%02d" seasonNum) <> "/"

-- | Generate episode filename prefix
--
-- If episode < offset: @S00E{episode}@ (specials)
-- Else: @S{season}E{episode - offset}@
genEpisodeFilename
  :: Int
  -- ^ Season number
  -> Int
  -- ^ Episode offset
  -> Int
  -- ^ Episode number
  -> Text
genEpisodeFilename seasonNum offset episode
  | episode < offset = T.pack $ printf "S00E%02d" episode
  | otherwise = T.pack $ printf "S%02dE%02d" seasonNum (episode - offset)

--------------------------------------------------------------------------------
-- Internal
--------------------------------------------------------------------------------

-- | Extract year from airDate (YYYY-MM-DD format)
extractYear :: Maybe Text -> Maybe Text
extractYear Nothing = Nothing
extractYear (Just airDate) =
  case T.splitOn "-" airDate of
    (year : _) | T.length year == 4 -> Just year
    _ -> Nothing

-- | Sanitize path by removing/replacing invalid characters
--
-- Removes: / \ : * ? " < > |
sanitizePath :: Text -> Text
sanitizePath =
  T.replace "/" "_"
    . T.replace "\\" "_"
    . T.replace ":" "_"
    . T.replace "*" "_"
    . T.replace "?" "_"
    . T.replace "\"" "_"
    . T.replace "<" "_"
    . T.replace ">" "_"
    . T.replace "|" "_"
