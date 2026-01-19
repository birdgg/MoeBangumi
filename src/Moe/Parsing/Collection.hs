{- | Collection/batch torrent structure parsing

This module handles parsing of torrent structures with multiple video files:

== Content Types

1. Single Episode - One video file (handled by existing logic)
2. Collection - Multiple episodes in one directory
   @[hyakuhuyu][Re Zero][51-66][BDRip 1080p AVC AAC][CHS]/EP51.mp4@
3. Multi-Season Collection - Multiple season subdirectories
   @[Group][Title]/[Group][Title][S1]/EP01.mp4, [Group][Title][S2]/EP01.mp4@

== Directory Structure Detection

Single Season: files directly under torrent root or single subdirectory
Multi-Season: multiple subdirectories with season indicators in names
-}
module Moe.Parsing.Collection
  ( -- * Content Type
    ContentType (..)

    -- * Parse Result
  , CollectionInfo (..)

    -- * Parsing Functions
  , detectContentType
  , parseCollectionInfo
  , extractEpisodeRange
  , extractSeasonFromPath
  , isSpecialDirectory
  , normalizeTitle
  )
where

import Data.Char (isDigit)
import Data.Containers.ListUtils (nubOrd)
import Data.Text qualified as T
import Text.Regex.TDFA ((=~))

import Moe.Parsing.Anime qualified as Anime
import Moe.Parsing.Types (ParseResult (..))

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Content type classification for torrents
data ContentType
  = -- | Single video file torrent (existing logic handles this)
    SingleEpisode
  | -- | Multiple episodes in one directory (single season collection)
    -- Example: [Group][Title][01-12]/EP01.mp4
    Collection
  | -- | Multiple season subdirectories
    -- Example: [Group][Title]/S1/EP01.mp4, S2/EP01.mp4
    MultiSeasonCollection
  deriving stock (Show, Eq)

-- | Parsed collection information
data CollectionInfo = CollectionInfo
  { contentType :: ContentType
  -- ^ Detected content type
  , title :: Maybe Text
  -- ^ Extracted title (Chinese preferred)
  , season :: Maybe Int
  -- ^ Season number (Nothing for multi-season, Just n for single season)
  , episodeRange :: Maybe (Int, Int)
  -- ^ Episode range if detected from directory name (e.g., [51-66])
  , parseResult :: Maybe ParseResult
  -- ^ Full parse result from directory name
  }
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------
-- Content Type Detection
--------------------------------------------------------------------------------

{- | Detect content type from file list

Strategy:
1. Count video files
2. Analyze directory structure depth
3. Check for season indicators in paths

== Examples

Single Episode:
@
["video.mkv"] -> SingleEpisode
@

Collection:
@
["EP01.mkv", "EP02.mkv", "EP03.mkv"] -> Collection
["Season 1/EP01.mkv", "Season 1/EP02.mkv"] -> Collection (same season dir)
@

Multi-Season Collection:
@
["S1/EP01.mkv", "S2/EP01.mkv"] -> MultiSeasonCollection
["[Group][Title][S1]/EP01.mkv", "[Group][Title][S2]/EP01.mkv"] -> MultiSeasonCollection
@
-}
detectContentType :: [Text] -> ContentType
detectContentType filePaths
  | length videoFiles <= 1 = SingleEpisode
  | hasMultipleSeasonDirs = MultiSeasonCollection
  | otherwise = Collection
 where
  videoFiles = filter isVideoPath filePaths

  -- Get first directory component from each path
  firstDirs = mapMaybe getFirstDirectory videoFiles

  -- Check if we have multiple distinct season directories
  hasMultipleSeasonDirs =
    let seasonDirs = filter hasSeasonIndicator (nubOrd firstDirs)
     in length seasonDirs > 1

  getFirstDirectory :: Text -> Maybe Text
  getFirstDirectory path =
    case T.splitOn "/" path of
      (dir : _ : _) -> Just dir  -- Has at least one directory level
      _ -> Nothing

  hasSeasonIndicator :: Text -> Bool
  hasSeasonIndicator dir =
    -- Check for S1, S2, Season 1, 第一季 patterns
    dir =~ ("S[0-9]+" :: Text)
      || dir =~ ("[Ss]eason[[:space:]]*[0-9]+" :: Text)
      || dir =~ ("第[一二三四五六七八九十0-9]+季" :: Text)

-- | Check if path is a video file
isVideoPath :: Text -> Bool
isVideoPath path =
  let lower = T.toLower path
   in any (`T.isSuffixOf` lower) videoExtensions
 where
  videoExtensions =
    [".mkv", ".mp4", ".avi", ".ts", ".m2ts", ".flv", ".wmv", ".webm", ".mov"]

--------------------------------------------------------------------------------
-- Collection Info Parsing
--------------------------------------------------------------------------------

{- | Parse collection information from torrent name and file paths

@
parseCollectionInfo "[hyakuhuyu][Re Zero][51-66][BDRip]" ["EP51.mkv", "EP52.mkv"]
=> CollectionInfo
     { contentType = Collection
     , title = Just "Re Zero"
     , season = Just 1
     , episodeRange = Just (51, 66)
     , parseResult = Just ParseResult {...}
     }
@
-}
parseCollectionInfo :: Text -> [Text] -> CollectionInfo
parseCollectionInfo torrentName filePaths =
  let contentType = detectContentType filePaths
      parsed = Anime.parseFilename torrentName
      episodeRng = extractEpisodeRange torrentName
      titleText = extractTitle parsed torrentName
      seasonNum = case contentType of
        MultiSeasonCollection -> Nothing  -- Each subdir has its own season
        _ -> extractSeasonFromParsed parsed
   in CollectionInfo
        { contentType = contentType
        , title = titleText
        , season = seasonNum
        , episodeRange = episodeRng
        , parseResult = parsed
        }
 where
  extractTitle :: Maybe ParseResult -> Text -> Maybe Text
  extractTitle (Just pr) _ =
    pr.nameChinese <|> pr.nameEnglish <|> pr.nameJapanese
  extractTitle Nothing name = Just $ normalizeTitle name

  extractSeasonFromParsed :: Maybe ParseResult -> Maybe Int
  extractSeasonFromParsed (Just pr) = pr.season <|> Just 1  -- Default to season 1
  extractSeasonFromParsed Nothing = Just 1

--------------------------------------------------------------------------------
-- Episode Range Extraction
--------------------------------------------------------------------------------

{- | Extract episode range from directory/torrent name

Matches patterns like:
- [01-12] -> (1, 12)
- [51-66] -> (51, 66)
- [01-24+SP] -> (1, 24)
- (01-12) -> (1, 12)

@
extractEpisodeRange "[hyakuhuyu][Re Zero][51-66][BDRip]"
=> Just (51, 66)
@
-}
extractEpisodeRange :: Text -> Maybe (Int, Int)
extractEpisodeRange name =
  let -- Pattern: [XX-YY] or (XX-YY)
      bracketPattern = "\\[([0-9]+)-([0-9]+)" :: Text
      parenPattern = "\\(([0-9]+)-([0-9]+)" :: Text
   in tryPattern bracketPattern name
        <|> tryPattern parenPattern name
 where
  tryPattern :: Text -> Text -> Maybe (Int, Int)
  tryPattern pattern text =
    let matches = text =~ pattern :: (Text, Text, Text)
     in case matches of
          (_, match, _)
            | not (T.null match) -> parseRange match
            | otherwise -> Nothing

  parseRange :: Text -> Maybe (Int, Int)
  parseRange match =
    let digits = T.filter (\c -> isDigit c || c == '-') match
        parts = T.splitOn "-" digits
     in case parts of
          [startStr, endStr] -> do
            start <- readMaybe (toString startStr)
            end <- readMaybe (toString endStr)
            guard (start <= end)
            pure (start, end)
          _ -> Nothing

--------------------------------------------------------------------------------
-- Season Extraction from Path
--------------------------------------------------------------------------------

{- | Extract season number from file path

Looks for season indicators in the path:
- /S1/ or /S01/ -> 1
- /Season 1/ -> 1
- /第一季/ -> 1
- /SP/ or /Special/ -> 0 (specials)

@
extractSeasonFromPath "[Group][Title][S2]/EP05.mkv"
=> Just 2

extractSeasonFromPath "[Group][Title]/SP/SP01.mkv"
=> Just 0
@
-}
extractSeasonFromPath :: Text -> Maybe Int
extractSeasonFromPath path
  -- Check for SP/Special directory first
  | isSpecialPath path = Just 0
  -- Try to extract season from path components
  | otherwise =
      let dirs = T.splitOn "/" path
       in listToMaybe $ mapMaybe extractSeasonFromDir dirs
 where
  extractSeasonFromDir :: Text -> Maybe Int
  extractSeasonFromDir dir =
    -- S1, S01, S2, etc.
    extractSPattern dir
      <|> extractSeasonPattern dir
      <|> extractChineseSeasonPattern dir

  extractSPattern :: Text -> Maybe Int
  extractSPattern dir =
    let pattern = "S([0-9]+)" :: Text
        matches = dir =~ pattern :: (Text, Text, Text)
     in case matches of
          (_, match, _)
            | not (T.null match) ->
                let digits = T.filter isDigit match
                 in readMaybe (T.unpack digits)
            | otherwise -> Nothing

  extractSeasonPattern :: Text -> Maybe Int
  extractSeasonPattern dir =
    let pattern = "[Ss]eason[[:space:]]*([0-9]+)" :: Text
        matches = dir =~ pattern :: (Text, Text, Text)
     in case matches of
          (_, match, _)
            | not (T.null match) ->
                let digits = T.filter isDigit match
                 in readMaybe (T.unpack digits)
            | otherwise -> Nothing

  extractChineseSeasonPattern :: Text -> Maybe Int
  extractChineseSeasonPattern dir = Anime.extractSeason dir

-- | Check if path contains SP/Special directory
isSpecialPath :: Text -> Bool
isSpecialPath path =
  let dirs = map T.toLower $ T.splitOn "/" path
   in any isSpecialDirectory dirs

--------------------------------------------------------------------------------
-- Special Directory Detection
--------------------------------------------------------------------------------

{- | Check if a directory name indicates special/extra content

Special directories should be renamed to S00 (Season 0 for specials).

@
isSpecialDirectory "SP" => True
isSpecialDirectory "Special" => True
isSpecialDirectory "OVA" => True
isSpecialDirectory "Extras" => True
isSpecialDirectory "Season 1" => False
@
-}
isSpecialDirectory :: Text -> Bool
isSpecialDirectory dir =
  let lower = T.toLower dir
   in lower `elem` specialDirs
        || lower =~ ("^sp[0-9]*$" :: Text)
        || lower =~ ("^ova[0-9]*$" :: Text)
        || lower =~ ("^oad[0-9]*$" :: Text)
 where
  specialDirs =
    [ "sp"
    , "sps"
    , "special"
    , "specials"
    , "ova"
    , "ovas"
    , "oad"
    , "oads"
    , "extra"
    , "extras"
    , "bonus"
    , "pv"
    , "pvs"
    , "cm"
    , "menu"
    , "nc"
    , "ncop"
    , "nced"
    ]

--------------------------------------------------------------------------------
-- Title Normalization
--------------------------------------------------------------------------------

{- | Normalize title by removing technical info

Removes common technical tags like resolution, codec, group names etc.

@
normalizeTitle "[hyakuhuyu][Re Zero][51-66][BDRip 1080p AVC AAC][CHS]"
=> "Re Zero"
@
-}
normalizeTitle :: Text -> Text
normalizeTitle name =
  let -- Remove bracketed content that looks like technical info
      withoutTech = removeTechnicalBrackets name
      -- Clean up extra spaces
      cleaned = T.strip $ T.unwords $ T.words withoutTech
   in if T.null cleaned then name else cleaned
 where
  removeTechnicalBrackets :: Text -> Text
  removeTechnicalBrackets t = foldr removeBracket t technicalPatterns

  removeBracket :: Text -> Text -> Text
  removeBracket pattern text =
    let regex = "\\[" <> pattern <> "\\]" :: Text
        matches = text =~ regex :: (Text, Text, Text)
     in case matches of
          (before, match, after)
            | not (T.null match) -> before <> after
            | otherwise -> text

  technicalPatterns :: [Text]
  technicalPatterns =
    [ "[0-9]+-[0-9]+"  -- Episode range like 51-66
    , "[0-9]+p"       -- Resolution like 1080p
    , "BDRip.*"       -- BDRip variants
    , "WEB-?DL.*"     -- WEB-DL variants
    , "AVC.*"         -- Video codec
    , "HEVC.*"
    , "x264.*"
    , "x265.*"
    , "AAC.*"         -- Audio codec
    , "FLAC.*"
    , "CHS"           -- Subtitle language
    , "CHT"
    , "GB"
    , "BIG5"
    ]
