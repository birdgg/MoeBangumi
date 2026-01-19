-- | Anime filename parser using regex
--
-- This module parses anime torrent filenames to extract metadata.
--
-- Example usage:
--
-- @
-- import Moe.Parsing.Anime (parseFilename)
--
-- let filename = "[Lilith-Raws] Sousou no Frieren - 01 [1080p][CHT]"
-- case parseFilename filename of
--   Nothing -> putStrLn "Parse failed"
--   Just result -> print result.episode  -- Just 1
-- @
module Moe.Parsing.Anime
  ( -- * Main Parser
    parseFilename

    -- * Component Parsers (exported for testing)
  , extractGroup
  , extractEpisode
  , extractSeason
  , extractResolution
  , extractSubtitles
  , extractNames

    -- * Utility Functions
  , preprocess
  , removeSeason
  )
where

import Data.Char (isDigit)
import Data.List (nub)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Text.Regex.TDFA ((=~))

import Moe.Parsing.Types
import Moe.Subtitle (Subtitle (..))

--------------------------------------------------------------------------------
-- Main Entry Point
--------------------------------------------------------------------------------

-- | Parse anime filename to extract all metadata
--
-- Returns 'Nothing' if the filename doesn't match expected patterns.
-- Returns 'Just ParseResult' with extracted fields.
parseFilename :: Text -> Maybe ParseResult
parseFilename input = do
  let processed = preprocess input

  -- Extract subtitle group (first bracket)
  let grp = extractGroup processed

  -- Remove group from title for further processing
  let withoutGroup = case grp of
        Nothing -> processed
        Just g -> T.replace ("[" <> g <> "]") "" processed

  -- Extract episode and split title/metadata
  let (titlePart, ep, metaPart) = splitByEpisode withoutGroup

  -- Extract season from title part
  let seasonNum = extractSeason titlePart

  -- Extract names
  let (nameEn, nameZh, nameJp) = extractNames titlePart

  -- Extract resolution and subtitles from metadata
  let res = extractResolution metaPart
  let subs = extractSubtitles metaPart

  pure
    ParseResult
      { nameEnglish = nameEn
      , nameChinese = nameZh
      , nameJapanese = nameJp
      , episode = ep
      , season = seasonNum
      , subtitleGroup = grp
      , resolution = res
      , subtitles = subs
      }

--------------------------------------------------------------------------------
-- Preprocessing
--------------------------------------------------------------------------------

-- | Preprocess filename: normalize brackets, remove technical specs
preprocess :: Text -> Text
preprocess =
  removeTechnicalSpecs
    . T.replace "【" "["
    . T.replace "】" "]"
    . T.replace "～" "~"
 where
  removeTechnicalSpecs t =
    T.unwords $ filter (not . isTechnicalSpec) $ T.words t

  isTechnicalSpec word =
    let lower = T.toLower word
     in any (`T.isSuffixOf` lower) ["fps", "bit", "khz", "hz"]
          && T.any isDigit word

--------------------------------------------------------------------------------
-- Group Extraction
--------------------------------------------------------------------------------

-- | Extract subtitle group from first bracket
extractGroup :: Text -> Maybe Text
extractGroup title =
  case T.breakOn "[" title of
    (_, rest)
      | T.null rest -> Nothing
      | otherwise ->
          case T.breakOn "]" (T.drop 1 rest) of
            (groupName, _)
              | T.null groupName -> Nothing
              | otherwise -> Just (T.strip groupName)

--------------------------------------------------------------------------------
-- Episode Extraction
--------------------------------------------------------------------------------

-- | Episode patterns (order matters - more specific first)
episodePatterns :: [(Text, Text -> Maybe Int)]
episodePatterns =
  [ -- S01E01 format
    ("S[0-9]+E([0-9]+)", extractFirstDigits)
  , -- [13.END] or [13END] or [13完] format
    ("\\[([0-9]+)\\.?(END|完)\\]", extractFirstDigits)
  , -- [01] or [第01集] format
    ("\\[第?([0-9]+)[集话話]?\\]", extractFirstDigits)
  , -- (01) format
    ("\\(([0-9]+)\\)", extractFirstDigits)
  , -- EP01 or E01 format
    ("[Ee][Pp]?([0-9]+)", extractFirstDigits)
  , -- 第01话/集/話 format
    ("第([0-9]+)[话話集]", extractFirstDigits)
  , -- - 01 format (dash followed by number)
    (" - ([0-9]+)", extractFirstDigits)
  , -- Plain number at end after space
    (" ([0-9]{1,3})($|[^0-9pPkK])", extractFirstDigits)
  ]

extractFirstDigits :: Text -> Maybe Int
extractFirstDigits t =
  let digits = T.takeWhile isDigit $ T.dropWhile (not . isDigit) t
   in if T.null digits then Nothing else readMaybe (T.unpack digits)

-- | Split input into (title, episode, metadata)
splitByEpisode :: Text -> (Text, Maybe Int, Text)
splitByEpisode input =
  case findEpisodeMatch input of
    Nothing -> (input, Nothing, "")
    Just (beforeMatch, episodeNum, afterMatch) ->
      (T.strip beforeMatch, Just episodeNum, afterMatch)

findEpisodeMatch :: Text -> Maybe (Text, Int, Text)
findEpisodeMatch input = listToMaybe $ mapMaybe tryPattern episodePatterns
 where
  tryPattern (pattern, extractor) =
    let matches = input =~ pattern :: (Text, Text, Text)
     in case matches of
          (before, match, after)
            | not (T.null match) ->
                case extractor match of
                  Just ep -> Just (before, ep, after)
                  Nothing -> Nothing
            | otherwise -> Nothing

-- | Extract episode number from text
extractEpisode :: Text -> Maybe Int
extractEpisode input =
  case findEpisodeMatch input of
    Just (_, ep, _) -> Just ep
    Nothing -> Nothing

--------------------------------------------------------------------------------
-- Season Extraction
--------------------------------------------------------------------------------

-- | Extract season number from title
extractSeason :: Text -> Maybe Int
extractSeason title =
  -- Try S1/S01/Season 1 format first
  let sPattern = "S[Ee]?[Aa]?[Ss]?[Oo]?[Nn]?[[:space:]]*([0-9]+)" :: Text
      sMatches = title =~ sPattern :: (Text, Text, Text)
   in case sMatches of
        (_, match, _)
          | not (T.null match) -> extractFirstDigits match
          | otherwise ->
              -- Try 第X季/期 format
              let cnPattern = "第([一二三四五六七八九十0-9]+)[季期]" :: Text
                  cnMatches = title =~ cnPattern :: (Text, Text, Text)
               in case cnMatches of
                    (_, cnMatch, _)
                      | not (T.null cnMatch) -> parseSeasonNumber cnMatch
                      | otherwise -> Nothing

parseSeasonNumber :: Text -> Maybe Int
parseSeasonNumber t =
  let inner = T.filter (\c -> c /= '第' && c /= '季' && c /= '期') t
   in case readMaybe (T.unpack inner) of
        Just n -> Just n
        Nothing -> Map.lookup inner chineseNumberMap

--------------------------------------------------------------------------------
-- Resolution Extraction
--------------------------------------------------------------------------------

-- | Extract video resolution
extractResolution :: Text -> Maybe Text
extractResolution t
  | t =~ ("2160[pP]?" :: Text) || t =~ ("4[kK]" :: Text) = Just "2160P"
  | t =~ ("1080[pP]?" :: Text) = Just "1080P"
  | t =~ ("720[pP]?" :: Text) = Just "720P"
  | otherwise = Nothing

--------------------------------------------------------------------------------
-- Subtitle Language Extraction
--------------------------------------------------------------------------------

-- | Extract subtitle languages
extractSubtitles :: Text -> [Subtitle]
extractSubtitles t = nub $ sort $ concat
  [ [CHT | matchesCht]
  , [CHS | matchesChs]
  , [JAP | matchesJap]
  , [ENG | matchesEng]
  ]
 where
  -- Match like Rust: [简中]|CHS|SC|GB|GBK|GB2312
  matchesChs = t =~ ("简|中|CHS|SC|GB|GBK|GB2312" :: Text)
  -- Match like Rust: 繁|CHT|BIG5
  matchesCht = t =~ ("繁|CHT|BIG5" :: Text)
  -- Match like Rust: [日]|JP|JPSC
  matchesJap = t =~ ("日|JP|JPSC" :: Text)
  -- Match like Rust: ENG|英语|英文
  matchesEng = t =~ ("ENG|英语|英文" :: Text)

--------------------------------------------------------------------------------
-- Name Extraction
--------------------------------------------------------------------------------

-- | Extract English, Chinese, and Japanese names from title
extractNames :: Text -> (Maybe Text, Maybe Text, Maybe Text)
extractNames titleInfo =
  let
    -- Clean the title
    cleaned = cleanTitleText titleInfo

    -- Split by separators
    parts =
      filter (not . T.null)
        . map T.strip
        $ T.split (\c -> c == '/' || c == '_') cleaned

    -- Classify by language
    jpTitle = find isJapanese parts
    zhTitle = find isChinese parts
    enTitle = find isEnglish parts
   in
    (cleanName <$> enTitle, cleanName <$> zhTitle, cleanName <$> jpTitle)
 where
  cleanTitleText t =
    removeSeasonMarkers
      . T.replace "(仅限港澳台地区)" ""
      . T.replace "（仅限港澳台地区）" ""
      . T.replace "新番" ""
      . T.replace "月番" ""
      . T.replace "[" " "
      . T.replace "]" " "
      $ t

  removeSeasonMarkers t =
    foldr
      (\marker acc -> T.replace marker "" acc)
      t
      [ " S1", " S2", " S3", " S4", " S01", " S02", " S03", " S04"
      , "Season 1", "Season 2", "Season 3"
      , "第一季", "第二季", "第三季", "第四季"
      ]

  -- Clean name by replacing punctuation with spaces (like Rust's clean_name)
  cleanName = T.strip . collapseSpaces . T.map replacePunctuation
  collapseSpaces = T.unwords . T.words
  -- Replace non-word/non-CJK characters with space (like Rust PUNCTUATION_PATTERN)
  replacePunctuation c
    | isWordOrCJK c = c
    | otherwise = ' '
  isWordOrCJK c =
    (c >= 'a' && c <= 'z')
      || (c >= 'A' && c <= 'Z')
      || (c >= '0' && c <= '9')
      || c == '_'
      || (c >= '\x4e00' && c <= '\x9fff') -- CJK
      || (c >= '\x3040' && c <= '\x309f') -- Hiragana
      || (c >= '\x30a0' && c <= '\x30ff') -- Katakana

-- | Check if text contains Japanese characters (hiragana/katakana)
isJapanese :: Text -> Bool
isJapanese t =
  T.any
    (\c -> (c >= '\x3040' && c <= '\x309f') || (c >= '\x30a0' && c <= '\x30ff'))
    t

-- | Check if text contains Chinese characters (CJK ideographs)
isChinese :: Text -> Bool
isChinese t =
  T.any (\c -> c >= '\x4e00' && c <= '\x9fff') t
    && not (isJapanese t)

-- | Check if text is primarily English
isEnglish :: Text -> Bool
isEnglish t =
  let latinCount = T.length $ T.filter isLatinAlpha t
   in latinCount >= 3 && not (isChinese t) && not (isJapanese t)
 where
  isLatinAlpha c = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')

--------------------------------------------------------------------------------
-- Season Removal (for search)
--------------------------------------------------------------------------------

-- | Remove season information from title for search purposes
--
-- Returns (cleaned title, season number)
removeSeason :: Text -> (Text, Maybe Int)
removeSeason name =
  let -- Find all season matches (case insensitive S/Season + number, or Chinese format)
      seasonPattern = "[Ss][Ee][Aa][Ss][Oo][Nn][ ]*[0-9]+|[Ss][0-9]+|第.[季期]" :: Text
      matches = name =~ seasonPattern :: (Text, Text, Text)
   in case matches of
        (before, match, after)
          | not (T.null match) ->
              let cleaned = T.strip before <> " " <> T.strip after
                  seasonNum = parseSeasonFromMatch match
               in (cleanName cleaned, seasonNum)
          | otherwise ->
              (cleanName name, Nothing)
 where
  cleanName = T.strip . collapseSpaces
  collapseSpaces = T.unwords . T.words

  parseSeasonFromMatch :: Text -> Maybe Int
  parseSeasonFromMatch t =
    let upper = T.toUpper t
     in if "SEASON" `T.isInfixOf` upper || "S" `T.isPrefixOf` upper
          then extractFirstDigits t
          else parseSeasonNumber t
