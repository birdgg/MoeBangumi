{- | Torrent title parser

This module parses quality information from torrent titles.
Typical torrent titles look like:

@
[Lilith-Raws] Sousou no Frieren - 01 [Baha][WEB-DL][1080p][AVC AAC][CHT][MP4]
[ANi] 葬送的芙莉蓮 - 01 [1080p][Baha][WEB-DL][AAC AVC][CHT].mp4
[桜都字幕组] 葬送のフリーレン / Sousou no Frieren [01][1080p][简体内嵌]
@
-}
module Moe.Washing.Parser (
  -- * Main Parser
  parseTitle,

  -- * Individual Parsers
  parseGroup,
  parseResolution,
  parseVideoCodec,
  parseAudioCodec,
  parseSubtitles,
  parseIsBDRip,
)
where

import Data.Text qualified as T
import Moe.Subtitle
import Moe.Washing.Types

-- | Parse quality information from a torrent title
parseTitle :: Text -> TorrentQuality
parseTitle title =
  TorrentQuality
    { group = parseGroup title
    , resolution = parseResolution title
    , videoCodec = parseVideoCodec title
    , audioCodec = parseAudioCodec title
    , subtitles = parseSubtitles title
    , isBDRip = parseIsBDRip title
    }

{- | Parse subtitle group name from title
Extracts the first bracketed text, e.g., "[Lilith-Raws]" -> "Lilith-Raws"
-}
parseGroup :: Text -> Maybe Text
parseGroup title =
  case T.breakOn "[" title of
    (_, rest)
      | T.null rest -> Nothing
      | otherwise ->
          case T.breakOn "]" (T.drop 1 rest) of
            (groupName, _)
              | T.null groupName -> Nothing
              | otherwise -> Just groupName

-- | Parse video resolution from title
parseResolution :: Text -> Maybe Resolution
parseResolution title
  | containsAny ["2160p", "4K", "4k", "UHD"] = Just R2160p
  | containsAny ["1080p", "1080P", "FHD"] = Just R1080p
  | containsAny ["720p", "720P", "HD"] = Just R720p
  | containsAny ["480p", "480P", "SD"] = Just R480p
  | otherwise = Nothing
 where
  titleLower = T.toLower title
  containsAny = any (`T.isInfixOf` titleLower) . map T.toLower

-- | Parse video codec from title
parseVideoCodec :: Text -> Maybe VideoCodec
parseVideoCodec title
  | containsAny ["AV1", "av1"] = Just AV1
  | containsAny ["HEVC", "H.265", "H265", "x265"] = Just H265
  | containsAny ["AVC", "H.264", "H264", "x264"] = Just H264
  | otherwise = Nothing
 where
  containsAny = any (`T.isInfixOf` title)

-- | Parse audio codec from title
parseAudioCodec :: Text -> Maybe AudioCodec
parseAudioCodec title
  | containsAny ["TrueHD", "truehd"] = Just TrueHD
  | containsAny ["DTS", "dts"] = Just DTS
  | containsAny ["FLAC", "flac"] = Just FLAC
  | containsAny ["AAC", "aac"] = Just AAC
  | otherwise = Nothing
 where
  containsAny = any (`T.isInfixOf` title)

-- | Parse subtitle languages from title
parseSubtitles :: Text -> [Subtitle]
parseSubtitles title =
  concat
    [ [CHT | containsAny chtPatterns]
    , [CHS | containsAny chsPatterns]
    , [JAP | containsAny japPatterns]
    ]
 where
  containsAny = any (`T.isInfixOf` title)
  chtPatterns = ["CHT", "繁體", "繁体", "BIG5", "正體"]
  chsPatterns = ["CHS", "简体", "簡體", "GB", "简中", "簡中"]
  japPatterns = ["JAP", "日語", "日语", "Japanese"]

-- | Check if the torrent is a Blu-ray rip
parseIsBDRip :: Text -> Bool
parseIsBDRip title = any (`T.isInfixOf` title) patterns
 where
  patterns = ["BDRip", "BD-Rip", "Blu-ray", "BluRay", "BDMV", "BD"]
