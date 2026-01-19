{- | Torrent quality comparison algorithm

This module implements the washing (upgrade) comparison logic.
It compares two torrents and determines if the new one is better.

Comparison factors (in priority order):
1. Subtitle group priority (from user settings)
2. Subtitle language pattern priority (from user settings)
3. Video resolution (higher is better)
4. Video codec (newer is better: AV1 > H265 > H264)
5. Audio codec (better quality preferred)
6. BD-Rip source (preferred over web sources)
-}
module Moe.Washing.Compare (
  -- * Main Comparison
  compare,
  compareWithSettings,

  -- * Individual Comparisons
  compareGroup,
  compareLanguage,
  compareResolution,
  compareVideoCodec,
  compareAudioCodec,
  compareBDRip,

  -- * Scoring
  calculateScore,
)
where

import Data.List (elemIndex, findIndex)
import Data.Ord qualified as Ord
import Data.Text qualified as T
import Moe.Setting (PrioritySettings (..))
import Moe.Subtitle (Subtitle, SubtitlePattern, matchesPattern)
import Moe.Washing.Types
import Prelude hiding (compare, group)

{- | Compare two torrent qualities with default settings
Returns Better if new is better than current
-}
compare :: TorrentQuality -> TorrentQuality -> CompareResult
compare current new =
  let currentScore = calculateScore Nothing current
      newScore = calculateScore Nothing new
   in case Ord.compare newScore currentScore of
        GT -> Better
        LT -> Worse
        EQ -> Equal

-- | Compare two torrent qualities using user priority settings
compareWithSettings :: PrioritySettings -> TorrentQuality -> TorrentQuality -> CompareResult
compareWithSettings settings current new =
  -- First check group priority (highest weight)
  case compareGroup (groups settings) (group current) (group new) of
    Better -> Better
    Worse -> Worse
    Equal ->
      -- Then check language pattern priority
      case compareLanguage (languages settings) (subtitles current) (subtitles new) of
        Better -> Better
        Worse -> Worse
        Equal ->
          -- Finally compare other factors by score
          let currentScore = calculateScore (Just settings) current
              newScore = calculateScore (Just settings) new
           in case Ord.compare newScore currentScore of
                GT -> Better
                LT -> Worse
                EQ -> Equal

{- | Compare subtitle groups based on user priority list (case-insensitive)
Lower index = higher priority
-}
compareGroup :: [Text] -> Maybe Text -> Maybe Text -> CompareResult
compareGroup priorities currentGroup newGroup =
  let prioritiesLower = map T.toLower priorities
      currentIdx = currentGroup >>= \g -> elemIndex (T.toLower g) prioritiesLower
      newIdx = newGroup >>= \g -> elemIndex (T.toLower g) prioritiesLower
   in case (currentIdx, newIdx) of
        -- Both in priority list: lower index is better
        (Just ci, Just ni) -> case Ord.compare ni ci of
          LT -> Better -- new has lower index (higher priority)
          GT -> Worse
          EQ -> Equal
        -- Only new is in list: new is better
        (Nothing, Just _) -> Better
        -- Only current is in list: current is better
        (Just _, Nothing) -> Worse
        -- Neither in list: equal
        (Nothing, Nothing) -> Equal

{- | Compare subtitle language patterns based on user priority list
Lower index = higher priority
Exact match required: torrent subtitles must match pattern exactly
-}
compareLanguage :: [SubtitlePattern] -> [Subtitle] -> [Subtitle] -> CompareResult
compareLanguage priorities currentSubs newSubs =
  let currentIdx = findIndex (`matchesPattern` currentSubs) priorities
      newIdx = findIndex (`matchesPattern` newSubs) priorities
   in case (currentIdx, newIdx) of
        -- Both match a pattern: lower index is better
        (Just ci, Just ni) -> case Ord.compare ni ci of
          LT -> Better -- new has lower index (higher priority)
          GT -> Worse
          EQ -> Equal
        -- Only new matches a pattern: new is better
        (Nothing, Just _) -> Better
        -- Only current matches a pattern: current is better
        (Just _, Nothing) -> Worse
        -- Neither matches any pattern: equal
        (Nothing, Nothing) -> Equal

-- | Compare video resolutions
compareResolution :: Maybe Resolution -> Maybe Resolution -> CompareResult
compareResolution current new = compareMaybeOrd current new

-- | Compare video codecs
compareVideoCodec :: Maybe VideoCodec -> Maybe VideoCodec -> CompareResult
compareVideoCodec current new = compareMaybeOrd current new

-- | Compare audio codecs
compareAudioCodec :: Maybe AudioCodec -> Maybe AudioCodec -> CompareResult
compareAudioCodec current new = compareMaybeOrd current new

-- | Compare BD-Rip status (BD-Rip is preferred)
compareBDRip :: Bool -> Bool -> CompareResult
compareBDRip current new = case (current, new) of
  (False, True) -> Better
  (True, False) -> Worse
  _ -> Equal

-- | Helper to compare Maybe values where Just > Nothing and higher Ord > lower
compareMaybeOrd :: (Ord a) => Maybe a -> Maybe a -> CompareResult
compareMaybeOrd current new = case (current, new) of
  (Just c, Just n) -> case Ord.compare n c of
    GT -> Better
    LT -> Worse
    EQ -> Equal
  (Nothing, Just _) -> Better
  (Just _, Nothing) -> Worse
  (Nothing, Nothing) -> Equal

{- | Calculate a numeric score for a torrent quality
Higher score = better quality
This is used for overall comparison when group priorities are equal
-}
calculateScore :: Maybe PrioritySettings -> TorrentQuality -> Int
calculateScore _settings quality =
  resolutionScore + codecScore + audioScore + bdScore
 where
  resolutionScore = case resolution quality of
    Just R2160p -> 400
    Just R1080p -> 300
    Just R720p -> 200
    Just R480p -> 100
    Nothing -> 0

  codecScore = case videoCodec quality of
    Just AV1 -> 30
    Just H265 -> 20
    Just H264 -> 10
    Nothing -> 0

  audioScore = case audioCodec quality of
    Just TrueHD -> 4
    Just DTS -> 3
    Just FLAC -> 2
    Just AAC -> 1
    Nothing -> 0

  bdScore = if isBDRip quality then 50 else 0
