{- | Torrent quality selection via washing comparison

This module implements the washing (upgrade) comparison logic for RSS items.
It determines which torrents to download by comparing new items against
existing torrents in the database.

== Washing Decision Logic

1. Query all existing torrents for this subscription (batch query)
2. Build a Map from episode -> TorrentQuality (using group and languages)
3. For each RSS item with episode number:
   - If no existing torrent for this episode: candidate for download
   - If existing torrent: compare quality
     - Better: candidate for download
     - Worse/Equal: skip
4. For each episode, only keep the single best item to download
5. Debug log the selected item titles

== Key Design

Uses existing Torrent.group and Torrent.languages fields for quality comparison.
This enables proper group priority and language pattern matching without needing
to store the full original title.
-}
module App.Jobs.RssFetch.Washing
  ( -- * Main Selection
    selectBestByWashing
  )
where

import Data.Aeson (object, (.=))
import Data.Map.Strict qualified as Map
import Effectful
import Effectful.Log (Log)
import Effectful.Log qualified as Log

import Infra.Database.Effect (DB)
import Infra.Database.Repository.Torrent qualified as TorrentRepo
import Infra.External.Rss.Parser (ParsedRssItem (..))
import Moe.Parsing.Types (ParseResult (..))
import Moe.Setting (PrioritySettings)
import Moe.Torrent (Torrent (..))
import Moe.Washing.Compare qualified as Washing
import Moe.Washing.Parser qualified as WashingParser
import Moe.Washing.Types (CompareResult (..), TorrentQuality (..))

--------------------------------------------------------------------------------
-- Washing (Torrent Quality Selection)
--------------------------------------------------------------------------------

-- | State for tracking the best item per episode during washing
data EpisodeBest = EpisodeBest
  { item :: ParsedRssItem
  , quality :: TorrentQuality
  }

{- | Select the best items to download using washing comparison

Algorithm:
1. Batch query all torrents for this subscription
2. Build Map Episode TorrentQuality from existing torrents
3. Scan RSS items, keeping only the best per episode
4. Debug log selected titles

Returns at most one item per episode - the highest quality one.
Items without episode numbers are skipped.
-}
selectBestByWashing ::
  (DB :> es, Log :> es) =>
  Int ->
  -- ^ Subscription ID
  PrioritySettings ->
  -- ^ Priority settings for washing comparison
  [ParsedRssItem] ->
  Eff es [ParsedRssItem]
selectBestByWashing subscriptionId priorities items = do
  -- Step 1: Batch query all existing torrents for this subscription
  existingTorrents <- TorrentRepo.findBySubscriptionId subscriptionId

  -- Step 2: Build Map Episode TorrentQuality from existing torrents
  let existingMap = buildExistingMap existingTorrents

  -- Step 3: Scan RSS items and select best per episode
  let bestMap = selectBestItems priorities existingMap items

  -- Step 4: Extract selected items
  let selectedItems = map (.item) (Map.elems bestMap)

  -- Step 5: Debug log selected titles
  logSelectedItems subscriptionId selectedItems

  pure selectedItems

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

{- | Build a Map from episode number to TorrentQuality from existing torrents

For each torrent with an episode number, constructs a TorrentQuality
using the stored group and languages fields.

When multiple torrents exist for the same episode (shouldn't happen normally),
uses the newest one (first in list since sorted by created_at DESC).
-}
buildExistingMap :: [Torrent] -> Map.Map Int TorrentQuality
buildExistingMap torrents =
  Map.fromList
    [ (ep, torrentToQuality torrent)
    | torrent <- torrents
    , Just ep <- [torrent.episodeNumber]
    ]
 where
  torrentToQuality :: Torrent -> TorrentQuality
  torrentToQuality t =
    TorrentQuality
      { group = t.group
      , resolution = Nothing
      , videoCodec = Nothing
      , audioCodec = Nothing
      , subtitles = t.languages
      , isBDRip = False
      }

{- | Scan RSS items and select the best item for each episode

For each RSS item with an episode:
1. Compare with existing torrent quality (if any)
2. Compare with current best candidate (if any)
3. Keep only if better than both

Returns a Map from episode to the best item to download.
-}
selectBestItems ::
  PrioritySettings ->
  Map.Map Int TorrentQuality ->
  [ParsedRssItem] ->
  Map.Map Int EpisodeBest
selectBestItems priorities existingMap =
  foldl' (processItem priorities existingMap) Map.empty

{- | Process a single RSS item and update the best map if needed
-}
processItem ::
  PrioritySettings ->
  Map.Map Int TorrentQuality ->
  Map.Map Int EpisodeBest ->
  ParsedRssItem ->
  Map.Map Int EpisodeBest
processItem priorities existingMap bestMap item =
  case item.parseResult.episode of
    Nothing -> bestMap -- Skip items without episode
    Just ep ->
      let newQuality = WashingParser.parseTitle item.originalTitle
          existingQuality = Map.lookup ep existingMap
          currentBest = Map.lookup ep bestMap
       in if shouldInclude priorities existingQuality currentBest newQuality
            then Map.insert ep (EpisodeBest item newQuality) bestMap
            else bestMap

{- | Determine if a new item should be included in the download list

A new item should be included if:
1. No existing torrent AND no current candidate, OR
2. Better than existing torrent AND (no current candidate OR better than current candidate)
-}
shouldInclude ::
  PrioritySettings ->
  Maybe TorrentQuality ->
  -- ^ Existing torrent quality (from DB)
  Maybe EpisodeBest ->
  -- ^ Current best candidate
  TorrentQuality ->
  -- ^ New item quality
  Bool
shouldInclude priorities existingQuality currentBest newQuality =
  let -- Check if better than existing (or no existing)
      betterThanExisting = case existingQuality of
        Nothing -> True
        Just existing -> Washing.compareWithSettings priorities existing newQuality == Better

      -- Check if better than current candidate (or no candidate)
      betterThanCurrent = case currentBest of
        Nothing -> True
        Just best -> Washing.compareWithSettings priorities best.quality newQuality == Better
   in betterThanExisting && betterThanCurrent

{- | Log the selected items at trace level
-}
logSelectedItems :: (Log :> es) => Int -> [ParsedRssItem] -> Eff es ()
logSelectedItems subscriptionId items = do
  forM_ items $ \item ->
    Log.logTrace "Washing selected item" $
      object
        [ "subscription_id" .= subscriptionId
        , "episode" .= item.parseResult.episode
        , "title" .= item.originalTitle
        , "info_hash" .= item.infoHash
        , "group" .= item.parseResult.subtitleGroup
        ]
