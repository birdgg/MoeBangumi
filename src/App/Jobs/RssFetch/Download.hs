{- | Download integration for RSS items

This module handles downloading torrents via qBittorrent and saving
torrent records to the database.

== Download Flow

For each ParsedRssItem:
1. Check if savePath is configured (skip if Nothing)
2. Check if episode number exists (skip if Nothing)
3. Build magnet link from info_hash
4. Build rename pattern: "{titleChinese} s{season:02d}e{episode:02d}"
5. Add torrent with tags: ['moe', 'rename']
6. Save torrent record to database

Errors are logged but don't stop processing of other items.
-}
module App.Jobs.RssFetch.Download
  ( -- * Download Operations
    downloadSelectedItems
  , downloadAndSave

    -- * Utilities
  , buildMagnetLink
  , formatRename
  )
where

import Data.Aeson (object, (.=))
import Data.Text.Display (display)
import Effectful
import Effectful.Log (Log)
import Effectful.Log qualified as Log
import Text.Printf (printf)

import Infra.Database.Effect (DB)
import Infra.Database.Repository.Torrent (CreateTorrent (..))
import Infra.Database.Repository.Torrent qualified as TorrentRepo
import Infra.Downloader.Effect (Downloader, addTorrent)
import Infra.Downloader.Types (AddTorrentOptions (..), TorrentSource (..), defaultAddOptions)
import Infra.External.Rss.Parser (ParsedRssItem (..))
import Moe.Bangumi qualified
import Moe.Subscription qualified
import Moe.Parsing.Types (ParseResult (..))

--------------------------------------------------------------------------------
-- Download Integration
--------------------------------------------------------------------------------

{- | Download selected RSS items and save to database

For each ParsedRssItem:
1. Check if savePath is configured (skip if Nothing)
2. Check if episode number exists (skip if Nothing)
3. Build magnet link from info_hash
4. Build rename pattern: "{titleChinese} s01e{episode:02d}"
5. Add torrent with tags: ['moe', 'rename']
6. Save torrent record to database

Errors are logged but don't stop processing of other items.
-}
downloadSelectedItems ::
  (DB :> es, Downloader :> es, Log :> es) =>
  Moe.Subscription.Subscription ->
  Moe.Bangumi.Bangumi ->
  Int ->
  -- ^ RSS ID for reference
  [ParsedRssItem] ->
  Eff es ()
downloadSelectedItems subscription bangumi rssId items =
  forM_ items $ \item ->
    downloadAndSave subscription bangumi rssId item

{- | Download a single item and save to database

Skips download if:
- savePath is Nothing
- episode is Nothing

On downloader error: logs warning and continues (no DB record saved)
On success: saves torrent record to database
-}
downloadAndSave ::
  (DB :> es, Downloader :> es, Log :> es) =>
  Moe.Subscription.Subscription ->
  Moe.Bangumi.Bangumi ->
  Int ->
  -- ^ RSS ID
  ParsedRssItem ->
  Eff es ()
downloadAndSave subscription bangumi rssId item = do
  case (subscription.savePath, item.parseResult.episode) of
    (Nothing, _) ->
      Log.logTrace "Skipping download: no savePath configured" $
        object
          [ "subscription_id" .= subscription.id
          , "bangumi_id" .= bangumi.id
          , "info_hash" .= item.infoHash
          ]
    (_, Nothing) ->
      Log.logTrace "Skipping download: no episode number" $
        object
          [ "subscription_id" .= subscription.id
          , "bangumi_id" .= bangumi.id
          , "info_hash" .= item.infoHash
          ]
    (Just savePath, Just episode) -> do
      let magnetLink = buildMagnetLink item.infoHash
          renameTo = formatRename bangumi.titleChinese bangumi.season episode
          options =
            defaultAddOptions
              { savePath = Just savePath
              , tags = ["moe", "rename"]
              , rename = Just renameTo
              }

      Log.logInfo "Adding torrent to downloader" $
        object
          [ "subscription_id" .= subscription.id
          , "bangumi_id" .= bangumi.id
          , "episode" .= episode
          , "info_hash" .= item.infoHash
          , "save_path" .= savePath
          , "rename" .= renameTo
          ]

      result <- addTorrent (MagnetLink magnetLink) options

      case result of
        Left err ->
          Log.logAttention "Failed to add torrent to downloader" $
            object
              [ "subscription_id" .= subscription.id
              , "bangumi_id" .= bangumi.id
              , "info_hash" .= item.infoHash
              , "error" .= display err
              ]
        Right () -> do
          Log.logInfo "Torrent added successfully" $
            object
              [ "subscription_id" .= subscription.id
              , "bangumi_id" .= bangumi.id
              , "episode" .= episode
              , "info_hash" .= item.infoHash
              ]

          -- Check if torrent already exists before inserting (avoid unique constraint violation)
          existing <- TorrentRepo.findByInfoHash item.infoHash
          case existing of
            Just _ ->
              Log.logTrace "Torrent already exists in database, skipping insert" $
                object ["info_hash" .= item.infoHash]
            Nothing -> do
              -- Save torrent record to database
              let createDto =
                    CreateTorrent
                      { subscriptionId = subscription.id
                      , rssId = Just rssId
                      , infoHash = item.infoHash
                      , torrentUrl = item.torrentUrl
                      , episodeNumber = Just episode
                      , group = item.parseResult.subtitleGroup
                      , languages = Nothing
                      }
              void $ TorrentRepo.create createDto

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

{- | Build magnet link from info_hash

Format: magnet:?xt=urn:btih:{info_hash}
-}
buildMagnetLink :: Text -> Text
buildMagnetLink infoHash =
  "magnet:?xt=urn:btih:" <> infoHash

{- | Format rename pattern for qBittorrent

Format: "{titleChinese} s{season:02d}e{episode:02d}"

Examples:
- formatRename "葬送的芙莉莲" 1 5  -> "葬送的芙莉莲 s01e05"
- formatRename "无职转生" 2 12     -> "无职转生 s02e12"
-}
formatRename :: Text -> Int -> Int -> Text
formatRename titleChinese season episode =
  titleChinese <> " " <> toText (printf "s%02de%02d" season episode :: String)
