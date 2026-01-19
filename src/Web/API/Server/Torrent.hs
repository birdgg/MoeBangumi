-- | Torrent API handlers
module Web.API.Server.Torrent
  ( torrentServer
  )
where

import Data.Aeson (object, (.=))
import Data.Text.Display (display)
import Data.ByteString.Lazy qualified as LBS
import Data.Text.Encoding qualified as TE
import Effectful (IOE, runEff, (:>))
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
import Effectful.Log (Log)
import Effectful.Log qualified as Log
import Effectful.Reader.Static qualified as Reader
import Servant hiding ((:>))

import App.Jobs.RssFetch.Download (formatRename)
import Infra.Database.Effect (DB)
import Infra.Database.Repository.Bangumi qualified as BangumiRepo
import Infra.Database.Repository.Subscription qualified as SubRepo
import Infra.Database.Repository.Torrent (CreateTorrent (..))
import Infra.Database.Repository.Torrent qualified as TorrentRepo
import Infra.Downloader.Effect (addTorrent)
import Infra.Downloader.QBittorrent (runDownloader)
import Infra.Downloader.Types (AddTorrentOptions (..), TorrentSource (..), defaultAddOptions)
import Infra.Environment.Env (MoeEnv (..))
import Infra.External.Rss.Parser (extractInfoHashFromMagnet)
import Moe.Bangumi (Bangumi (..))
import Moe.Subscription (Subscription (..))
import Moe.Monad (MoeM)
import Moe.Setting (Settings (..))
import Moe.Torrent (Torrent (..))
import RequireCallStack
import Web.API.Routes.Torrent qualified as TorrentRoutes
import Web.API.Routes.Torrent.Types (AddTorrentDTO (..))
import Web.Types (MoeEff)

-- | Torrent API server implementation
torrentServer :: RequireCallStack => ServerT TorrentRoutes.API MoeEff
torrentServer =
  TorrentRoutes.Routes'
    { addTorrent = addTorrentHandler
    }

{- | Manually add a torrent download

Workflow:
1. Validate subscriptionId exists and get savePath
2. Get bangumi for subscription (for title/season info)
3. Extract infoHash from magnet link
4. Add torrent to qBittorrent via Downloader Effect
5. Save torrent record to database
6. Return created Torrent record
-}
addTorrentHandler ::
  ( DB :> es
  , Reader.Reader MoeEnv :> es
  , Error ServerError :> es
  , Log :> es
  , IOE :> es
  ) =>
  AddTorrentDTO ->
  MoeM es Torrent
addTorrentHandler dto = do
  -- Step 1: Validate Subscription exists
  maybeSubscription <- SubRepo.getOne dto.subscriptionId
  subscription <- case maybeSubscription of
    Nothing ->
      Error.throwError $
        err404
          { errBody = "Subscription not found with ID: " <> show (dto.subscriptionId :: Int)
          }
    Just s -> pure s

  -- Validate savePath is configured
  savePath <- case subscription.savePath of
    Nothing ->
      Error.throwError $
        err400
          { errBody = "Subscription has no savePath configured (subscription_id: " <> show (dto.subscriptionId :: Int) <> ")"
          }
    Just path -> pure path

  -- Step 2: Get Bangumi for title/season info
  maybeBangumi <- BangumiRepo.getOne subscription.bangumiId
  bangumi <- case maybeBangumi of
    Nothing ->
      Error.throwError $
        err500
          { errBody = "Bangumi not found for subscription"
          }
    Just b -> pure b

  -- Step 3: Extract infoHash from magnet link
  infoHash <- case extractInfoHashFromMagnet dto.torrentUrl of
    Nothing ->
      Error.throwError $
        err400
          { errBody = "Failed to extract infoHash from magnet link: " <> LBS.fromStrict (TE.encodeUtf8 dto.torrentUrl)
          }
    Just hash -> pure hash

  Log.logInfo "Adding torrent manually" $
    object
      [ "subscription_id" .= dto.subscriptionId
      , "bangumi_id" .= subscription.bangumiId
      , "episode" .= dto.episodeNumber
      , "info_hash" .= infoHash
      , "save_path" .= savePath
      ]

  -- Step 4: Add torrent to downloader
  env <- Reader.ask
  settings :: Settings <- liftIO $ readTVarIO env.settingsVar

  let renameTo = formatRename bangumi.titleChinese bangumi.season dto.episodeNumber
      options =
        defaultAddOptions
          { savePath = Just savePath
          , tags = ["moe", "rename"]
          , rename = Just renameTo
          }

  -- Run Downloader Effect locally
  result <-
    liftIO $
      runEff $
        runDownloader env.httpManager settings.downloader $
          addTorrent (MagnetLink dto.torrentUrl) options

  case result of
    Left err ->
      Error.throwError $
        err502
          { errBody = "Failed to add torrent to downloader: " <> LBS.fromStrict (TE.encodeUtf8 (display err))
          }
    Right () -> do
      Log.logInfo "Torrent added to downloader successfully" $
        object
          [ "info_hash" .= infoHash
          , "rename" .= renameTo
          ]

      -- Step 5: Check if torrent already exists in DB
      existing <- TorrentRepo.findByInfoHash infoHash
      case existing of
        Just existingTorrent -> do
          Log.logTrace "Torrent already exists in database" $
            object ["info_hash" .= infoHash, "torrent_id" .= existingTorrent.id]
          pure existingTorrent
        Nothing -> do
          -- Step 6: Save torrent record to database
          let createDto =
                CreateTorrent
                  { subscriptionId = dto.subscriptionId
                  , rssId = Nothing -- Manual addition has no RSS source
                  , infoHash = infoHash
                  , torrentUrl = dto.torrentUrl
                  , episodeNumber = Just dto.episodeNumber
                  , group = Nothing
                  , languages = Nothing
                  }
          TorrentRepo.create createDto
