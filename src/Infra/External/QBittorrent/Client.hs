-- | qBittorrent Web API client using servant-client
--
-- This module provides ClientM functions for interacting with the qBittorrent Web API.
module Infra.External.QBittorrent.Client
  ( -- * Auth Operations
    login
  , logout

    -- * Torrent Operations
  , addTorrent
  , getTorrentsInfo
  , getTorrentFiles
  , pauseTorrents
  , resumeTorrents
  , deleteTorrents
  , addTags
  , removeTags
  , renameFile
  , setLocation

    -- * App Operations
  , defaultSavePath
  , setPreferences

    -- * Sync Operations
  , syncMaindata

    -- * Re-exports
  , module Infra.External.QBittorrent.Types
  , mkQBittorrentBaseUrl
  )
where

import Data.Aeson qualified as Aeson
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import Infra.External.QBittorrent.API (QBittorrentRoutes, mkQBittorrentBaseUrl)
import Infra.External.QBittorrent.API qualified as API
import Infra.External.QBittorrent.Types
import Moe.Setting (DownloaderSettings (..))
import Servant.API (NoContent)
import Servant.Client (ClientM)
import Servant.Client.Generic (AsClientT, genericClient)

-- | Generated client functions record
qbClient :: QBittorrentRoutes (AsClientT ClientM)
qbClient = genericClient

-- | Login (returns "Ok." on success or "Fails." on failure)
login :: DownloaderSettings -> ClientM Text
login cfg = API.login (API.auth qbClient) (LoginForm cfg.username cfg.password)

-- | Logout
logout :: ClientM NoContent
logout = API.logout (API.auth qbClient)

-- | Add torrent
addTorrent :: AddTorrentRequest -> ClientM Text
addTorrent = API.add (API.torrents qbClient)

-- | Get torrents info
getTorrentsInfo :: Maybe TorrentInfoRequest -> ClientM [TorrentInfo]
getTorrentsInfo mReq = case mReq of
  Nothing -> API.info (API.torrents qbClient) Nothing Nothing Nothing Nothing
  Just req ->
    API.info
      (API.torrents qbClient)
      (torrentFilterToText <$> req.filter)
      req.category
      req.tag
      req.hashes

-- | Get torrent files
getTorrentFiles :: Text -> ClientM [TorrentFile]
getTorrentFiles = API.files (API.torrents qbClient)

-- | Pause torrents
pauseTorrents :: [Text] -> ClientM NoContent
pauseTorrents hashes = API.pause (API.torrents qbClient) (HashesForm $ T.intercalate "|" hashes)

-- | Resume torrents
resumeTorrents :: [Text] -> ClientM NoContent
resumeTorrents hashes = API.resume (API.torrents qbClient) (HashesForm $ T.intercalate "|" hashes)

-- | Delete torrents
deleteTorrents :: [Text] -> Bool -> ClientM NoContent
deleteTorrents hashes deleteFiles =
  API.delete
    (API.torrents qbClient)
    (DeleteTorrentsForm (T.intercalate "|" hashes) (if deleteFiles then "true" else "false"))

-- | Add tags
addTags :: [Text] -> [Text] -> ClientM NoContent
addTags hashes tags =
  API.addTags (API.torrents qbClient) (TagsForm (T.intercalate "|" hashes) (T.intercalate "," tags))

-- | Remove tags
removeTags :: [Text] -> [Text] -> ClientM NoContent
removeTags hashes tags =
  API.removeTags (API.torrents qbClient) (TagsForm (T.intercalate "|" hashes) (T.intercalate "," tags))

-- | Rename file
renameFile :: Text -> Text -> Text -> ClientM NoContent
renameFile hash oldPath newPath =
  API.renameFile (API.torrents qbClient) (RenameFileForm hash oldPath newPath)

-- | Set location
setLocation :: [Text] -> Text -> ClientM NoContent
setLocation hashes location =
  API.setLocation (API.torrents qbClient) (SetLocationForm (T.intercalate "|" hashes) location)

-- | Get default save path
defaultSavePath :: ClientM Text
defaultSavePath = API.defaultSavePath (API.app qbClient)

-- | Set preferences
setPreferences :: Aeson.Value -> ClientM NoContent
setPreferences prefs =
  API.setPreferences (API.app qbClient) (PreferencesForm jsonStr)
  where
    jsonStr = TL.toStrict $ TLE.decodeUtf8 $ Aeson.encode prefs

-- | Sync maindata
syncMaindata :: Int64 -> ClientM SyncMainData
syncMaindata = API.maindata (API.sync qbClient)
