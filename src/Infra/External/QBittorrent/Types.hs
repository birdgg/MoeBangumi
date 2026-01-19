-- | Types for qBittorrent Web API
module Infra.External.QBittorrent.Types
  ( -- * Enums
    TorrentFilter (..)
  , torrentFilterToText

    -- * Torrent Info
  , TorrentInfo (..)
  , isCompleted
  , TorrentFile (..)
  , TorrentInfoRequest (..)
  , AddTorrentRequest (..)

    -- * Sync
  , SyncMainData (..)
  , SyncTorrentInfo (..)
  , ServerState (..)

    -- * Form Types (for servant-client)
  , LoginForm (..)
  , HashesForm (..)
  , DeleteTorrentsForm (..)
  , TagsForm (..)
  , RenameFileForm (..)
  , SetLocationForm (..)
  , PreferencesForm (..)

    -- * Errors
  , QBittorrentError (..)
  )
where

import Data.Aeson
import Data.HashMap.Strict qualified as HashMap
import Data.Text.Display (Display (..), displayBuilder)
import Web.FormUrlEncoded (Form (..), ToForm (..))

-- | Torrent state filter for qBittorrent 5.0+
data TorrentFilter
  = FilterAll
  | FilterDownloading
  | FilterSeeding
  | FilterCompleted
  | FilterStopped
  | FilterActive
  | FilterInactive
  | FilterRunning
  | FilterStalled
  | FilterStalledUploading
  | FilterStalledDownloading
  | FilterErrored
  deriving stock (Show, Eq, Generic)

-- | Convert filter to API string
torrentFilterToText :: TorrentFilter -> Text
torrentFilterToText = \case
  FilterAll -> "all"
  FilterDownloading -> "downloading"
  FilterSeeding -> "seeding"
  FilterCompleted -> "completed"
  FilterStopped -> "stopped"
  FilterActive -> "active"
  FilterInactive -> "inactive"
  FilterRunning -> "running"
  FilterStalled -> "stalled"
  FilterStalledUploading -> "stalled_uploading"
  FilterStalledDownloading -> "stalled_downloading"
  FilterErrored -> "errored"

instance ToJSON TorrentFilter where
  toJSON = String . torrentFilterToText

-- | Request parameters for getting torrent list
data TorrentInfoRequest = TorrentInfoRequest
  { filter :: Maybe TorrentFilter
  , category :: Maybe Text
  , tag :: Maybe Text
  , hashes :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

-- | Torrent information from qBittorrent
data TorrentInfo = TorrentInfo
  { hash :: Text
  , name :: Text
  , state :: Text
  , progress :: Double
  , savePath :: Text
  , size :: Int64
  , downloaded :: Int64
  , eta :: Int64
  , tags :: Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON TorrentInfo where
  parseJSON = withObject "TorrentInfo" $ \o ->
    TorrentInfo
      <$> o .: "hash"
      <*> o .: "name"
      <*> o .: "state"
      <*> o .: "progress"
      <*> o .: "save_path"
      <*> o .: "size"
      <*> o .: "downloaded"
      <*> o .: "eta"
      <*> o .:? "tags" .!= ""

-- | Check if the torrent download is completed
isCompleted :: TorrentInfo -> Bool
isCompleted t =
  t.progress >= 1.0
    || t.state == "uploading"
    || t.state == "stalledUP"
    || t.state == "pausedUP"
    || t.state == "forcedUP"
    || t.state == "queuedUP"
    || t.state == "checkingUP"

-- | File information within a torrent
data TorrentFile = TorrentFile
  { index :: Int
  , name :: Text
  , size :: Int64
  , progress :: Double
  , priority :: Int
  , isSeed :: Bool
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON TorrentFile where
  parseJSON = withObject "TorrentFile" $ \o ->
    TorrentFile
      <$> o .: "index"
      <*> o .: "name"
      <*> o .: "size"
      <*> o .: "progress"
      <*> o .: "priority"
      <*> o .: "is_seed"

-- | Request to add torrents via URLs
data AddTorrentRequest = AddTorrentRequest
  { urls :: Maybe Text
  , savepath :: Maybe Text
  , category :: Maybe Text
  , tags :: Maybe Text
  , rename :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON AddTorrentRequest where
  toJSON req =
    object $
      catMaybes
        [ ("urls" .=) <$> req.urls
        , ("savepath" .=) <$> req.savepath
        , ("category" .=) <$> req.category
        , ("tags" .=) <$> req.tags
        , ("rename" .=) <$> req.rename
        ]

-- | Sync maindata response from qBittorrent
data SyncMainData = SyncMainData
  { rid :: Int64
  , fullUpdate :: Bool
  , torrents :: Map Text SyncTorrentInfo
  , torrentsRemoved :: [Text]
  , serverState :: Maybe ServerState
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON SyncMainData where
  parseJSON = withObject "SyncMainData" $ \o ->
    SyncMainData
      <$> o .: "rid"
      <*> o .:? "full_update" .!= False
      <*> o .:? "torrents" .!= mempty
      <*> o .:? "torrents_removed" .!= []
      <*> o .:? "server_state"

-- | Partial torrent info for sync API
data SyncTorrentInfo = SyncTorrentInfo
  { name :: Maybe Text
  , state :: Maybe Text
  , progress :: Maybe Double
  , savePath :: Maybe Text
  , size :: Maybe Int64
  , downloaded :: Maybe Int64
  , eta :: Maybe Int64
  , dlspeed :: Maybe Int64
  , upspeed :: Maybe Int64
  , numSeeds :: Maybe Int64
  , numLeechs :: Maybe Int64
  , ratio :: Maybe Double
  , addedOn :: Maybe Int64
  , completionOn :: Maybe Int64
  , category :: Maybe Text
  , tags :: Maybe Text
  , contentPath :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON SyncTorrentInfo where
  parseJSON = withObject "SyncTorrentInfo" $ \o ->
    SyncTorrentInfo
      <$> o .:? "name"
      <*> o .:? "state"
      <*> o .:? "progress"
      <*> o .:? "save_path"
      <*> o .:? "size"
      <*> o .:? "downloaded"
      <*> o .:? "eta"
      <*> o .:? "dlspeed"
      <*> o .:? "upspeed"
      <*> o .:? "num_seeds"
      <*> o .:? "num_leechs"
      <*> o .:? "ratio"
      <*> o .:? "added_on"
      <*> o .:? "completion_on"
      <*> o .:? "category"
      <*> o .:? "tags"
      <*> o .:? "content_path"

-- | Server state from sync maindata
data ServerState = ServerState
  { dlInfoSpeed :: Maybe Int64
  , upInfoSpeed :: Maybe Int64
  , dlInfoData :: Maybe Int64
  , upInfoData :: Maybe Int64
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ServerState where
  parseJSON = withObject "ServerState" $ \o ->
    ServerState
      <$> o .:? "dl_info_speed"
      <*> o .:? "up_info_speed"
      <*> o .:? "dl_info_data"
      <*> o .:? "up_info_data"

-- | Errors that can occur during qBittorrent API operations
data QBittorrentError
  = -- | Network request failed
    NetworkError Text
  | -- | Authentication failed
    AuthError Text
  | -- | API returned an error status (status code, message)
    ApiError Int Text
  | -- | Failed to parse JSON response
    ParseError Text
  | -- | Invalid torrent provided
    InvalidTorrent Text
  deriving stock (Show, Eq)

instance Display QBittorrentError where
  displayBuilder (NetworkError msg) =
    "QBittorrent network error: " <> displayBuilder msg
  displayBuilder (AuthError msg) =
    "QBittorrent authentication error: " <> displayBuilder msg
  displayBuilder (ApiError code msg) =
    "QBittorrent API error " <> displayBuilder code <> ": " <> displayBuilder msg
  displayBuilder (ParseError msg) =
    "QBittorrent parse error: " <> displayBuilder msg
  displayBuilder (InvalidTorrent msg) =
    "QBittorrent invalid torrent: " <> displayBuilder msg

-- ---------------------------------------------------------------------
-- Form Types for servant-client
-- ---------------------------------------------------------------------

-- | Login form
data LoginForm = LoginForm
  { username :: Text
  , password :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToForm)

-- | Hashes form (for pause/resume)
newtype HashesForm = HashesForm {hashes :: Text}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToForm)

-- | Delete torrents form
data DeleteTorrentsForm = DeleteTorrentsForm
  { hashes :: Text
  , deleteFiles :: Text -- "true" or "false"
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToForm)

-- | Tags form
data TagsForm = TagsForm
  { hashes :: Text
  , tags :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToForm)

-- | Rename file form
data RenameFileForm = RenameFileForm
  { hash :: Text
  , oldPath :: Text
  , newPath :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToForm)

-- | Set location form
data SetLocationForm = SetLocationForm
  { hashes :: Text
  , location :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToForm)

-- | Preferences form (json field contains JSON string)
newtype PreferencesForm = PreferencesForm {json :: Text}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToForm)

-- | ToForm instance for AddTorrentRequest
instance ToForm AddTorrentRequest where
  toForm req =
    Form $
      HashMap.fromList $
        catMaybes
          [ fmap (\v -> ("urls", [v])) req.urls
          , fmap (\v -> ("savepath", [v])) req.savepath
          , fmap (\v -> ("category", [v])) req.category
          , fmap (\v -> ("tags", [v])) req.tags
          , fmap (\v -> ("rename", [v])) req.rename
          ]
