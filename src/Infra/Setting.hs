module Infra.Setting (
  readOrCreateSettings,
  writeSettings,
)
where

import Data.Aeson (decodeFileStrict, encodeFile)
import Effectful
import Effectful.FileSystem (FileSystem, doesFileExist)
import Moe.Setting
import Moe.Subtitle (Subtitle (..), SubtitlePattern (..))
import System.FilePath ((</>))

settingFile :: FilePath
settingFile = "setting.json"

-- | Read settings from file, or create it with default values if it doesn't exist
readOrCreateSettings ::
  (FileSystem :> es, IOE :> es) =>
  FilePath ->
  Eff es Settings
readOrCreateSettings path = do
  let settingPath = path </> settingFile
  exists <- doesFileExist settingPath
  if exists
    then do
      result <- liftIO $ decodeFileStrict settingPath
      case result of
        Just settings -> pure settings
        Nothing -> liftIO $ fail $ "Failed to decode settings file: " <> settingPath
    else do
      let settings = defaultSettings
      liftIO $ encodeFile settingPath settings
      pure settings

-- | Write settings to file
writeSettings ::
  (IOE :> es) =>
  FilePath ->
  Settings ->
  Eff es ()
writeSettings path settings = do
  let settingPath = path </> settingFile
  liftIO $ encodeFile settingPath settings

--------------------------------------------------------------------------------
-- Default Settings
--------------------------------------------------------------------------------

-- TODO: the each part of default settings should be in @src/Moe/Setting.hs
defaultSettings :: Settings
defaultSettings =
  Settings
    { downloader =
        DownloaderSettings
          { url = "http://localhost:8080"
          , username = ""
          , password = ""
          , savePath = "/Media/Bangumi"
          }
    , filter_ =
        FilterSettings
          { globalRssFilters =
              [ "720[Pp]"
              , "\\d-\\d"
              , "合集"
              ]
          }
    , notification =
        NotificationSettings
          { telegram =
              TelegramConfig
                { botToken = ""
                , chatId = ""
                }
          }
    , priority =
        PrioritySettings
          { groups =
              [ "SweetSub"
              , "千夏字幕组"
              , "拨雪寻春"
              , "喵萌奶茶屋"
              , "LoliHouse"
              , "北宇治"
              , "诸神字幕组"
              , "霜庭云花"
              , "桜都字幕组"
              , "澄空学园"
              ]
          , languages =
              [ SubtitlePattern [CHS, JAP]
              , SubtitlePattern [CHT, JAP]
              , SubtitlePattern [CHS]
              , SubtitlePattern [CHT]
              ]
          }
    , tmdb =
        TmdbSettings
          { apiKey = ""
          }
    , emby =
        EmbySettings
          { url = ""
          , apiKey = ""
          }
    }
