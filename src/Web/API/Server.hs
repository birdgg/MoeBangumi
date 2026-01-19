module Web.API.Server where

import Servant

import RequireCallStack
import Web.API.Routes qualified as API
import Web.API.Server.Bangumi qualified as BangumiAPI
import Web.API.Server.Calendar qualified as CalendarAPI
import Web.API.Server.Downloader qualified as DownloaderAPI
import Web.API.Server.Emby qualified as EmbyAPI
import Web.API.Server.Logs qualified as LogsAPI
import Web.API.Server.Metadata qualified as MetadataAPI
import Web.API.Server.Notification qualified as NotificationAPI
import Web.API.Server.Series qualified as SeriesAPI
import Web.API.Server.Settings qualified as SettingsAPI
import Web.API.Server.Torrent qualified as TorrentAPI
import Web.Types

apiServer :: RequireCallStack => ServerT API.Routes MoeEff
apiServer =
  API.Routes'
    { series = SeriesAPI.seriesServer
    , bangumi = BangumiAPI.bangumiServer
    , calendar = CalendarAPI.calendarServer
    , logs = LogsAPI.logsServer
    , torrent = TorrentAPI.torrentServer
    , notification = NotificationAPI.notificationServer
    , emby = EmbyAPI.embyServer
    , downloader = DownloaderAPI.downloaderServer
    , settings = SettingsAPI.settingsServer
    , metadata = MetadataAPI.metadataServer
    }
