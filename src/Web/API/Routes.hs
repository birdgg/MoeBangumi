module Web.API.Routes where

import Servant

import Web.API.Routes.Bangumi qualified as Bangumi
import Web.API.Routes.Calendar qualified as Calendar
import Web.API.Routes.Downloader qualified as Downloader
import Web.API.Routes.Emby qualified as Emby
import Web.API.Routes.Logs qualified as Logs
import Web.API.Routes.Metadata qualified as Metadata
import Web.API.Routes.Notification qualified as Notification
import Web.API.Routes.Series qualified as Series
import Web.API.Routes.Settings qualified as Settings
import Web.API.Routes.Torrent qualified as Torrent

type Routes = "api" :> NamedRoutes Routes'

data Routes' mode = Routes'
  { series :: mode :- "series" :> Series.API
  , bangumi :: mode :- "bangumi" :> Bangumi.API
  , calendar :: mode :- "calendar" :> Calendar.API
  , logs :: mode :- "logs" :> Logs.API
  , torrent :: mode :- "torrent" :> Torrent.API
  , notification :: mode :- "notification" :> Notification.API
  , emby :: mode :- "emby" :> Emby.API
  , downloader :: mode :- "downloader" :> Downloader.API
  , settings :: mode :- "settings" :> Settings.API
  , metadata :: mode :- "metadata" :> Metadata.API
  }
  deriving stock (Generic)
