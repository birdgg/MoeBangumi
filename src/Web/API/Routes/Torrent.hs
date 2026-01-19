module Web.API.Routes.Torrent where

import Servant

import Moe.Torrent (Torrent)
import Web.API.Routes.Torrent.Types (AddTorrentDTO)

type API = NamedRoutes Routes'

data Routes' mode = Routes'
  { addTorrent
      :: mode
        :- Summary "Manually add a torrent download"
          :> ReqBody '[JSON] AddTorrentDTO
          :> Post '[JSON] Torrent
  }
  deriving (Generic)
