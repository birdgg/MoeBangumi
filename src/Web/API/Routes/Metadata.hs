module Web.API.Routes.Metadata where

import Infra.External.BangumiData.Types (CalendarItem)
import Moe.Metadata
  ( BgmtvSearchItem
  , EpisodeInfo
  , MikanBangumiDetail
  , MikanSearchResult
  , TmdbSearchItem
  )
import Servant

type API = NamedRoutes Routes'

data Routes' mode = Routes'
  { searchBgmtv ::
      mode
        :- Summary "Search bangumi metadata from BGM.tv"
          :> "search"
          :> "bgmtv"
          :> QueryParam' '[Required, Strict] "keyword" Text
          :> Get '[JSON] [BgmtvSearchItem]
  , searchTmdb ::
      mode
        :- Summary "Search bangumi metadata from TMDB"
          :> "search"
          :> "tmdb"
          :> QueryParam' '[Required, Strict] "keyword" Text
          :> Get '[JSON] [TmdbSearchItem]
  , mikanSearch ::
      mode
        :- Summary "Search bangumi on Mikan"
          :> "mikan"
          :> "search"
          :> QueryParam' '[Required, Strict] "keyword" Text
          :> Get '[JSON] [MikanSearchResult]
  , mikanRss ::
      mode
        :- Summary "Get Mikan bangumi detail with RSS URLs"
          :> "mikan"
          :> "rss"
          :> QueryParam' '[Required, Strict] "id" Text
          :> Get '[JSON] MikanBangumiDetail
  , episodes ::
      mode
        :- Summary "Get episodes for a BGM.tv subject"
          :> "episodes"
          :> Capture "subject_id" Int64
          :> Get '[JSON] [EpisodeInfo]
  , bangumiData ::
      mode
        :- Summary "Lookup bangumi-data entry by BGM.tv ID and air date"
          :> Description "Uses airdate to determine which seasonal JSON file to fetch, then searches for matching bgmtv_id"
          :> "bangumi-data"
          :> QueryParam' '[Required, Strict] "bgmtv_id" Int
          :> QueryParam' '[Required, Strict] "airdate" Text
          :> Get '[JSON] (Maybe CalendarItem)
  }
  deriving (Generic)
