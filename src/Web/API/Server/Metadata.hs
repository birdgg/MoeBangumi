-- | Metadata API handlers
module Web.API.Server.Metadata (
  metadataServer,
)
where

import App.Metadata.Service qualified as Service
import Effectful (IOE, (:>))
import Effectful.Reader.Static qualified as Reader
import Infra.Environment.Env (MoeEnv (..))
import Infra.External.BangumiData.Client qualified as BangumiData
import Infra.External.BangumiData.Types (CalendarItem)
import Infra.External.Bgmtv.API (bgmtvBaseUrl)
import Infra.External.Bgmtv.Client qualified as BgmtvClient
import Infra.External.Bgmtv.Types qualified as Bgm
import Moe.Metadata
import Moe.Monad (MoeM)
import RequireCallStack
import Servant hiding ((:>))
import Servant.Client (mkClientEnv, runClientM)
import Web.API.Routes.Metadata qualified as Metadata
import Web.Types (MoeEff)

-- | Metadata API server implementation
metadataServer :: (RequireCallStack) => ServerT Metadata.API MoeEff
metadataServer =
  Metadata.Routes'
    { searchBgmtv = searchBgmtvHandler
    , searchTmdb = searchTmdbHandler
    , mikanSearch = mikanSearchHandler
    , mikanRss = mikanRssHandler
    , episodes = episodesHandler
    , bangumiData = bangumiDataHandler
    }

-- | Search BGM.tv metadata handler
searchBgmtvHandler ::
  ( Reader.Reader MoeEnv :> es
  , IOE :> es
  ) =>
  Text ->
  MoeM es [BgmtvSearchItem]
searchBgmtvHandler keyword = Service.searchFromBgmtv keyword

-- | Search TMDB metadata handler
searchTmdbHandler ::
  ( Reader.Reader MoeEnv :> es
  , IOE :> es
  ) =>
  Text ->
  MoeM es [TmdbSearchItem]
searchTmdbHandler keyword = Service.searchFromTmdb keyword

-- | Search Mikan handler (placeholder - returns empty for now)
mikanSearchHandler ::
  ( Reader.Reader MoeEnv :> es
  , IOE :> es
  ) =>
  Text ->
  MoeM es [MikanSearchResult]
mikanSearchHandler _keyword = do
  -- TODO: Implement Mikan search when API client is available
  pure []

-- | Get Mikan RSS handler (placeholder - returns empty for now)
mikanRssHandler ::
  ( Reader.Reader MoeEnv :> es
  , IOE :> es
  ) =>
  Text ->
  MoeM es MikanBangumiDetail
mikanRssHandler mikanId = do
  -- TODO: Implement Mikan RSS when API client is available
  pure
    MikanBangumiDetail
      { id = mikanId
      , name = ""
      , subgroups = []
      }

-- | Get episodes for a BGM.tv subject
episodesHandler ::
  ( Reader.Reader MoeEnv :> es
  , IOE :> es
  ) =>
  Int64 ->
  MoeM es [EpisodeInfo]
episodesHandler subjectId = do
  env <- Reader.ask @MoeEnv
  let clientEnv = mkClientEnv env.httpManager bgmtvBaseUrl
  result <- liftIO $ runClientM (BgmtvClient.getEpisodes subjectId) clientEnv
  case result of
    Left _err -> pure []
    Right response -> pure $ map toEpisodeInfo response.data_
 where
  toEpisodeInfo :: Bgm.Episode -> EpisodeInfo
  toEpisodeInfo ep =
    EpisodeInfo
      { id = ep.id
      , sort = ep.sort
      , ep = ep.ep
      , name = ep.name
      , nameCn = ep.nameCn
      , airdate = ep.airdate
      }

-- | Lookup bangumi-data entry by BGM.tv ID and air date
bangumiDataHandler ::
  ( Reader.Reader MoeEnv :> es
  , IOE :> es
  ) =>
  Int ->
  Text ->
  MoeM es (Maybe CalendarItem)
bangumiDataHandler bgmtvId airdate = do
  env <- Reader.ask @MoeEnv
  result <- liftIO $ BangumiData.findByBgmtvId env.httpManager bgmtvId airdate
  case result of
    Left _err -> pure Nothing
    Right item -> pure item
