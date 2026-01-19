module Web.Server (
  -- * Server
  runMoe,
)
where

import Control.Exception (bracket)
import Control.Exception.Backtrace (BacktraceMechanism (..), setBacktraceMechanismState)
import Control.Monad.Except qualified as Except
import Data.Aeson
import Data.OpenApi (OpenApi, Operation, PathItem)
import Data.Pool qualified as Pool
import Data.Text.Display (display)
import Effectful
import Effectful.Concurrent
import Effectful.Error.Static (runErrorWith)
import Effectful.Fail (runFailIO)
import Effectful.FileSystem
import Effectful.Reader.Static qualified as Reader
import Effectful.Time (runTime)
import Infra.Database.Effect (runDBPool)
import Log (Logger)
import Log qualified

import Infra.Environment.Config (MoeConfig (..), toLogBaseLevel)
import Network.Wai qualified as Wai
import Network.Wai.Application.Static (
  defaultFileServerSettings,
  ss404Handler,
  ssIndices,
  staticApp,
 )
import Network.Wai.Handler.Warp (
  defaultSettings,
  runSettings,
  setOnException,
  setPort,
 )
import Optics.Core
import Servant (
  Application,
  Context (..),
  Handler,
  ServerError (..),
  Tagged (..),
  serveWithContextT,
 )
import Servant.OpenApi
import WaiAppStatic.Types (unsafeToPiece)

import Colourista (blueMessage)
import RequireCallStack
import Sel
import Servant.Server.Generic (AsServerT)

import App.Calendar.Job qualified as CalendarJob
import App.Jobs.LogCleanup qualified as LogCleanup
import App.Jobs.Rename qualified as Rename
import App.Jobs.RssFetch qualified as RssFetch
import App.Jobs.Scheduler qualified as Scheduler
import Infra.Environment (getMoeEnv)
import Infra.Environment.Env
import Infra.Logging qualified as Logging
import Moe.Monad (MoeM)
import Web.API.Routes qualified as API
import Web.API.Server qualified as API
import Web.Routers
import Web.Scalar (scalarHtml)
import Web.Tracing (handleExceptions)
import Web.Types

runMoe :: IO ()
runMoe = do
  setBacktraceMechanismState HasCallStackBacktrace True
  secureMain $
    bracket
      (getMoeEnv & runFileSystem & runFailIO & runEff)
      (runEff . shutdownMoe)
      ( \env ->
          runEff . withUnliftStrategy (ConcUnlift Ephemeral Unlimited) . runTime . runConcurrent $ do
            let baseURL = "http://localhost:" <> display env.httpPort
            liftIO $ blueMessage $ "🌺 Starting server on " <> baseURL
            let withLogger = Logging.makeLogger env.dataFolder (Just env.logBroadcast)
            withLogger
              ( \appLogger ->
                  provideCallStack $ runServer appLogger env
              )
      )

shutdownMoe :: MoeEnv -> Eff '[IOE] ()
shutdownMoe env =
  liftIO $
    Pool.destroyAllResources env.pool

runServer :: (Concurrent :> es, IOE :> es) => Logger -> MoeEnv -> MoeM es ()
runServer appLogger moeEnv = do
  -- Start background jobs (each job manages its own effect stack)
  let jobs =
        [ RssFetch.rssFetchJob appLogger moeEnv
        , LogCleanup.logCleanupJob appLogger moeEnv.environment (toLogBaseLevel moeEnv.config.logLevel) moeEnv.dataFolder
        , Rename.renameJob appLogger moeEnv
        , CalendarJob.calendarSyncJob appLogger moeEnv
        ]
  liftIO $ Scheduler.runJobsConcurrently jobs

  -- Start HTTP server
  let server = mkServer appLogger moeEnv
      warpSettings =
        setPort (fromIntegral moeEnv.httpPort) $
          setOnException (handleExceptions appLogger moeEnv.environment) defaultSettings
  liftIO $ runSettings warpSettings server

mkServer ::
  (RequireCallStack) =>
  Logger ->
  MoeEnv ->
  Application
mkServer logger moeEnv =
  serveWithContextT
    (Proxy @ServerRoutes)
    Servant.EmptyContext
    (naturalTransform moeEnv logger)
    moeServer

moeServer ::
  (RequireCallStack) =>
  Routes (AsServerT MoeEff)
moeServer =
  Routes
    { api = API.apiServer
    , doc = docServer
    , assets = spaFileServer "web/dist"
    }

docServer :: DocRoutes (AsServerT MoeEff)
docServer =
  DocRoutes
    { docUI = pure scalarHtml
    , openApiSpec = pure openApiHandler
    }

-- | Static file server with SPA fallback (serves index.html for unknown routes)
spaFileServer :: FilePath -> Tagged MoeEff Wai.Application
spaFileServer root = Tagged $ staticApp settings
 where
  settings =
    (defaultFileServerSettings root)
      { ssIndices = [unsafeToPiece "index.html"]
      , ss404Handler = Just serveIndexHtml
      }

  serveIndexHtml :: Wai.Application
  serveIndexHtml req =
    staticApp settings indexRequest
   where
    indexRequest =
      req
        { Wai.pathInfo = ["index.html"]
        , Wai.rawPathInfo = "/index.html"
        }

naturalTransform ::
  (RequireCallStack) =>
  MoeEnv ->
  Logger ->
  MoeEff a ->
  Handler a
naturalTransform moeEnv logger app = do
  result <-
    liftIO $
      Right
        <$> app
          & runDBPool moeEnv.pool
          & runTime
          & runErrorWith
            ( \callstack err -> do
                Log.logInfo "Server error" $
                  object
                    [ "error_headers" .= map show (errHeaders err)
                    , "error_http_code" .= errHTTPCode err
                    , "error_reason_phrase" .= errReasonPhrase err
                    , "exception" .= prettyCallStack callstack
                    ]
                pure . Left $ err
            )
          & Logging.runLog moeEnv.environment (toLogBaseLevel moeEnv.config.logLevel) logger
          & runConcurrent
          & Reader.runReader moeEnv
          & runEff
  either Except.throwError pure result

openApiHandler :: OpenApi
openApiHandler =
  toOpenApi (Proxy @API.Routes)
    & #info % #title .~ "Moe Bangumi API"
    & #info % #description ?~ "Moe Bangumi API Documentation"
    -- Set operationIds for all endpoints
    & setOperationId "/api/series" "get" "listSeries"
    & setOperationId "/api/bangumi" "get" "listBangumi"
    & setOperationId "/api/bangumi" "post" "createBangumi"
    & setOperationId "/api/bangumi/{id}" "get" "getBangumi"
    & setOperationId "/api/bangumi/{id}" "patch" "updateBangumi"
    & setOperationId "/api/calendar" "get" "getCalendar"
    & setOperationId "/api/calendar/refresh" "post" "refreshCalendar"
    & setOperationId "/api/logs" "get" "getLogs"
    & setOperationId "/api/logs/stream" "get" "streamLogs"
    & setOperationId "/api/torrent" "post" "addTorrent"
    & setOperationId "/api/notification/test" "post" "testNotification"
    & setOperationId "/api/emby/libraries" "get" "getEmbyLibraries"
    & setOperationId "/api/emby/import" "post" "importEmby"
    & setOperationId "/api/emby/test" "post" "testEmby"
    & setOperationId "/api/settings" "get" "getSettings"
    & setOperationId "/api/settings" "put" "updateSettings"
    & setOperationId "/api/metadata/search" "get" "searchMetadata"
    & setOperationId "/api/metadata/mikan/search" "get" "searchMikan"
    & setOperationId "/api/metadata/mikan/rss" "get" "getMikanRss"
    & setOperationId "/api/metadata/episodes/{subject_id}" "get" "getEpisodes"

-- | Helper function to set operationId for a specific path and method
setOperationId :: FilePath -> Text -> Text -> OpenApi -> OpenApi
setOperationId path method opId =
  #paths % at path % _Just % methodLens method % _Just % #operationId ?~ opId
 where
  methodLens :: Text -> Lens' PathItem (Maybe Operation)
  methodLens = \case
    "get" -> #get
    "post" -> #post
    "put" -> #put
    "patch" -> #patch
    "delete" -> #delete
    _ -> #get -- fallback
