{- | Emby API handlers

Implements the Emby import API endpoints.
-}
module Web.API.Server.Emby
  ( embyServer
  )
where

import Data.Text.Display (display)
import Effectful (IOE, runEff, (:>))
import Effectful.Concurrent (runConcurrent)
import Effectful.Error.Static (Error, throwError)
import Effectful.Log (Log)
import Effectful.Reader.Static qualified as Reader
import Log.Backend.StandardOutput qualified as StdOut
import Servant hiding ((:>), throwError)

import Infra.Environment.Config (MoeConfig (..), toLogBaseLevel)

import App.Emby.Sync (ImportResult, importFromEmby)
import Infra.Database.Effect (DB, runDBPool)
import Infra.Environment.Env (MoeEnv (..))
import Infra.External.Emby.Effect (EmbyLibrary (..), getEmbyLibraries, runEmby)
import Infra.Logging (runLog)
import Moe.Monad (MoeM)
import Moe.Setting (Settings (..))
import RequireCallStack
import Web.API.Routes.Emby qualified as EmbyRoutes
import Web.Types (MoeEff)

-- | Emby API server implementation
embyServer :: (RequireCallStack) => ServerT EmbyRoutes.API MoeEff
embyServer =
  EmbyRoutes.Routes'
    { libraries = librariesHandler
    , import_ = importHandler
    , test = testHandler
    }

-- | Get Emby libraries
librariesHandler ::
  ( Reader.Reader MoeEnv :> es
  , Error ServerError :> es
  , IOE :> es
  ) =>
  MoeM es [EmbyRoutes.EmbyLibraryInfo]
librariesHandler = do
  env <- Reader.ask @MoeEnv
  settings :: Settings <- liftIO $ readTVarIO env.settingsVar
  let embySettings = settings.emby

  result <- liftIO $
    getEmbyLibraries
      & runEmby env.httpManager embySettings
      & runEff

  case result of
    Left err ->
      throwError err500 {errBody = encodeUtf8 $ display err}
    Right libs ->
      -- Filter and convert to EmbyLibraryInfo, only return TV libraries
      pure $
        libs
          & filter isTvLibrary
          & mapMaybe toLibraryInfo
 where
  isTvLibrary lib = case lib.collectionType of
    Just "tvshows" -> True
    Just "mixed" -> True
    _ -> False

  toLibraryInfo lib = do
    libId <- lib.itemId
    pure $
      EmbyRoutes.EmbyLibraryInfo
        { id = libId
        , name = lib.name
        , collectionType = lib.collectionType
        }

-- | Import Bangumi from Emby
importHandler ::
  ( DB :> es
  , Reader.Reader MoeEnv :> es
  , Error ServerError :> es
  , Log :> es
  , IOE :> es
  ) =>
  EmbyRoutes.ImportEmbyRequest ->
  MoeM es ImportResult
importHandler req = do
  env <- Reader.ask @MoeEnv
  settings :: Settings <- liftIO $ readTVarIO env.settingsVar
  let embySettings = settings.emby
  let libraryIds = req.libraryIds

  -- Run import with Emby effect using a temporary stdout logger
  liftIO $
    StdOut.withJsonStdOutLogger $ \logger ->
      importFromEmby embySettings libraryIds
        & runDBPool env.pool
        & runEmby env.httpManager embySettings
        & runLog env.environment (toLogBaseLevel env.config.logLevel) logger
        & runConcurrent
        & runEff

-- | Test Emby connection
testHandler ::
  ( Reader.Reader MoeEnv :> es
  , Error ServerError :> es
  , IOE :> es
  ) =>
  EmbyRoutes.TestEmbyRequest ->
  MoeM es EmbyRoutes.TestEmbyResponse
testHandler req = do
  env <- Reader.ask @MoeEnv
  let embySettings = req.settings

  -- Try to get libraries to test connection
  result <- liftIO $
    getEmbyLibraries
      & runEmby env.httpManager embySettings
      & runEff

  pure $ case result of
    Right libraries ->
      EmbyRoutes.TestEmbyResponse
        { success = True
        , message = "连接成功"
        , libraryCount = Just (length libraries)
        }
    Left err ->
      EmbyRoutes.TestEmbyResponse
        { success = False
        , message = display err
        , libraryCount = Nothing
        }
