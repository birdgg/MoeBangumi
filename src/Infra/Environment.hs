module Infra.Environment
  ( getMoeEnv
  , mkPool
  )
where

import Colourista (errorMessage, successMessage)
import Control.Exception (throwIO)
import GHC.IO.Exception (userError)
import Data.Pool (Pool)
import Data.Pool qualified as Pool
import Data.Pool.Introspection (defaultPoolConfig)
import Data.Text.Display (display)
import Database.SQLite.Simple qualified as SQLite
import Effectful
import Effectful.FileSystem (FileSystem, createDirectoryIfMissing)
import Env qualified
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS (tlsManagerSettings)

import UnliftIO.STM qualified as STM

import Infra.Database.Migration qualified as Migration
import Infra.Database.Types (Migration (..), MigrationError, MigrationResult (..))
import Infra.Environment.Config
import Infra.Environment.Env
import Infra.Setting (readOrCreateSettings)

mkPool
  :: IOE :> es
  => FilePath -- Database path
  -> Eff es (Pool SQLite.Connection)
mkPool dbPath =
  liftIO $
    Pool.newPool $
      defaultPoolConfig
        (SQLite.open dbPath)
        SQLite.close
        60  -- idle timeout in seconds
        10  -- max connections

configToEnv :: (FileSystem :> es, IOE :> es) => MoeConfig -> Eff es MoeEnv
configToEnv moeConfig = do
  let dataFolder = moeConfig.dataFolder
      posterFolder = dataFolder <> "/posters"
  createDirectoryIfMissing True dataFolder
  createDirectoryIfMissing True posterFolder
  let dbPath = dataFolder <> "/moe-bangumi.db"
  pool <- mkPool dbPath
  liftIO $ runMigrationsOnStartup pool
  settings <- readOrCreateSettings dataFolder
  settingsVar <- newTVarIO settings
  httpManager <- liftIO $ HTTP.newManager tlsManagerSettings
  logBroadcast <- liftIO STM.newBroadcastTChanIO
  pure
    MoeEnv
      { pool = pool
      , httpPort = moeConfig.httpPort
      , environment = moeConfig.environment
      , config = moeConfig
      , settingsVar = settingsVar
      , dataFolder = dataFolder
      , posterFolder = posterFolder
      , httpManager = httpManager
      , logBroadcast = logBroadcast
      }

getMoeEnv :: (FileSystem :> es, IOE :> es) => Eff es MoeEnv
getMoeEnv = do
  config <- liftIO $ Env.parse id parseConfig
  configToEnv config

-- | Run database migrations at startup
-- Throws an exception if migrations fail, causing the server to terminate
runMigrationsOnStartup :: Pool SQLite.Connection -> IO ()
runMigrationsOnStartup pool = do
  let migrationsDir = "migrations"

  Pool.withResource pool $ \conn -> do
    Migration.createMigrationsTable conn
    pendingResult <- Migration.getPendingMigrations conn migrationsDir

    case pendingResult of
      Left err -> do
        errorMessage $ "Failed to check migrations: " <> display err
        throwMigrationError err

      Right [] ->
        successMessage "Database schema is up to date"

      Right pending -> do
        putTextLn $ "📦 Found " <> show (length pending) <> " pending migration(s):"
        forM_ pending $ \m ->
          putTextLn $ "  → " <> m.migrationVersion <> "_" <> m.migrationName

        result <- Migration.runMigrations conn migrationsDir
        case result of
          Left err -> do
            errorMessage $ "Migration failed: " <> display err
            throwMigrationError err

          Right (MigrationSuccess count) ->
            successMessage $ "Successfully executed " <> show count <> " migration(s)"

          Right MigrationNoOp ->
            successMessage "No migrations needed"
  where
    throwMigrationError :: MigrationError -> IO a
    throwMigrationError err = throwIO $ userError $ toString $ display err
