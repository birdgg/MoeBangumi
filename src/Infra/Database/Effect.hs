{-# LANGUAGE TemplateHaskell #-}

-- | Effectful bindings for database operations
module Infra.Database.Effect (
  -- * Effect
  DB,

  -- * Connection Pool
  Pool.PoolConfig,
  Pool.defaultPoolConfig,
  Pool.newPool,
  Pool.destroyAllResources,

  -- * Handler
  runDBPool,

  -- * Query Operations
  query,
  query_,
  execute,
  execute_,
  executeMany,
  queryNamed,
  executeNamed,

  -- * Transaction
  withTransaction,

  -- * Migration
  runMigrations,
  getMigrationStatus,
  getPendingMigrations,
  createMigrationsTable,

  -- * Low-level
  withConnection,
  getConnection,

  -- * Re-exports
  module Database.SQLite.Simple,
  module Infra.Database.Types,
)
where

import Control.Exception (bracket_)
import Data.Pool (Pool)
import Data.Pool qualified as Pool
import Database.SQLite.Simple (Connection, FromRow, NamedParam, Only (..), Query, ToRow)
import Database.SQLite.Simple qualified as SQL
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH
import Infra.Database.Migration qualified as M
import Infra.Database.Types

-- | Transaction context: Nothing = use pool, Just = use this connection
type TxContext = IORef (Maybe Connection)

-- | Database effect for database operations
data DB :: Effect where
  Query :: (ToRow q, FromRow r) => Query -> q -> DB m [r]
  Query_ :: (FromRow r) => Query -> DB m [r]
  Execute :: (ToRow q) => Query -> q -> DB m ()
  Execute_ :: Query -> DB m ()
  ExecuteMany :: (ToRow q) => Query -> [q] -> DB m ()
  QueryNamed :: (FromRow r) => Query -> [NamedParam] -> DB m [r]
  ExecuteNamed :: Query -> [NamedParam] -> DB m ()
  WithTransaction :: m a -> DB m a
  RunMigrations :: FilePath -> DB m (Either MigrationError MigrationResult)
  GetMigrationStatus :: FilePath -> DB m (Either MigrationError ([MigrationRecord], [Migration]))
  GetPendingMigrations :: FilePath -> DB m (Either MigrationError [Migration])
  CreateMigrationsTable :: DB m ()
  GetConnection :: DB m Connection

type instance DispatchOf DB = Dynamic

makeEffect ''DB

-- | Run DB effect using a connection pool
runDBPool ::
  (IOE :> es) =>
  Pool Connection ->
  Eff (DB : es) a ->
  Eff es a
runDBPool pool action = do
  txCtx <- liftIO $ newIORef Nothing
  interpret (handleDB pool txCtx) action

-- | DB effect handler
handleDB ::
  (IOE :> es) =>
  Pool Connection ->
  TxContext ->
  EffectHandler DB es
handleDB pool txCtx env = \case
  Query q params ->
    withPoolConn pool txCtx $ \conn -> SQL.query conn q params
  Query_ q ->
    withPoolConn pool txCtx $ \conn -> SQL.query_ conn q
  Execute q params ->
    withPoolConn pool txCtx $ \conn -> SQL.execute conn q params
  Execute_ q ->
    withPoolConn pool txCtx $ \conn -> SQL.execute_ conn q
  ExecuteMany q params ->
    withPoolConn pool txCtx $ \conn -> SQL.executeMany conn q params
  QueryNamed q params ->
    withPoolConn pool txCtx $ \conn -> SQL.queryNamed conn q params
  ExecuteNamed q params ->
    withPoolConn pool txCtx $ \conn -> SQL.executeNamed conn q params
  WithTransaction innerAction ->
    unliftWithPoolConn pool txCtx env $ \conn unlift ->
      SQL.withTransaction conn (unlift innerAction)
  RunMigrations dir ->
    withPoolConn pool txCtx $ \conn -> M.runMigrations conn dir
  GetMigrationStatus dir ->
    withPoolConn pool txCtx $ \conn -> M.getMigrationStatus conn dir
  GetPendingMigrations dir ->
    withPoolConn pool txCtx $ \conn -> M.getPendingMigrations conn dir
  CreateMigrationsTable ->
    withPoolConn pool txCtx M.createMigrationsTable
  GetConnection ->
    withPoolConn pool txCtx pure

-- | Run an IO action with a connection
withPoolConn ::
  (IOE :> es) =>
  Pool Connection ->
  TxContext ->
  (Connection -> IO a) ->
  Eff es a
withPoolConn pool txCtx f = liftIO $ do
  mConn <- readIORef txCtx
  case mConn of
    Just conn -> f conn
    Nothing -> Pool.withResource pool f

-- | Run an effectful action with a dedicated transaction connection
unliftWithPoolConn ::
  (IOE :> es) =>
  Pool Connection ->
  TxContext ->
  LocalEnv localEs es ->
  (Connection -> (forall r. Eff localEs r -> IO r) -> IO a) ->
  Eff es a
unliftWithPoolConn pool txCtx env f =
  localSeqUnliftIO env $ \unlift ->
    Pool.withResource pool $ \conn ->
      bracket_
        (writeIORef txCtx (Just conn))
        (writeIORef txCtx Nothing)
        (f conn unlift)

-- | Access the raw connection
withConnection :: (DB :> es) => (Connection -> Eff es a) -> Eff es a
withConnection f = getConnection >>= f
