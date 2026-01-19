module Infra.Database.Migration
  ( runMigrations
  , getMigrationStatus
  , getPendingMigrations
  , createMigrationsTable
  )
where

import Control.Exception (try)
import GHC.IO.Exception (IOError)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Database.SQLite.Simple (Connection, Only (..), SQLError, execute_, query_)
import Database.SQLite.Simple qualified as SQL
import Database.SQLite3 qualified as Direct
import Infra.Database.Types
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeBaseName, takeExtension, (</>))

-- | Run all pending migrations from the given directory
runMigrations :: Connection -> FilePath -> IO (Either MigrationError MigrationResult)
runMigrations conn migrationsDir = do
  exists <- doesDirectoryExist migrationsDir
  if not exists
    then pure $ Left $ DirectoryNotFound migrationsDir
    else do
      createMigrationsTable conn
      pendingResult <- getPendingMigrations conn migrationsDir
      case pendingResult of
        Left err -> pure $ Left err
        Right [] -> pure $ Right MigrationNoOp
        Right pending -> runPendingMigrations conn pending

-- | Get list of pending (not yet executed) migrations
getPendingMigrations :: Connection -> FilePath -> IO (Either MigrationError [Migration])
getPendingMigrations conn migrationsDir = do
  migrationsResult <- loadMigrationsFromDir migrationsDir
  case migrationsResult of
    Left err -> pure $ Left err
    Right migrations -> do
      executed <- getExecutedVersions conn
      let pending = filter (\m -> migrationVersion m `Set.notMember` executed) migrations
      pure $ Right $ sortOn migrationVersion pending

-- | Get migration status (executed and pending)
getMigrationStatus :: Connection -> FilePath -> IO (Either MigrationError ([MigrationRecord], [Migration]))
getMigrationStatus conn migrationsDir = do
  exists <- doesDirectoryExist migrationsDir
  if not exists
    then pure $ Left $ DirectoryNotFound migrationsDir
    else do
      createMigrationsTable conn
      executed <- getExecutedMigrations conn
      pendingResult <- getPendingMigrations conn migrationsDir
      case pendingResult of
        Left err -> pure $ Left err
        Right pending -> pure $ Right (executed, pending)

-- | Create the _migrations table if it doesn't exist
createMigrationsTable :: Connection -> IO ()
createMigrationsTable conn =
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS _migrations (\
    \  version TEXT PRIMARY KEY NOT NULL,\
    \  name TEXT NOT NULL,\
    \  executed_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL\
    \)"

-- | Load all migration files from directory
loadMigrationsFromDir :: FilePath -> IO (Either MigrationError [Migration])
loadMigrationsFromDir dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then pure $ Left $ DirectoryNotFound dir
    else do
      filesResult <- try $ listDirectory dir
      case filesResult of
        Left (err :: IOError) ->
          pure $ Left $ ReadError dir (T.pack $ show err)
        Right files -> do
          let sqlFiles = filter (\f -> takeExtension f == ".sql") files
          loadMigrationFiles dir sqlFiles
  where
    loadMigrationFiles :: FilePath -> [FilePath] -> IO (Either MigrationError [Migration])
    loadMigrationFiles baseDir sqlFiles = do
      results <- forM sqlFiles $ \filename -> do
        case parseMigrationFilename filename of
          Nothing -> pure $ Left $ InvalidFilename filename
          Just (version, name) -> do
            contentResult <- try $ TIO.readFile (baseDir </> filename)
            case contentResult of
              Left (err :: IOError) ->
                pure $ Left $ ReadError filename (T.pack $ show err)
              Right content ->
                pure $ Right $ Migration version name content
      pure $ sequence results

-- | Parse migration filename into (version, name)
parseMigrationFilename :: FilePath -> Maybe (Text, Text)
parseMigrationFilename filename = do
  let baseName = takeBaseName filename
      (versionPart, rest) = break (== '_') baseName
  guard (length versionPart == 14)
  guard (all (`elem` ['0' .. '9']) versionPart)
  case rest of
    ('_' : namePart) | not (null namePart) ->
      Just (T.pack versionPart, T.pack namePart)
    _ -> Nothing

-- | Get set of executed migration versions
getExecutedVersions :: Connection -> IO (Set.Set Text)
getExecutedVersions conn = do
  rows <- query_ conn "SELECT version FROM _migrations" :: IO [Only Text]
  pure $ Set.fromList $ map fromOnly rows

-- | Get all executed migration records
getExecutedMigrations :: Connection -> IO [MigrationRecord]
getExecutedMigrations conn =
  query_ conn "SELECT version, name, executed_at FROM _migrations ORDER BY version"

-- | Run pending migrations in a transaction
runPendingMigrations :: Connection -> [Migration] -> IO (Either MigrationError MigrationResult)
runPendingMigrations conn migrations = do
  result <- try $ SQL.withTransaction conn $ do
    forM migrations $ \migration -> do
      -- Use direct-sqlite exec to execute multiple SQL statements
      let directConn = SQL.connectionHandle conn
      Direct.exec directConn (migrationSql migration)
      SQL.execute
        conn
        "INSERT INTO _migrations (version, name) VALUES (?, ?)"
        (migrationVersion migration, migrationName migration)
  case result of
    Left (err :: SQLError) ->
      pure $ Left $ SqlError "Migration failed" (T.pack $ show err)
    Right _ ->
      pure $ Right $ MigrationSuccess (length migrations)
