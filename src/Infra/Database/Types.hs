module Infra.Database.Types
  ( Migration (..)
  , MigrationRecord (..)
  , MigrationResult (..)
  , MigrationError (..)
  )
where

import Data.Text.Display (Display (..))
import Data.Time (UTCTime)
import Database.SQLite.Simple (FromRow (..), field)

-- | A migration file parsed from the filesystem
data Migration = Migration
  { migrationVersion :: Text
  -- ^ Version identifier from filename (e.g., "20240114120000")
  , migrationName :: Text
  -- ^ Descriptive name from filename (e.g., "create_users")
  , migrationSql :: Text
  -- ^ SQL content of the migration
  }
  deriving stock (Show, Eq)

-- | A record of an executed migration stored in _migrations table
data MigrationRecord = MigrationRecord
  { recordVersion :: Text
  , recordName :: Text
  , recordExecutedAt :: UTCTime
  }
  deriving stock (Show, Eq)

instance FromRow MigrationRecord where
  fromRow = MigrationRecord <$> field <*> field <*> field

-- | Result of running migrations
data MigrationResult
  = -- | Successfully executed N migrations
    MigrationSuccess Int
  | -- | No migrations to run
    MigrationNoOp
  deriving stock (Show, Eq)

-- | Migration errors
data MigrationError
  = -- | Invalid filename format
    InvalidFilename FilePath
  | -- | Failed to read migration file
    ReadError FilePath Text
  | -- | SQL execution error
    SqlError Text Text
  | -- | Migration directory not found
    DirectoryNotFound FilePath
  deriving stock (Show, Eq)

instance Display MigrationError where
  displayBuilder = \case
    InvalidFilename path ->
      "Invalid migration filename: " <> displayBuilder path
    ReadError path reason ->
      "Failed to read migration file '" <> displayBuilder path <> "': " <> displayBuilder reason
    SqlError context details ->
      "SQL error during " <> displayBuilder context <> ": " <> displayBuilder details
    DirectoryNotFound path ->
      "Migration directory not found: " <> displayBuilder path
