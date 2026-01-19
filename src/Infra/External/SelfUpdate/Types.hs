module Infra.External.SelfUpdate.Types
  ( -- * Configuration
    UpdateConfig (..)
  , defaultUpdateConfig

    -- * Release Information
  , Release (..)
  , ReleaseAsset (..)

    -- * Update Status
  , UpdateStatus (..)
  , UpdateResult (..)

    -- * Errors
  , UpdateError (..)
  )
where

-- | Configuration for checking and performing updates
data UpdateConfig = UpdateConfig
  { repoOwner :: Text
  -- ^ GitHub repository owner (user or organization)
  , repoName :: Text
  -- ^ GitHub repository name
  , binName :: Text
  -- ^ Name of the binary file to update
  , currentVersion :: Text
  -- ^ Current version of the application (e.g., "1.0.0" or "v1.0.0")
  , targetTriple :: Maybe Text
  -- ^ Optional target platform triple (e.g., "x86_64-unknown-linux-gnu").
  -- If Nothing, will be detected automatically.
  , authToken :: Maybe Text
  -- ^ Optional GitHub personal access token for private repositories
  }
  deriving stock (Show, Eq)

-- | Create a default config with required fields
defaultUpdateConfig
  :: Text -- ^ repo owner
  -> Text -- ^ repo name
  -> Text -- ^ binary name
  -> Text -- ^ current version
  -> UpdateConfig
defaultUpdateConfig owner name bin ver =
  UpdateConfig
    { repoOwner = owner
    , repoName = name
    , binName = bin
    , currentVersion = ver
    , targetTriple = Nothing
    , authToken = Nothing
    }

-- | A GitHub release
data Release = Release
  { version :: Text
  -- ^ Version tag (e.g., "v1.0.0")
  , assets :: [ReleaseAsset]
  -- ^ Available release assets
  , htmlUrl :: Text
  -- ^ URL to the release page
  , body :: Maybe Text
  -- ^ Release notes/description
  }
  deriving stock (Show, Eq)

-- | A downloadable asset in a release
data ReleaseAsset = ReleaseAsset
  { name :: Text
  -- ^ Asset filename (e.g., "myapp-linux-x86_64.tar.gz")
  , downloadUrl :: Text
  -- ^ URL to download the asset
  , size :: Int
  -- ^ File size in bytes
  , contentType :: Text
  -- ^ MIME type of the asset
  }
  deriving stock (Show, Eq)

-- | Result of checking for updates
data UpdateStatus
  = -- | Current version is the latest
    UpToDate
  | -- | A newer version is available
    UpdateAvailable Release
  deriving stock (Show, Eq)

-- | Result of performing an update
data UpdateResult
  = -- | Update completed successfully
    UpdateSuccess
      { newVersion :: Text
      -- ^ The version that was installed
      }
  | -- | No update was needed
    NoUpdateNeeded
  deriving stock (Show, Eq)

-- | Errors that can occur during update operations
data UpdateError
  = -- | Network request failed
    NetworkError Text
  | -- | Failed to parse response from GitHub API
    ParseError Text
  | -- | No matching asset found for the current platform
    AssetNotFound Text
  | -- | Failed to extract the downloaded archive
    ExtractError Text
  | -- | Failed to replace the current executable
    ReplaceError Text
  | -- | Version comparison error
    VersionError Text
  | -- | Failed to detect current executable path
    ExecutablePathError Text
  deriving stock (Show, Eq)
