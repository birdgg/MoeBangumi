-- | Self-update client
--
-- This module provides pure IO functions for self-updating from GitHub releases.
module Infra.External.SelfUpdate.Client
  ( -- * Update Operations
    checkUpdate
  , performUpdate

    -- * Re-exports
  , module Infra.External.SelfUpdate.Types
  , module Infra.External.SelfUpdate.GitHub
  , module Infra.External.SelfUpdate.Archive
  )
where

import Control.Exception (catch)
import GHC.IO.Exception (IOError)
import Data.Text qualified as T
import Data.Text.Read qualified as T
import Network.HTTP.Client (Manager)
import Infra.External.SelfUpdate.Archive qualified as Archive
import Infra.External.SelfUpdate.GitHub qualified as GitHub
import Infra.External.SelfUpdate.Types
import Infra.External.SelfUpdate.GitHub (fetchReleases, fetchLatestRelease, downloadAsset, findMatchingAsset, getTargetTriple)
import Infra.External.SelfUpdate.Archive (extractTarGz, extractBinary, replaceBinary, getCurrentExecutable)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.IO.Temp (getCanonicalTemporaryDirectory)

-- | Check for updates
checkUpdate :: Manager -> UpdateConfig -> IO (Either UpdateError UpdateStatus)
checkUpdate manager config = do
  result <- GitHub.fetchLatestRelease manager config
  case result of
    Left err -> pure $ Left err
    Right release ->
      if isNewerVersion config.currentVersion release.version
        then pure $ Right $ UpdateAvailable release
        else pure $ Right UpToDate

-- | Perform update
performUpdate :: Manager -> UpdateConfig -> IO (Either UpdateError UpdateResult)
performUpdate manager config = do
  releaseResult <- GitHub.fetchLatestRelease manager config
  case releaseResult of
    Left err -> pure $ Left err
    Right release -> do
      if not (isNewerVersion config.currentVersion release.version)
        then pure $ Right NoUpdateNeeded
        else do
          case GitHub.findMatchingAsset config release of
            Left err -> pure $ Left err
            Right asset -> performUpdateWithAsset manager config release asset

-- | Perform update with a specific asset
performUpdateWithAsset
  :: Manager
  -> UpdateConfig
  -> Release
  -> ReleaseAsset
  -> IO (Either UpdateError UpdateResult)
performUpdateWithAsset manager config release asset = do
  execResult <- Archive.getCurrentExecutable
  case execResult of
    Left err -> pure $ Left err
    Right currentExe -> do
      tmpBase <- getCanonicalTemporaryDirectory
      let tmpDir = tmpBase </> ("self-update-" <> T.unpack release.version)
      createDirectoryIfMissing True tmpDir

      let archivePath = tmpDir </> T.unpack asset.name
      downloadResult <- GitHub.downloadAsset manager config asset archivePath
      case downloadResult of
        Left err -> do
          cleanup tmpDir
          pure $ Left err
        Right () -> do
          extractResult <- Archive.extractBinary archivePath config.binName tmpDir
          case extractResult of
            Left err -> do
              cleanup tmpDir
              pure $ Left err
            Right newBinaryPath -> do
              replaceResult <- Archive.replaceBinary newBinaryPath currentExe
              cleanup tmpDir
              case replaceResult of
                Left err -> pure $ Left err
                Right () -> pure $ Right $ UpdateSuccess release.version

-- | Cleanup temporary directory
cleanup :: FilePath -> IO ()
cleanup dir = removeDirectoryRecursive dir `catch` handler
  where
    handler :: IOError -> IO ()
    handler _ = pure ()

-- | Compare versions to determine if an update is available
isNewerVersion :: Text -> Text -> Bool
isNewerVersion current new =
  compareVersions (parseVersion new) (parseVersion current) == GT

-- | Parse a version string into numeric components
parseVersion :: Text -> [Int]
parseVersion v =
  let stripped = fromMaybe v (T.stripPrefix "v" v)
      parts = T.splitOn "." stripped
  in  map parseNum parts

-- | Parse a single version component to Int
parseNum :: Text -> Int
parseNum t = case T.decimal t of
  Right (n, _) -> n
  Left _ -> 0

-- | Compare version lists lexicographically
compareVersions :: [Int] -> [Int] -> Ordering
compareVersions [] [] = EQ
compareVersions [] _ = LT
compareVersions _ [] = GT
compareVersions (x : xs) (y : ys)
  | x == y = compareVersions xs ys
  | otherwise = compare x y
