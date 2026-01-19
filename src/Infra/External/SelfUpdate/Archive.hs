-- | Archive extraction for self-update functionality
module Infra.External.SelfUpdate.Archive
  ( -- * Extraction
    extractTarGz
  , extractBinary

    -- * Binary Replacement
  , replaceBinary
  , getCurrentExecutable
  )
where

import Codec.Archive.Tar qualified as Tar
import Codec.Compression.GZip qualified as GZip
import Control.Exception (try)
import Data.ByteString.Lazy qualified as LBS
import GHC.IO.Exception (IOError)
import Data.Text qualified as T
import Infra.External.SelfUpdate.Types
import System.Directory
  ( copyFile
  , doesFileExist
  , getPermissions
  , removeFile
  , renameFile
  , setOwnerExecutable
  , setPermissions
  )
import System.Environment (getExecutablePath)
import System.FilePath (takeFileName, (</>))
import System.Posix.Files (setFileMode)

-- | Extract a tar.gz archive to a directory
extractTarGz :: FilePath -> FilePath -> IO (Either UpdateError ())
extractTarGz archivePath destDir = do
  result <- try $ do
    compressed <- LBS.readFile archivePath
    let decompressed = GZip.decompress compressed
        entries = Tar.read decompressed
    Tar.unpack destDir entries
  case result of
    Left (e :: IOError) ->
      pure $ Left $ ExtractError $ T.pack $ show e
    Right () -> pure $ Right ()

-- | Extract a specific binary from a tar.gz archive
extractBinary :: FilePath -> Text -> FilePath -> IO (Either UpdateError FilePath)
extractBinary archivePath binName destDir = do
  result <- try $ do
    compressed <- LBS.readFile archivePath
    let decompressed = GZip.decompress compressed
        entries = Tar.read decompressed
    extractBinaryFromEntries entries binName destDir
  case result of
    Left (e :: IOError) ->
      pure $ Left $ ExtractError $ T.pack $ show e
    Right r -> pure r

-- | Extract binary from tar entries
extractBinaryFromEntries
  :: Tar.Entries Tar.FormatError
  -> Text
  -> FilePath
  -> IO (Either UpdateError FilePath)
extractBinaryFromEntries entries binName destDir =
  go entries
  where
    binNameStr = T.unpack binName
    go Tar.Done = pure $ Left $ ExtractError $ "Binary not found in archive: " <> binName
    go (Tar.Fail err) = pure $ Left $ ExtractError $ T.pack $ show err
    go (Tar.Next entry rest) = do
      let entryName = takeFileName $ Tar.entryPath entry
      if entryName == binNameStr
        then case Tar.entryContent entry of
          Tar.NormalFile content _size -> do
            let destPath = destDir </> binNameStr
            LBS.writeFile destPath content
            setFileMode destPath 0o755
            pure $ Right destPath
          _ -> go rest
        else go rest

-- | Replace the current executable with a new one
replaceBinary :: FilePath -> FilePath -> IO (Either UpdateError ())
replaceBinary newBinaryPath currentPath = do
  let backupPath = currentPath <> ".backup"
  result <- try $ do
    exists <- doesFileExist newBinaryPath
    if not exists
      then pure $ Left $ ReplaceError "New binary does not exist"
      else do
        copyFile currentPath backupPath
        removeFile currentPath
        renameFile newBinaryPath currentPath
        perms <- getPermissions currentPath
        setPermissions currentPath (setOwnerExecutable True perms)
        removeFile backupPath
        pure $ Right ()
  case result of
    Left (e :: IOError) -> do
      backupExists <- doesFileExist backupPath
      if backupExists
        then do
          _ <- try @IOError $ renameFile backupPath currentPath
          pure ()
        else pure ()
      pure $ Left $ ReplaceError $ T.pack $ show e
    Right r -> pure r

-- | Get the path to the current executable
getCurrentExecutable :: IO (Either UpdateError FilePath)
getCurrentExecutable = do
  result <- try getExecutablePath
  case result of
    Left (e :: IOError) ->
      pure $ Left $ ExecutablePathError $ T.pack $ show e
    Right path -> pure $ Right path
