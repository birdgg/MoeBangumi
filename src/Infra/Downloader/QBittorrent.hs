{- | QBittorrent implementation of Downloader effect

This module provides automatic session management:

- Login is performed lazily on first operation
- Session cookies are maintained via servant-client's built-in CookieJar (TVar)
- On auth failure, auto-retry with fresh login

= Cookie Handling

servant-client supports cookie persistence via the 'cookieJar' field in 'ClientEnv'.
When set to @Just (TVar CookieJar)@, cookies are automatically:

1. Inserted into requests from the shared jar
2. Extracted from responses and stored back

This is essential for qBittorrent authentication, which uses session cookies (SID).

= Usage

@
import Infra.Downloader.Effect
import Infra.Downloader.QBittorrent

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  settings <- getDownloaderSettings
  runEff
    . runDownloader manager settings
    $ do
      result <- addTorrent (MagnetLink "magnet:?...") defaultAddOptions
      case result of
        Right () -> liftIO $ putStrLn "Success"
        Left err -> liftIO $ print (display err)
@
-}
module Infra.Downloader.QBittorrent (
  runDownloader,
)
where

import Control.Retry (RetryPolicy, RetryStatus, exponentialBackoff, limitRetries)
import Data.Text.Display (display)
import Data.Text qualified as T
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Retry (Retry, retrying, runRetry)
import Network.HTTP.Client (Manager)
import Network.HTTP.Types.Status (statusCode)
import Servant.Client (ClientEnv (..), ClientError (..), mkClientEnv, runClientM)
import Servant.Client qualified as Servant

import Infra.Downloader.Effect (Downloader (..))
import Infra.Downloader.Types hiding (ConnectionError)
import Infra.Downloader.Types qualified as DT
import Infra.External.QBittorrent.Client qualified as Client
import Infra.External.QBittorrent.Types qualified as QBTypes
import Moe.Setting (DownloaderSettings (..))

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

-- | Temporary download path to prevent Plex from scanning incomplete files
tempDownloadPath :: Text
tempDownloadPath = "/data/downloads/moe-staging"

--------------------------------------------------------------------------------
-- Session State
--------------------------------------------------------------------------------

-- | Session state tracking for auto-auth
--
-- Note: Cookie persistence is handled by servant-client's ClientEnv.cookieJar
-- field (TVar CookieJar). This state only tracks whether we've attempted login.
newtype SessionState = SessionState
  { isLoggedIn :: Bool
  }

newSessionState :: SessionState
newSessionState = SessionState{isLoggedIn = False}

--------------------------------------------------------------------------------
-- Retry Policy
--------------------------------------------------------------------------------

{- | Retry policy for connection errors

Exponential backoff starting at 500ms, up to 3 retries.
Total max wait: 500ms + 1000ms + 2000ms = 3.5s
-}
connectionRetryPolicy :: RetryPolicy
connectionRetryPolicy = exponentialBackoff 500_000 <> limitRetries 3

--------------------------------------------------------------------------------
-- Handler
--------------------------------------------------------------------------------

{- | Run Downloader effect using QBittorrent as backend

Session management is handled automatically:

- First operation triggers login
- Session cookies (SID) are persisted via TVar CookieJar
- Subsequent operations reuse the session cookie
- Auth failures trigger auto re-login
-}
runDownloader ::
  (IOE :> es) =>
  Manager ->
  DownloaderSettings ->
  Eff (Downloader : es) a ->
  Eff es a
runDownloader manager settings action = do
  -- Create a TVar CookieJar for cookie persistence across requests
  -- This is essential for qBittorrent auth which uses SID session cookies
  cookieJarVar <- liftIO $ newTVarIO mempty
  let baseEnv = mkClientEnv manager (Client.mkQBittorrentBaseUrl settings)
      -- Enable cookie handling by setting the cookieJar field
      clientEnv = baseEnv{cookieJar = Just cookieJarVar}
  sessionRef <- liftIO $ newIORef newSessionState
  runRetry $
    interpret (handleDownloader clientEnv settings sessionRef) $
      inject action

-- | Effect handler with auto-auth and retry
handleDownloader ::
  (Retry :> es, IOE :> es) =>
  ClientEnv ->
  DownloaderSettings ->
  IORef SessionState ->
  EffectHandler Downloader es
handleDownloader env settings sessionRef _ = \case
  AddTorrent source opts ->
    withAuth env settings sessionRef $ addTorrentImpl env source opts
  GetTorrents mFilter ->
    withAuth env settings sessionRef $ getTorrentsImpl env mFilter
  GetTorrentFiles hash ->
    withAuth env settings sessionRef $ getTorrentFilesImpl env hash
  BatchRenameFiles ops ->
    withAuth env settings sessionRef $ batchRenameImpl env ops
  PauseTorrents hashes ->
    withAuth env settings sessionRef $ pauseTorrentsImpl env hashes
  ResumeTorrents hashes ->
    withAuth env settings sessionRef $ resumeTorrentsImpl env hashes
  DeleteTorrents hashes deleteFiles ->
    withAuth env settings sessionRef $ deleteTorrentsImpl env hashes deleteFiles
  RemoveTags hashes tags ->
    withAuth env settings sessionRef $ removeTagsImpl env hashes tags

--------------------------------------------------------------------------------
-- Auto-Auth Wrapper
--------------------------------------------------------------------------------

{- | Execute operation with automatic authentication and retry

Strategy:
1. Check if logged in, if not -> login
2. Execute operation with retry for ConnectionError
3. If auth error -> retry once after re-login (handled separately)
-}
withAuth ::
  (Retry :> es, IOE :> es) =>
  ClientEnv ->
  DownloaderSettings ->
  IORef SessionState ->
  IO (Either DownloaderError a) ->
  Eff es (Either DownloaderError a)
withAuth env settings sessionRef operation = do
  -- Ensure we're logged in
  loginResult <- ensureLoggedIn env settings sessionRef
  case loginResult of
    Left err -> pure $ Left err
    Right () -> do
      -- Execute with retry for connection errors
      result <- retrying connectionRetryPolicy shouldRetry (const $ liftIO operation)
      -- Handle auth error separately (not retried by policy)
      case result of
        Left (AuthenticationError _) -> do
          retryResult <- doLogin env settings sessionRef
          case retryResult of
            Left err -> pure $ Left err
            Right () -> liftIO operation -- Single retry after re-auth
        _ -> pure result
 where
  -- Only retry on ConnectionError (from Infra.Downloader.Types)
  shouldRetry :: RetryStatus -> Either DownloaderError a -> Eff es Bool
  shouldRetry _ (Left (DT.ConnectionError _)) = pure True
  shouldRetry _ _ = pure False

-- | Ensure we're logged in, login if not
ensureLoggedIn ::
  (IOE :> es) =>
  ClientEnv ->
  DownloaderSettings ->
  IORef SessionState ->
  Eff es (Either DownloaderError ())
ensureLoggedIn env settings sessionRef = do
  session <- liftIO $ readIORef sessionRef
  if session.isLoggedIn
    then pure $ Right ()
    else doLogin env settings sessionRef

-- | Perform login and update session state
doLogin ::
  (IOE :> es) =>
  ClientEnv ->
  DownloaderSettings ->
  IORef SessionState ->
  Eff es (Either DownloaderError ())
doLogin env settings sessionRef = do
  loginResult <- liftIO $ runClientM (Client.login settings) env
  case loginResult of
    Right resp
      | resp == "Ok." -> do
          liftIO $ modifyIORef' sessionRef (\s -> s{isLoggedIn = True})
          pure $ Right ()
      | otherwise ->
          pure $ Left $ AuthenticationError $ "Login failed: " <> resp
    Left err ->
      pure $ Left $ AuthenticationError $ "Login failed: " <> show err

--------------------------------------------------------------------------------
-- Implementation Functions
--------------------------------------------------------------------------------

-- | Add torrent implementation
addTorrentImpl ::
  ClientEnv ->
  TorrentSource ->
  AddTorrentOptions ->
  IO (Either DownloaderError ())
addTorrentImpl env source opts = do
  case source of
    MagnetLink url -> do
      let qbReq =
            QBTypes.AddTorrentRequest
              { urls = Just url
              , savepath = Just tempDownloadPath
              , category = opts.category
              , tags = tagsToText [RenameTag, MoeTag]
              , rename = opts.rename
              }
      result <- runClientM (Client.addTorrent qbReq) env
      pure $ first clientErrorToDownloaderError $ void result
    TorrentFileContent _ ->
      -- TODO: Support .torrent files (requires multipart form support in servant)
      pure $ Left $ InvalidInput "TorrentFileContent not yet supported, use MagnetLink instead"

-- | Get torrents implementation
getTorrentsImpl ::
  ClientEnv ->
  Maybe TorrentStatusFilter ->
  IO (Either DownloaderError [SimpleTorrentInfo])
getTorrentsImpl env mFilter = do
  let qbFilter = fmap filterToQBFilter mFilter
      req =
        Just $
          QBTypes.TorrentInfoRequest
            { filter = qbFilter
            , category = Nothing
            , tag = Nothing
            , hashes = Nothing
            }
  result <- runClientM (Client.getTorrentsInfo req) env
  pure $ bimap clientErrorToDownloaderError (map toSimpleTorrentInfo) result

-- | Get torrent files implementation
getTorrentFilesImpl ::
  ClientEnv ->
  Text ->
  IO (Either DownloaderError [SimpleTorrentFile])
getTorrentFilesImpl env hash = do
  result <- runClientM (Client.getTorrentFiles hash) env
  pure $ bimap clientErrorToDownloaderError (map toSimpleTorrentFile) result

-- | Batch rename implementation
batchRenameImpl ::
  ClientEnv ->
  [RenameOperation] ->
  IO (Either DownloaderError ())
batchRenameImpl env ops = do
  -- Execute renames sequentially (qBittorrent doesn't support batch)
  results <- traverse renameSingle ops
  -- Return first error if any, otherwise success
  case partitionEithers results of
    ([], _) -> pure $ Right ()
    (firstErr : _, _) -> pure $ Left firstErr
 where
  renameSingle op = do
    result <- runClientM (Client.renameFile op.torrentHash op.oldPath op.newPath) env
    pure $ first clientErrorToDownloaderError $ void result

-- | Pause torrents implementation
pauseTorrentsImpl ::
  ClientEnv ->
  [Text] ->
  IO (Either DownloaderError ())
pauseTorrentsImpl env hashes = do
  result <- runClientM (Client.pauseTorrents hashes) env
  pure $ first clientErrorToDownloaderError $ void result

-- | Resume torrents implementation
resumeTorrentsImpl ::
  ClientEnv ->
  [Text] ->
  IO (Either DownloaderError ())
resumeTorrentsImpl env hashes = do
  result <- runClientM (Client.resumeTorrents hashes) env
  pure $ first clientErrorToDownloaderError $ void result

-- | Delete torrents implementation
deleteTorrentsImpl ::
  ClientEnv ->
  [Text] ->
  Bool ->
  IO (Either DownloaderError ())
deleteTorrentsImpl env hashes deleteFiles = do
  result <- runClientM (Client.deleteTorrents hashes deleteFiles) env
  pure $ first clientErrorToDownloaderError $ void result

-- | Remove tags implementation
removeTagsImpl ::
  ClientEnv ->
  [Text] ->
  [Text] ->
  IO (Either DownloaderError ())
removeTagsImpl env hashes tags = do
  result <- runClientM (Client.removeTags hashes tags) env
  pure $ first clientErrorToDownloaderError $ void result

--------------------------------------------------------------------------------
-- Conversion Functions
--------------------------------------------------------------------------------

-- | Convert tags list to comma-separated text for qBittorrent API
tagsToText :: [TorrentTag] -> Maybe Text
tagsToText [] = Nothing
tagsToText ts = Just $ T.intercalate "," $ map display ts

-- | Convert generic filter to QBittorrent filter
filterToQBFilter :: TorrentStatusFilter -> QBTypes.TorrentFilter
filterToQBFilter = \case
  FilterAll -> QBTypes.FilterAll
  FilterDownloading -> QBTypes.FilterDownloading
  FilterCompleted -> QBTypes.FilterCompleted
  FilterPaused -> QBTypes.FilterStopped

-- | Convert QBittorrent torrent info to simplified version
toSimpleTorrentInfo :: QBTypes.TorrentInfo -> SimpleTorrentInfo
toSimpleTorrentInfo t =
  SimpleTorrentInfo
    { hash = t.hash
    , name = t.name
    , state = t.state
    , progress = t.progress
    , savePath = t.savePath
    , tags = if T.null t.tags then [] else T.splitOn "," t.tags
    }

-- | Convert QBittorrent file info to simplified version
toSimpleTorrentFile :: QBTypes.TorrentFile -> SimpleTorrentFile
toSimpleTorrentFile f =
  SimpleTorrentFile
    { index = f.index
    , name = f.name
    , size = f.size
    , progress = f.progress
    }

-- | Convert ClientError to DownloaderError
clientErrorToDownloaderError :: ClientError -> DownloaderError
clientErrorToDownloaderError = \case
  FailureResponse _req resp ->
    let code = statusCode (Servant.responseStatusCode resp)
     in if code == 401 || code == 403
          then AuthenticationError "Unauthorized"
          else
            if code == 404
              then TorrentNotFound "Resource not found"
              else ClientError $ "HTTP " <> show code
  DecodeFailure msg _ ->
    ClientError $ "Decode error: " <> toText msg
  UnsupportedContentType _ _ ->
    ClientError "Unsupported content type"
  InvalidContentTypeHeader _ ->
    ClientError "Invalid content type header"
  Servant.ConnectionError exc ->
    DT.ConnectionError $ show exc
