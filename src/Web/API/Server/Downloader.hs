{- | Downloader API handlers

Implements the downloader test API endpoint.
-}
module Web.API.Server.Downloader
  ( downloaderServer
  )
where

import Effectful (IOE, (:>))
import Effectful.Error.Static (Error)
import Effectful.Reader.Static qualified as Reader
import Network.HTTP.Client (Manager)
import Servant hiding ((:>))
import Servant.Client (ClientError (..), ResponseF (..), mkClientEnv, runClientM)

import Infra.Environment.Env (MoeEnv (..))
import Infra.External.QBittorrent.Client qualified as Client
import Moe.Monad (MoeM)
import Moe.Setting (DownloaderSettings)
import RequireCallStack
import Web.API.Routes.Downloader qualified as DownloaderRoutes
import Web.Types (MoeEff)

-- | Downloader API server implementation
downloaderServer :: (RequireCallStack) => ServerT DownloaderRoutes.API MoeEff
downloaderServer =
  DownloaderRoutes.Routes'
    { test = testHandler
    }

-- | Test downloader connection
testHandler ::
  ( Reader.Reader MoeEnv :> es
  , Error ServerError :> es
  , IOE :> es
  ) =>
  DownloaderRoutes.TestDownloaderRequest ->
  MoeM es DownloaderRoutes.TestDownloaderResponse
testHandler req = do
  env <- Reader.ask @MoeEnv
  liftIO $ testConnection env.httpManager req.settings

-- | Test qBittorrent connection by attempting to login
testConnection :: Manager -> DownloaderSettings -> IO DownloaderRoutes.TestDownloaderResponse
testConnection manager settings = do
  let clientEnv = mkClientEnv manager (Client.mkQBittorrentBaseUrl settings)
  loginResult <- runClientM (Client.login settings) clientEnv
  pure $ case loginResult of
    Right resp
      | resp == "Ok." ->
          DownloaderRoutes.TestDownloaderResponse
            { success = True
            , message = "连接成功"
            }
      | otherwise ->
          DownloaderRoutes.TestDownloaderResponse
            { success = False
            , message = "认证失败：用户名或密码错误"
            }
    Left err ->
      DownloaderRoutes.TestDownloaderResponse
        { success = False
        , message = clientErrorToMessage err
        }

-- | Convert ClientError to user-friendly message
clientErrorToMessage :: ClientError -> Text
clientErrorToMessage = \case
  ConnectionError _ -> "无法连接到服务器，请检查地址是否正确"
  FailureResponse _ resp -> decodeResponseBody resp.responseBody
  DecodeFailure _ _ -> "响应解析失败"
  UnsupportedContentType _ _ -> "不支持的响应格式"
  InvalidContentTypeHeader _ -> "无效的响应头"

-- | Decode response body to text
decodeResponseBody :: LByteString -> Text
decodeResponseBody body =
  case decodeUtf8' (toStrict body) of
    Right txt -> txt
    Left _ -> "服务器返回错误"
