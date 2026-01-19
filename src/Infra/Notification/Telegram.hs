{- | Telegram implementation of Notification effect

This module provides the Telegram-based handler for the Notification effect.
If Telegram is not configured (empty botToken or chatId), notifications are disabled.

= Usage

@
import Infra.Notification.Effect
import Infra.Notification.Telegram

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  settings <- getSettings
  runEff
    . runNotification manager settings
    $ do
      result <- sendNotification "Hello World!" Nothing
      case result of
        Right () -> liftIO $ putStrLn "Sent!"
        Left err -> liftIO $ print (display err)
@
-}
module Infra.Notification.Telegram
  ( runNotification
  )
where

import Data.Text qualified as T
import Effectful
import Effectful.Dispatch.Dynamic
import Network.HTTP.Client (Manager)
import Network.HTTP.Types.Status (statusCode)
import Servant.Client (ClientEnv, ClientError (..), mkClientEnv, runClientM)
import Servant.Client qualified as Servant

import Infra.External.Telegram.API (mkTelegramBaseUrl)
import Infra.External.Telegram.Client qualified as Telegram
import Infra.External.Telegram.Types
import Infra.Notification.Effect (Notification (..))
import Infra.Notification.Types
import Moe.Setting (TelegramConfig (..))

--------------------------------------------------------------------------------
-- Handler
--------------------------------------------------------------------------------

{- | Run Notification effect using Telegram as backend

If Telegram is not configured (empty botToken or chatId), all notifications
will return @Left NotificationDisabled@.
-}
runNotification ::
  (IOE :> es) =>
  Manager ->
  TelegramConfig ->
  Eff (Notification : es) a ->
  Eff es a
runNotification manager config
  | isEnabled config = interpret (handleTelegram clientEnv config)
  | otherwise = interpret handleDisabled
 where
  clientEnv = mkClientEnv manager (mkTelegramBaseUrl config.botToken)

-- | Check if Telegram notification is enabled
isEnabled :: TelegramConfig -> Bool
isEnabled config =
  not (T.null config.botToken) && not (T.null config.chatId)

--------------------------------------------------------------------------------
-- Handlers
--------------------------------------------------------------------------------

-- | Handler when notification is disabled
handleDisabled :: (IOE :> es) => EffectHandler Notification es
handleDisabled _ = \case
  SendNotification _ _ -> pure $ Left NotificationDisabled

-- | Handler for Telegram notifications
handleTelegram ::
  (IOE :> es) =>
  ClientEnv ->
  TelegramConfig ->
  EffectHandler Notification es
handleTelegram env config _ = \case
  SendNotification message mImageUrl ->
    liftIO $ sendTelegramNotification env config message mImageUrl

--------------------------------------------------------------------------------
-- Implementation
--------------------------------------------------------------------------------

-- | Send notification via Telegram
sendTelegramNotification ::
  ClientEnv ->
  TelegramConfig ->
  Text ->
  Maybe Text ->
  IO (Either NotificationError ())
sendTelegramNotification env config message mImageUrl = do
  result <- case mImageUrl of
    Nothing ->
      -- Send text message
      runClientM (Telegram.sendTextMessage config.chatId message) env
    Just imageUrl ->
      -- Send photo with caption
      runClientM (Telegram.sendPhotoWithCaption config.chatId imageUrl message) env

  pure $ case result of
    Right resp
      | resp.ok -> Right ()
      | otherwise ->
          Left $
            NotificationTelegramError $
              TelegramApiError
                (fromMaybe 0 resp.errorCode)
                (fromMaybe "Unknown error" resp.description)
    Left err ->
      Left $ NotificationTelegramError $ clientErrorToTelegramError err

-- | Convert ClientError to TelegramError
clientErrorToTelegramError :: ClientError -> TelegramError
clientErrorToTelegramError = \case
  FailureResponse _req resp ->
    let code = statusCode (Servant.responseStatusCode resp)
     in TelegramApiError code $ "HTTP " <> show code
  DecodeFailure msg _ ->
    TelegramParseError $ toText msg
  UnsupportedContentType _ _ ->
    TelegramParseError "Unsupported content type"
  InvalidContentTypeHeader _ ->
    TelegramParseError "Invalid content type header"
  Servant.ConnectionError exc ->
    TelegramNetworkError $ show exc
