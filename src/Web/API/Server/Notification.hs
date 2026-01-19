{- | Notification API handlers

This module provides handlers for notification-related API endpoints.
-}
module Web.API.Server.Notification
  ( notificationServer
  )
where

import Data.Text qualified as T
import Data.Text.Display (display)
import Effectful qualified
import Effectful.Error.Static (Error, throwError)
import Effectful.Reader.Static qualified as Reader
import Servant hiding (throwError)

import Infra.Environment.Env (MoeEnv (..))
import Infra.Notification.Effect (sendNotification)
import Infra.Notification.Telegram (runNotification)
import Moe.Monad (MoeM)
import Moe.Setting (TelegramConfig (..))
import RequireCallStack
import Web.API.Routes.Notification qualified as Notification
import Web.API.Routes.Notification.Types (TestNotificationRequest (..))
import Web.Types (MoeEff)

--------------------------------------------------------------------------------
-- Server
--------------------------------------------------------------------------------

-- | Notification API server implementation
notificationServer :: RequireCallStack => ServerT Notification.API MoeEff
notificationServer =
  Notification.Routes'
    { test = testNotificationHandler
    }

--------------------------------------------------------------------------------
-- Handlers
--------------------------------------------------------------------------------

-- | Test notification handler
--
-- Sends a test message via Telegram to verify configuration.
-- Uses the provided bot token and chat ID from the request body.
testNotificationHandler ::
  ( Reader.Reader MoeEnv Effectful.:> es
  , Error ServerError Effectful.:> es
  , Effectful.IOE Effectful.:> es
  ) =>
  TestNotificationRequest ->
  MoeM es NoContent
testNotificationHandler req = do
  -- Validate input
  when (T.null req.botToken) $
    throwError err400{errBody = "Bot token is required"}
  when (T.null req.chatId) $
    throwError err400{errBody = "Chat ID is required"}

  env <- Reader.ask @MoeEnv

  -- Create temporary config from request
  let tempConfig =
        TelegramConfig
          { botToken = req.botToken
          , chatId = req.chatId
          }

  -- Send test notification
  let testMessage = "🎉 <b>测试通知</b>\n\n这是来自 moe-bangumi 的测试消息，配置正确！"
  result <-
    sendNotification testMessage Nothing
      & runNotification env.httpManager tempConfig

  case result of
    Left err -> do
      let errMsg = display err
      throwError err400{errBody = encodeUtf8 errMsg}
    Right () ->
      pure NoContent
