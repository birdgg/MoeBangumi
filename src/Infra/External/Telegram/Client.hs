{- | Telegram Bot API client using servant-client

This module provides ClientM functions for interacting with the Telegram Bot API.

== Usage

@
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Infra.External.Telegram.Client
import Servant.Client (mkClientEnv, runClientM)

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  let env = mkClientEnv manager (mkTelegramBaseUrl "your-bot-token")
  result <- runClientM (sendTextMessage "chat-id" "Hello!") env
  case result of
    Right resp -> print resp
    Left err -> print err
@
-}
module Infra.External.Telegram.Client
  ( -- * API Functions
    sendMessage
  , sendTextMessage
  , sendPhoto
  , sendPhotoWithCaption

    -- * Re-exports
  , module Infra.External.Telegram.Types
  , mkTelegramBaseUrl
  )
where

import Infra.External.Telegram.API (TelegramRoutes, mkTelegramBaseUrl)
import Infra.External.Telegram.API qualified as API
import Infra.External.Telegram.Types
import Servant.Client (ClientM)
import Servant.Client.Generic (AsClientT, genericClient)

-- | Generated client functions record (internal)
client' :: TelegramRoutes (AsClientT ClientM)
client' = genericClient

-- | Send a message with full control over request
sendMessage :: SendMessageRequest -> ClientM TelegramResponse
sendMessage = API.sendMessage client'

-- | Send a plain text message (convenience method)
sendTextMessage :: Text -> Text -> ClientM TelegramResponse
sendTextMessage chatId text =
  sendMessage
    SendMessageRequest
      { chatId = chatId
      , text = text
      , parseMode = Just "HTML"
      }

-- | Send a photo with full control over request
sendPhoto :: SendPhotoRequest -> ClientM TelegramResponse
sendPhoto = API.sendPhoto client'

-- | Send a photo with caption (convenience method)
sendPhotoWithCaption :: Text -> Text -> Text -> ClientM TelegramResponse
sendPhotoWithCaption chatId photoUrl caption =
  sendPhoto
    SendPhotoRequest
      { chatId = chatId
      , photo = photoUrl
      , caption = Just caption
      , parseMode = Just "HTML"
      }
