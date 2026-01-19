{- | Telegram Bot API definition for servant-client

This module provides the Servant API type definitions for the Telegram Bot API.
-}
module Infra.External.Telegram.API
  ( -- * API Types
    TelegramAPI
  , TelegramRoutes (..)

    -- * Configuration
  , mkTelegramBaseUrl
  )
where

import Infra.External.Telegram.Types
import Servant.API
import Servant.Client (BaseUrl (..), Scheme (..))

-- | Telegram Bot API routes
type TelegramAPI = NamedRoutes TelegramRoutes

-- | Telegram Bot API routes using NamedRoutes pattern
data TelegramRoutes mode = TelegramRoutes
  { sendMessage
      :: mode
        :- "sendMessage"
          :> ReqBody '[JSON] SendMessageRequest
          :> Post '[JSON] TelegramResponse
  , sendPhoto
      :: mode
        :- "sendPhoto"
          :> ReqBody '[JSON] SendPhotoRequest
          :> Post '[JSON] TelegramResponse
  }
  deriving stock (Generic)

-- | Create base URL for Telegram Bot API with token
--
-- The URL is: https://api.telegram.org/bot{token}
mkTelegramBaseUrl :: Text -> BaseUrl
mkTelegramBaseUrl token =
  BaseUrl
    { baseUrlScheme = Https
    , baseUrlHost = "api.telegram.org"
    , baseUrlPort = 443
    , baseUrlPath = "/bot" <> toString token
    }
