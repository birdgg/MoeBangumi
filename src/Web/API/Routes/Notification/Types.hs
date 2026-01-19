{- | Notification API types

This module provides types for the notification API endpoints.
-}
module Web.API.Routes.Notification.Types
  ( -- * Request Types
    TestNotificationRequest (..)
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Data.OpenApi (ToSchema)

--------------------------------------------------------------------------------
-- Request Types
--------------------------------------------------------------------------------

-- | Request body for testing Telegram notification
data TestNotificationRequest = TestNotificationRequest
  { botToken :: Text
  -- ^ Telegram Bot API token
  , chatId :: Text
  -- ^ Telegram chat ID
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToSchema)

instance FromJSON TestNotificationRequest where
  parseJSON = withObject "TestNotificationRequest" $ \o ->
    TestNotificationRequest
      <$> o .: "bot_token"
      <*> o .: "chat_id"

instance ToJSON TestNotificationRequest where
  toJSON req =
    object
      [ "bot_token" .= req.botToken
      , "chat_id" .= req.chatId
      ]
