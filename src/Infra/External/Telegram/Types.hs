{- | Telegram Bot API types

This module provides types for interacting with the Telegram Bot API.
-}
module Infra.External.Telegram.Types
  ( -- * Request Types
    SendMessageRequest (..)
  , SendPhotoRequest (..)

    -- * Response Types
  , TelegramResponse (..)
  , TelegramResult (..)

    -- * Error Types
  , TelegramError (..)
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.:?), (.=))
import Data.Text.Display (Display (..))

--------------------------------------------------------------------------------
-- Request Types
--------------------------------------------------------------------------------

-- | Request body for sending a text message
data SendMessageRequest = SendMessageRequest
  { chatId :: Text
  -- ^ Chat ID to send the message to
  , text :: Text
  -- ^ Message text (supports HTML formatting)
  , parseMode :: Maybe Text
  -- ^ Parse mode for formatting (e.g., "HTML", "Markdown")
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON SendMessageRequest where
  toJSON req =
    object $
      [ "chat_id" .= req.chatId
      , "text" .= req.text
      ]
        <> maybe [] (\pm -> ["parse_mode" .= pm]) req.parseMode

-- | Request body for sending a photo with caption
data SendPhotoRequest = SendPhotoRequest
  { chatId :: Text
  -- ^ Chat ID to send the photo to
  , photo :: Text
  -- ^ Photo URL
  , caption :: Maybe Text
  -- ^ Optional caption for the photo
  , parseMode :: Maybe Text
  -- ^ Parse mode for caption formatting
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON SendPhotoRequest where
  toJSON req =
    object $
      [ "chat_id" .= req.chatId
      , "photo" .= req.photo
      ]
        <> maybe [] (\c -> ["caption" .= c]) req.caption
        <> maybe [] (\pm -> ["parse_mode" .= pm]) req.parseMode

--------------------------------------------------------------------------------
-- Response Types
--------------------------------------------------------------------------------

-- | Generic Telegram API response
data TelegramResponse = TelegramResponse
  { ok :: Bool
  , description :: Maybe Text
  , errorCode :: Maybe Int
  -- ^ Error code (present when ok = false)
  , result :: Maybe TelegramResult
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON TelegramResponse where
  parseJSON = withObject "TelegramResponse" $ \o ->
    TelegramResponse
      <$> o .: "ok"
      <*> o .:? "description"
      <*> o .:? "error_code"
      <*> o .:? "result"

-- | Simplified result from Telegram API (we only care if it succeeded)
data TelegramResult = TelegramResult
  { messageId :: Int64
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON TelegramResult where
  parseJSON = withObject "TelegramResult" $ \o ->
    TelegramResult <$> o .: "message_id"

--------------------------------------------------------------------------------
-- Error Types
--------------------------------------------------------------------------------

-- | Errors that can occur when interacting with Telegram API
data TelegramError
  = TelegramNetworkError Text
  -- ^ Network error (connection failed, timeout, etc.)
  | TelegramApiError Int Text
  -- ^ API error with status code and description
  | TelegramParseError Text
  -- ^ Failed to parse response
  | TelegramConfigError Text
  -- ^ Configuration error (missing token, invalid chat ID, etc.)
  deriving stock (Show, Eq)

instance Display TelegramError where
  displayBuilder = \case
    TelegramNetworkError msg -> "Telegram network error: " <> displayBuilder msg
    TelegramApiError code msg ->
      "Telegram API error (code " <> displayBuilder code <> "): " <> displayBuilder msg
    TelegramParseError msg -> "Telegram parse error: " <> displayBuilder msg
    TelegramConfigError msg -> "Telegram config error: " <> displayBuilder msg
