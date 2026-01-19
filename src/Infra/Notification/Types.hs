{- | Notification types

This module provides types for the notification effect.
-}
module Infra.Notification.Types
  ( -- * Error Types
    NotificationError (..)
  )
where

import Data.Text.Display (Display (..))
import Infra.External.Telegram.Types (TelegramError)

--------------------------------------------------------------------------------
-- Error Types
--------------------------------------------------------------------------------

-- | Errors that can occur when sending notifications
data NotificationError
  = NotificationDisabled
  -- ^ Notification is disabled (no valid configuration)
  | NotificationTelegramError TelegramError
  -- ^ Error from Telegram API
  deriving stock (Show, Eq)

instance Display NotificationError where
  displayBuilder = \case
    NotificationDisabled -> "Notification disabled"
    NotificationTelegramError err -> "Telegram error: " <> displayBuilder err
