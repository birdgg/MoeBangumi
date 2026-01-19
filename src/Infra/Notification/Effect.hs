{-# LANGUAGE TemplateHaskell #-}

{- | Notification Effect - Abstract interface for sending notifications

This module provides a notification-agnostic effectful interface.
Currently supports Telegram, but can be extended to other channels.

= Supported Operations

- Send text notifications
- Send notifications with images

= Usage

@
import Infra.Notification.Effect

notify :: (Notification :> es) => Text -> Eff es ()
notify message = do
  result <- sendNotification message Nothing
  case result of
    Right () -> pure ()
    Left err -> liftIO $ print (display err)
@
-}
module Infra.Notification.Effect
  ( -- * Effect
    Notification (..)

    -- * Operations
  , sendNotification

    -- * Re-exports
  , module Infra.Notification.Types
  )
where

import Effectful
import Effectful.TH
import Infra.Notification.Types

-- | Notification effect for sending notifications
--
-- All operations return @Either NotificationError ()@ for consistent error handling.
-- If notification is disabled (no valid config), returns @Left NotificationDisabled@.
data Notification :: Effect where
  -- | Send a notification message
  SendNotification
    :: Text
    -- ^ Message text
    -> Maybe Text
    -- ^ Optional image URL
    -> Notification m (Either NotificationError ())

type instance DispatchOf Notification = Dynamic

makeEffect ''Notification
