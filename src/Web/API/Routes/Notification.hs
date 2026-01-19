{- | Notification API routes

This module provides API routes for notification-related endpoints.
-}
module Web.API.Routes.Notification
  ( API
  , Routes' (..)
  )
where

import Servant

import Web.API.Routes.Notification.Types (TestNotificationRequest)

-- | Notification API
type API = NamedRoutes Routes'

-- | Notification routes
data Routes' mode = Routes'
  { test
      :: mode
        :- "test"
          :> Summary "Test Telegram notification by sending a test message"
          :> ReqBody '[JSON] TestNotificationRequest
          :> Post '[JSON] NoContent
  }
  deriving stock (Generic)
