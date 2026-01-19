{- | Settings API route definitions

Provides REST API for managing application settings:
- GET /api/settings - Get current settings
- PUT /api/settings - Update settings
-}
module Web.API.Routes.Settings where

import Servant

import Moe.Setting (Settings)

type API = NamedRoutes Routes'

data Routes' mode = Routes'
  { getSettings
      :: mode
        :- Summary "Get current settings"
          :> Description "Retrieve the current application settings"
          :> Get '[JSON] Settings
  , updateSettings
      :: mode
        :- Summary "Update settings"
          :> Description "Update the application settings (full replacement)"
          :> ReqBody '[JSON] Settings
          :> Put '[JSON] Settings
  }
  deriving stock (Generic)
