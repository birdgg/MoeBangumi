{- | Settings API handler implementation

Implements the REST API handlers for settings management.
-}
module Web.API.Server.Settings
  ( settingsServer
  )
where

import Effectful qualified
import Effectful.Reader.Static qualified as Reader
import Servant

import Infra.Environment.Env (MoeEnv (..))
import Infra.Setting (writeSettings)
import Moe.Monad (MoeM)
import Moe.Setting (Settings)
import RequireCallStack
import Web.API.Routes.Settings qualified as Settings
import Web.Types (MoeEff)

-- | Settings API server implementation
settingsServer :: RequireCallStack => ServerT Settings.API MoeEff
settingsServer =
  Settings.Routes'
    { getSettings = getSettingsHandler
    , updateSettings = updateSettingsHandler
    }

-- | Handler for GET /api/settings
getSettingsHandler ::
  (Reader.Reader MoeEnv Effectful.:> es, Effectful.IOE Effectful.:> es) =>
  MoeM es Settings
getSettingsHandler = do
  env <- Reader.ask
  readTVarIO env.settingsVar

-- | Handler for PUT /api/settings
updateSettingsHandler ::
  (Reader.Reader MoeEnv Effectful.:> es, Effectful.IOE Effectful.:> es) =>
  Settings ->
  MoeM es Settings
updateSettingsHandler newSettings = do
  env <- Reader.ask
  -- Update TVar
  atomically $ writeTVar env.settingsVar newSettings
  -- Persist to file
  writeSettings env.dataFolder newSettings
  pure newSettings
