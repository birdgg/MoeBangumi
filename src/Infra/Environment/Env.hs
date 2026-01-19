module Infra.Environment.Env
  ( MoeEnv (..)
  , DeploymentEnv (..)
  )
where

import Data.Pool (Pool)
import Database.SQLite.Simple qualified as SQLite
import Network.HTTP.Client (Manager)
import UnliftIO.STM (TChan)

import Infra.Environment.Config (DeploymentEnv (..), MoeConfig)
import Infra.Logging.Types (LogEntry)
import Moe.Setting (Settings)

-- | The datatype that is used in the application
data MoeEnv = MoeEnv
  { pool :: Pool SQLite.Connection
  , httpPort :: Word16
  , environment :: DeploymentEnv
  , config :: MoeConfig
  , settingsVar :: TVar Settings
  -- ^ Settings stored in TVar for hot-reload support
  , dataFolder :: FilePath
  , posterFolder :: FilePath
  , httpManager :: Manager
  -- ^ Shared HTTP Manager for all external API calls
  , logBroadcast :: TChan LogEntry
  -- ^ Broadcast channel for real-time log streaming via SSE
  }
  deriving stock (Generic)
