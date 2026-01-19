-- | Externally facing config parsed from the environment.
module Infra.Environment.Config
  ( MoeConfig (..)
  , DeploymentEnv (..)
  , ConfigLogLevel (..)
  , parseConfig
  , parseDeploymentEnv
  , toLogBaseLevel
  )
where

import Data.Text.Display (Display (..))
import Env
  ( AsUnread (unread)
  , Error (..)
  , Parser
  , def
  , str
  , help
  , var
  )
import Env qualified
import Log.Data qualified as LogBase

-- | Log level for configuration (maps to log-base LogLevel)
data ConfigLogLevel
  = LogTrace
  | LogInfo
  | LogAttention
  deriving stock (Show, Eq, Generic)

-- | Convert ConfigLogLevel to log-base LogLevel
toLogBaseLevel :: ConfigLogLevel -> LogBase.LogLevel
toLogBaseLevel = \case
  LogTrace -> LogBase.LogTrace
  LogInfo -> LogBase.LogInfo
  LogAttention -> LogBase.LogAttention

data DeploymentEnv
  = Production
  | Development
  deriving stock (Bounded, Enum, Eq, Generic, Show)

instance Display DeploymentEnv where
  displayBuilder Production = "production"
  displayBuilder Development = "development"

-- | The datatype that is used to model the external configuration
data MoeConfig = MoeConfig
  { httpPort :: Word16
  , environment :: DeploymentEnv
  , dataFolder :: FilePath
  , logLevel :: ConfigLogLevel
  }
  deriving stock (Generic, Show)

parsePort :: Parser Error Word16
parsePort = var port "MOE_HTTP_PORT" (help "HTTP Port for Moe" <> def 3000)

parseDataFolder :: Parser Error FilePath
parseDataFolder = var str "MOE_DATA_FOLDER" (help "Data folder path for Moe" <> def "./data")

parseDeploymentEnv :: Parser Error DeploymentEnv
parseDeploymentEnv =
  var deploymentEnv "MOE_ENVIRONMENT" (help "Name of the current environment (production, development)" <> def Development)

parseLogLevel :: Parser Error ConfigLogLevel
parseLogLevel =
  var logLevelReader "MOE_LOG_LEVEL" (help "Log level (trace, info, attention)" <> def LogInfo)

parseConfig :: Parser Error MoeConfig
parseConfig =
  MoeConfig
    <$> parsePort
    <*> parseDeploymentEnv
    <*> parseDataFolder
    <*> parseLogLevel

-- Env parser helpers

int :: Env.Reader Error Int
int i = case readMaybe i of
  Nothing -> Left . unread . show $ i
  Just i' -> Right i'

port :: Env.Reader Error Word16
port p = case int p of
  Left err -> Left err
  Right intPort ->
    if intPort >= 1 && intPort <= 65535
      then Right $ fromIntegral intPort
      else Left . unread . show $ p

deploymentEnv :: Env.Reader Error DeploymentEnv
deploymentEnv "production" = Right Production
deploymentEnv "development" = Right Development
deploymentEnv e = Left $ unread e

logLevelReader :: Env.Reader Error ConfigLogLevel
logLevelReader "trace" = Right LogTrace
logLevelReader "info" = Right LogInfo
logLevelReader "attention" = Right LogAttention
logLevelReader e = Left $ unread e
