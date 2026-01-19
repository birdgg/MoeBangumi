{-# LANGUAGE TemplateHaskell #-}

-- | Effectful bindings for self-update operations
module Infra.External.SelfUpdate.Effect
  ( -- * Effect
    SelfUpdate

    -- * Handler
  , runSelfUpdate

    -- * Operations
  , checkUpdate
  , getReleases
  , performUpdate

    -- * Re-exports
  , module Infra.External.SelfUpdate.Types
  , getTargetTriple
  )
where

import Infra.External.SelfUpdate.Client qualified as Client
import Infra.External.SelfUpdate.GitHub (getTargetTriple)
import Infra.External.SelfUpdate.GitHub qualified as GitHub
import Infra.External.SelfUpdate.Types

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH
import Network.HTTP.Client (Manager)

-- | SelfUpdate effect for application self-update operations
data SelfUpdate :: Effect where
  CheckUpdate :: UpdateConfig -> SelfUpdate m (Either UpdateError UpdateStatus)
  GetReleases :: UpdateConfig -> SelfUpdate m (Either UpdateError [Release])
  PerformUpdate :: UpdateConfig -> SelfUpdate m (Either UpdateError UpdateResult)

type instance DispatchOf SelfUpdate = Dynamic

makeEffect ''SelfUpdate

-- | Run SelfUpdate effect
runSelfUpdate
  :: (IOE :> es)
  => Manager
  -> Eff (SelfUpdate : es) a
  -> Eff es a
runSelfUpdate manager = interpret $ \_ -> \case
  CheckUpdate config -> liftIO $ Client.checkUpdate manager config
  GetReleases config -> liftIO $ GitHub.fetchReleases manager config
  PerformUpdate config -> liftIO $ Client.performUpdate manager config
