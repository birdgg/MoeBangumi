{-# OPTIONS_GHC -Wno-orphans #-}

module Web.Types
  ( RouteEffects,
    MoeEff
  ) where

import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Error.Static (Error)
import Effectful.Log (Log)
import Infra.Database.Effect (DB)
import Effectful.Reader.Static qualified as Reader
import Effectful.Time (Time)
import Servant (ServerError)

import Infra.Environment.Env


type MoeEff = Eff RouteEffects

type RouteEffects =
  '[ DB
   , Time
   , Error ServerError
   , Log
   , Concurrent
   , Reader.Reader MoeEnv
   , IOE
   ]
