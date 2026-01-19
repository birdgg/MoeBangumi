module Moe.Monad where

import Effectful
import RequireCallStack

type MoeM es a = RequireCallStack => Eff es a
