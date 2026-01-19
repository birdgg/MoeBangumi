{- | Minimal job type definitions

Simple types for scheduled jobs without unnecessary abstractions.
Each job encapsulates its own effect stack and runs as pure IO.
-}
module App.Jobs.Types
  ( -- * Job Definition
    Job (..)

    -- * Time Helpers
  , seconds
  , minutes
  , hours
  , days
  )
where

import Data.Time (NominalDiffTime)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

{- | Job definition with self-contained IO action

Each job manages its own effect stack internally.
The action is pure IO, meaning the job is responsible for
running all necessary effect interpreters.
-}
data Job = Job
  { name :: Text
  -- ^ Job name for logging
  , interval :: NominalDiffTime
  -- ^ Execution interval
  , action :: IO ()
  -- ^ Self-contained task to execute (manages its own effects)
  }

--------------------------------------------------------------------------------
-- Time Helpers
--------------------------------------------------------------------------------

-- | Convert seconds to NominalDiffTime
seconds :: Integer -> NominalDiffTime
seconds = fromInteger

-- | Convert minutes to NominalDiffTime
minutes :: Integer -> NominalDiffTime
minutes m = fromInteger (m * 60)

-- | Convert hours to NominalDiffTime
hours :: Integer -> NominalDiffTime
hours h = fromInteger (h * 3600)

-- | Convert days to NominalDiffTime
days :: Integer -> NominalDiffTime
days d = fromInteger (d * 86400)
