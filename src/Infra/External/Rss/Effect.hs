{-# LANGUAGE TemplateHaskell #-}

-- | Effectful bindings for RSS operations
--
-- This module provides an effectful interface for RSS fetching.
--
-- == Usage
--
-- @
-- import Effectful
-- import Network.HTTP.Client (newManager)
-- import Network.HTTP.Client.TLS (tlsManagerSettings)
-- import Effectful.Rss
--
-- main :: IO ()
-- main = do
--   manager <- newManager tlsManagerSettings
--   result <- runEff . runRss manager $ do
--     items <- fetchRss "https://example.com/rss"
--     case items of
--       Right is -> liftIO $ print is
--       Left err -> liftIO $ print err
--   print result
-- @
module Infra.External.Rss.Effect
  ( -- * Effect
    Rss

    -- * Handler
  , runRss

    -- * Operations
  , fetchRss

    -- * Re-exports
  , module Infra.External.Rss.Types
  )
where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH
import Network.HTTP.Client (Manager)
import Infra.External.Rss.Client qualified as Client
import Infra.External.Rss.Types

-- | Rss effect for RSS fetching operations
data Rss :: Effect where
  -- | Fetch RSS feed and return raw items
  FetchRss :: Text -> Rss m (Either RssError [RawItem])

type instance DispatchOf Rss = Dynamic

makeEffect ''Rss

-- | Run Rss effect
--
-- This handler performs actual HTTP requests.
-- The HTTP Manager should be created by the caller and passed in for reuse.
runRss
  :: (IOE :> es)
  => Manager
  -> Eff (Rss : es) a
  -> Eff es a
runRss manager = interpret $ \_ -> \case
  FetchRss url -> liftIO $ Client.fetchRss manager url
