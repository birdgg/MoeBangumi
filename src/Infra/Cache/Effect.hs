{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}

{- | LRU Cache effect using lrucaching library

This module provides an effectful LRU (Least Recently Used) cache with TTL
(Time To Live) support. Uses the lrucaching library for efficient cache
management.

== Usage

@
import Effectful
import Effectful.Cache

example :: IO ()
example = runEff . runCache defaultCacheConfig $ do
  cachePut "key1" "value1"
  result <- cacheGet "key1"
  liftIO $ print result  -- Just "value1"
@
-}
module Infra.Cache.Effect (
  -- * Effect
  Cache,

  -- * Operations
  cacheGet,
  cachePut,
  cacheInvalidate,
  cacheClear,

  -- * Handler
  runCache,

  -- * Configuration
  CacheConfig (..),
  defaultCacheConfig,
) where

import Data.LruCache qualified as LRU
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..))
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Cache configuration
data CacheConfig = CacheConfig
  { maxSize :: Int
  -- ^ Maximum number of cache entries
  , ttl :: NominalDiffTime
  -- ^ Time-to-live for cache entries (in seconds)
  }
  deriving stock (Show, Eq)

-- | Default cache configuration (1000 entries, 24 hours TTL)
defaultCacheConfig :: CacheConfig
defaultCacheConfig =
  CacheConfig
    { maxSize = 1000
    , ttl = 86400 -- 24 hours
    }

--------------------------------------------------------------------------------
-- Internal Types
--------------------------------------------------------------------------------

-- | Cache entry with timestamp for TTL tracking
data CacheEntry v = CacheEntry
  { value :: v
  , insertedAt :: UTCTime
  }
  deriving stock (Show, Eq)

-- | Internal cache state with config
data CacheState k v = CacheState
  { cache :: LRU.LruCache k (CacheEntry v)
  , config :: CacheConfig
  }

--------------------------------------------------------------------------------
-- Effect Definition
--------------------------------------------------------------------------------

-- | Cache effect for LRU caching with TTL
data Cache k v :: Effect where
  CacheGet :: k -> Cache k v m (Maybe v)
  CachePut :: k -> v -> Cache k v m ()
  CacheInvalidate :: k -> Cache k v m ()
  CacheClear :: Cache k v m ()

type instance DispatchOf (Cache k v) = Dynamic

makeEffect ''Cache

--------------------------------------------------------------------------------
-- Handler
--------------------------------------------------------------------------------

-- | Run Cache effect with given configuration
runCache ::
  (IOE :> es, Hashable k, Ord k) =>
  CacheConfig ->
  Eff (Cache k v : es) a ->
  Eff es a
runCache cfg action = do
  stateRef <- liftIO $ newIORef initialState
  interpret (handleCache stateRef) action
 where
  initialState =
    CacheState
      { cache = LRU.empty cfg.maxSize
      , config = cfg
      }

-- | Cache effect handler
handleCache ::
  (IOE :> es, Hashable k, Ord k) =>
  IORef (CacheState k v) ->
  EffectHandler (Cache k v) es
handleCache stateRef = \_ -> \case
  CacheGet key -> handleGet stateRef key
  CachePut key val -> handlePut stateRef key val
  CacheInvalidate key -> handleInvalidate stateRef key
  CacheClear -> handleClear stateRef

-- | Handle cache get operation with TTL check
handleGet ::
  (IOE :> es, Hashable k, Ord k) =>
  IORef (CacheState k v) ->
  k ->
  Eff es (Maybe v)
handleGet stateRef key = liftIO $ do
  now <- getCurrentTime
  atomicModifyIORef' stateRef $ \st ->
    case LRU.lookup key st.cache of
      Nothing -> (st, Nothing)
      Just (entry, newCache)
        | isExpired now st.config.ttl entry ->
            -- Expired - remove from cache by not updating with the looked-up value
            -- LRU.lookup already removed it, so we just use the cache without re-inserting
            (st{cache = newCache}, Nothing)
        | otherwise ->
            (st{cache = newCache}, Just entry.value)

-- | Handle cache put operation
handlePut ::
  (IOE :> es, Hashable k, Ord k) =>
  IORef (CacheState k v) ->
  k ->
  v ->
  Eff es ()
handlePut stateRef key val = liftIO $ do
  now <- getCurrentTime
  let entry = CacheEntry{value = val, insertedAt = now}
  atomicModifyIORef' stateRef $ \st ->
    (st{cache = LRU.insert key entry st.cache}, ())

-- | Handle cache invalidate operation
--
-- Note: lrucaching doesn't have a delete operation, so we insert a dummy
-- expired entry that will be ignored on next lookup
handleInvalidate ::
  (IOE :> es, Hashable k, Ord k) =>
  IORef (CacheState k v) ->
  k ->
  Eff es ()
handleInvalidate stateRef key = liftIO $ do
  -- We can't delete, but we can mark as expired by setting a very old timestamp
  let expiredEntry = CacheEntry{value = error "expired", insertedAt = epoch}
  atomicModifyIORef' stateRef $ \st ->
    (st{cache = LRU.insert key expiredEntry st.cache}, ())
 where
  -- A time in the distant past (Unix epoch)
  epoch = UTCTime (fromGregorian 1970 1 1) 0

-- | Handle cache clear operation
handleClear ::
  (IOE :> es) =>
  IORef (CacheState k v) ->
  Eff es ()
handleClear stateRef = liftIO $
  atomicModifyIORef' stateRef $ \st ->
    (st{cache = LRU.empty st.config.maxSize}, ())

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | Check if a cache entry has expired
isExpired :: UTCTime -> NominalDiffTime -> CacheEntry v -> Bool
isExpired now ttlSeconds entry =
  diffUTCTime now entry.insertedAt > ttlSeconds
