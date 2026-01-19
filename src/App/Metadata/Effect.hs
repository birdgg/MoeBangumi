{-# LANGUAGE TemplateHaskell #-}

{- | Effectful bindings for Metadata service

This module provides an effectful interface for querying metadata from
BGM.tv and TMDB.
-}
module App.Metadata.Effect (
  -- * Effect
  Metadata,

  -- * Handler
  runMetadata,

  -- * Operations
  searchBgmtv,
  searchTmdb,
  getEpisodeOffset,
  getTmdbPoster,

  -- * Re-exports
  module Moe.Metadata,
) where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Reader.Static qualified as Reader
import Effectful.TH

import App.Metadata.Service qualified as Service
import Infra.Environment.Env (MoeEnv)
import Moe.Metadata

--------------------------------------------------------------------------------
-- Effect Definition
--------------------------------------------------------------------------------

-- | Metadata effect for metadata operations
data Metadata :: Effect where
  -- | Search bangumi from BGM.tv
  SearchBgmtv :: Text -> Metadata m [BgmtvSearchItem]
  -- | Search bangumi from TMDB
  SearchTmdb :: Text -> Metadata m [TmdbSearchItem]
  -- | Get episode offset (first episode's sort value) from BGM.tv
  GetEpisodeOffset :: Int64 -> Metadata m (Maybe Double)
  -- | Get poster URL from TMDB by tmdbId
  GetTmdbPoster :: Int -> Metadata m (Either Text Text)

type instance DispatchOf Metadata = Dynamic

makeEffect ''Metadata

--------------------------------------------------------------------------------
-- Handler
--------------------------------------------------------------------------------

{- | Run Metadata effect

This handler requires the underlying effects to be present in the stack:
- Reader MoeEnv: for HTTP manager and settings
- IOE: for IO operations
-}
runMetadata ::
  ( Reader.Reader MoeEnv :> es
  , IOE :> es
  ) =>
  Eff (Metadata : es) a ->
  Eff es a
runMetadata = interpret $ \_ -> \case
  SearchBgmtv keyword ->
    Service.searchFromBgmtv keyword
  SearchTmdb keyword ->
    Service.searchFromTmdb keyword
  GetEpisodeOffset subjectId ->
    Service.getEpisodeOffset subjectId
  GetTmdbPoster tmdbId ->
    Service.getTmdbPoster tmdbId
