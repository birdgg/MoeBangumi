{-# LANGUAGE TemplateHaskell #-}

{- | Emby Effect - Abstract interface for Emby media server

This module provides an effectful interface for Emby API operations.
Authentication is handled via API Key passed in headers.

= Supported Operations

- Get virtual folders (libraries)
- Get items from a library
- Get seasons for a series
- Get episode count for a series

= Usage

@
import Infra.External.Emby.Effect

syncLibrary :: (Emby :> es) => Eff es ()
syncLibrary = do
  result <- getEmbyLibraries
  case result of
    Right libs -> liftIO $ print libs
    Left err -> liftIO $ print (display err)
@
-}
module Infra.External.Emby.Effect
  ( -- * Effect
    Emby (..)

    -- * Operations
  , getEmbyLibraries
  , getEmbyItems
  , getEmbySeriesItems
  , getEmbySeasons
  , getEmbyEpisodeCount

    -- * Handler
  , runEmby

    -- * Re-exports
  , module Infra.External.Emby.Types
  )
where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH
import Network.HTTP.Client (Manager)
import Servant.Client (ClientEnv, ClientError (..), mkClientEnv, runClientM)
import Servant.Client qualified as Servant

import Infra.External.Emby.API (mkEmbyBaseUrl)
import Infra.External.Emby.Client qualified as Client
import Infra.External.Emby.Types
import Moe.Setting (EmbySettings (..), isEmbyEnabled)

--------------------------------------------------------------------------------
-- Effect Definition
--------------------------------------------------------------------------------

-- | Emby effect for media server operations
data Emby :: Effect where
  -- | Get all virtual folders (libraries)
  GetEmbyLibraries
    :: Emby m (Either EmbyError [EmbyLibrary])
  -- | Get items from a library
  GetEmbyItems
    :: Maybe Text
    -- ^ Parent ID (library ID)
    -> Maybe Text
    -- ^ Item types filter
    -> Emby m (Either EmbyError EmbyItemsResponse)
  -- | Get Series items from a library
  GetEmbySeriesItems
    :: Text
    -- ^ Library ID
    -> Emby m (Either EmbyError EmbyItemsResponse)
  -- | Get seasons for a series
  GetEmbySeasons
    :: Text
    -- ^ Series ID
    -> Emby m (Either EmbyError EmbyItemsResponse)
  -- | Get episode count for a series
  GetEmbyEpisodeCount
    :: Text
    -- ^ Series ID
    -> Emby m (Either EmbyError Int)

type instance DispatchOf Emby = Dynamic

makeEffect ''Emby

--------------------------------------------------------------------------------
-- Handler
--------------------------------------------------------------------------------

-- | Run Emby effect
runEmby ::
  (IOE :> es) =>
  Manager ->
  EmbySettings ->
  Eff (Emby : es) a ->
  Eff es a
runEmby manager settings action
  | not (isEmbyEnabled settings) = interpret handleDisabled action
  | otherwise = do
      let clientEnv = mkClientEnv manager (mkEmbyBaseUrl settings)
      interpret (handleEmby clientEnv settings.apiKey) action

-- | Handler when Emby is disabled
handleDisabled :: (IOE :> es) => EffectHandler Emby es
handleDisabled _ = \case
  GetEmbyLibraries -> pure $ Left EmbyDisabled
  GetEmbyItems _ _ -> pure $ Left EmbyDisabled
  GetEmbySeriesItems _ -> pure $ Left EmbyDisabled
  GetEmbySeasons _ -> pure $ Left EmbyDisabled
  GetEmbyEpisodeCount _ -> pure $ Left EmbyDisabled

-- | Effect handler
handleEmby ::
  (IOE :> es) =>
  ClientEnv ->
  Text ->
  EffectHandler Emby es
handleEmby env apiKey _ = \case
  GetEmbyLibraries ->
    liftIO $ first clientErrorToEmbyError <$> runClientM (Client.getLibraries apiKey) env
  GetEmbyItems parentId itemTypes ->
    liftIO $ first clientErrorToEmbyError <$> runClientM (Client.getItems apiKey parentId itemTypes) env
  GetEmbySeriesItems libraryId ->
    liftIO $ first clientErrorToEmbyError <$> runClientM (Client.getSeriesItems apiKey libraryId) env
  GetEmbySeasons seriesId ->
    liftIO $ first clientErrorToEmbyError <$> runClientM (Client.getSeasons apiKey seriesId) env
  GetEmbyEpisodeCount seriesId ->
    liftIO $ first clientErrorToEmbyError <$> runClientM (Client.getEpisodeCount apiKey seriesId) env

--------------------------------------------------------------------------------
-- Error Conversion
--------------------------------------------------------------------------------

-- | Convert ClientError to EmbyError
clientErrorToEmbyError :: ClientError -> EmbyError
clientErrorToEmbyError = \case
  FailureResponse _req resp ->
    EmbyApiError $ "HTTP " <> show (Servant.responseStatusCode resp)
  DecodeFailure msg _ ->
    EmbyParseError $ toText msg
  UnsupportedContentType _ _ ->
    EmbyParseError "Unsupported content type"
  InvalidContentTypeHeader _ ->
    EmbyParseError "Invalid content type header"
  Servant.ConnectionError exc ->
    EmbyConnectionError $ show exc
