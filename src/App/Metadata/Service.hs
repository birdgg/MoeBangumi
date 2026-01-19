{- | Metadata service for querying BGM.tv and TMDB data

This service provides independent search functions for each metadata source.
-}
module App.Metadata.Service (
  -- * Search Operations
  searchFromBgmtv,
  searchFromTmdb,

  -- * TMDB Operations
  getTmdbPoster,

  -- * BGM.tv Operations
  getBgmtvPoster,

  -- * Episode Operations
  getEpisodeOffset,
) where

import Data.Text qualified as T
import Effectful (Eff, IOE, (:>))
import Effectful.Reader.Static qualified as Reader
import Servant.Client (mkClientEnv, runClientM)

import Infra.Environment.Env (MoeEnv (..))
import Infra.External.Bgmtv.API (bgmtvBaseUrl)
import Infra.External.Bgmtv.Client qualified as BgmtvClient
import Infra.External.Bgmtv.Types qualified as Bgm
import Infra.External.Tmdb.API (tmdbBaseUrl)
import Infra.External.Tmdb.Client qualified as TmdbClient
import Infra.External.Tmdb.Types qualified as Tmdb
import Moe.Metadata (BgmtvSearchItem (..), TmdbMediaType (..), TmdbSearchItem (..))
import Moe.Parsing.Bgmtv (parseBgmtvName)
import Moe.Parsing.Bgmtv qualified as BgmtvParser
import Moe.Setting (Settings (..), TmdbSettings (..))

--------------------------------------------------------------------------------
-- Type Conversions
--------------------------------------------------------------------------------

{- | Convert BGM.tv Subject to our search item type

Uses parseBgmtvName to extract clean titles and season number
-}
bgmtvSubjectToSearchItem :: Bgm.Subject -> BgmtvSearchItem
bgmtvSubjectToSearchItem s =
  let japaneseParsed = parseBgmtvName s.name
      chineseParsed = parseBgmtvName s.nameCn
      -- Use the higher season number from either parse
      seasonNum = max (BgmtvParser.season japaneseParsed) (BgmtvParser.season chineseParsed)
   in BgmtvSearchItem
        { id = s.id
        , titleJapanese = BgmtvParser.title japaneseParsed
        , titleChinese = BgmtvParser.title chineseParsed
        , posterUrl = s.image
        , airDate = s.date
        , totalEpisodes = s.eps
        , season = seasonNum
        }

-- | Convert TMDB MultiSearchResult to our search item type
-- Returns Nothing for person results (which we filter out)
tmdbMultiToSearchItem :: Tmdb.MultiSearchResult -> Maybe TmdbSearchItem
tmdbMultiToSearchItem s = case s.mediaType of
  Tmdb.Person -> Nothing
  Tmdb.Tv ->
    Just
      TmdbSearchItem
        { id = s.id
        , name = fromMaybe "" (s.name <|> s.originalName)
        , mediaType = TmdbTv
        , airDate = s.firstAirDate
        , posterPath = s.posterPath
        }
  Tmdb.Movie ->
    Just
      TmdbSearchItem
        { id = s.id
        , name = fromMaybe "" (s.title <|> s.originalTitle)
        , mediaType = TmdbMovie
        , airDate = s.releaseDate
        , posterPath = s.posterPath
        }

--------------------------------------------------------------------------------
-- Search Operations
--------------------------------------------------------------------------------

-- | Search from BGM.tv
searchFromBgmtv ::
  ( Reader.Reader MoeEnv :> es
  , IOE :> es
  ) =>
  Text ->
  Eff es [BgmtvSearchItem]
searchFromBgmtv keyword = do
  env <- Reader.ask @MoeEnv
  let clientEnv = mkClientEnv env.httpManager bgmtvBaseUrl
  result <- liftIO $ runClientM (BgmtvClient.searchBangumi keyword) clientEnv
  case result of
    Left _err -> pure []
    Right subjects -> pure $ map bgmtvSubjectToSearchItem subjects

-- | Search from TMDB using multi search API
-- Returns both TV shows and movies, filters out person results
searchFromTmdb ::
  ( Reader.Reader MoeEnv :> es
  , IOE :> es
  ) =>
  Text ->
  Eff es [TmdbSearchItem]
searchFromTmdb keyword = do
  env <- Reader.ask @MoeEnv
  -- Read settings from TVar for hot-reload support
  settings :: Settings <- readTVarIO env.settingsVar
  let apiKey = (settings.tmdb :: TmdbSettings).apiKey
      clientEnv = mkClientEnv env.httpManager tmdbBaseUrl
  if T.null apiKey
    then pure []
    else do
      result <- liftIO $ runClientM (TmdbClient.searchMulti apiKey keyword) clientEnv
      case result of
        Left _err -> pure []
        Right response -> pure $ mapMaybe tmdbMultiToSearchItem response.results

--------------------------------------------------------------------------------
-- Episode Operations
--------------------------------------------------------------------------------

{- | Get episode offset for a BGM.tv subject

Returns the sort value of the first episode (by sort order).
-}
getEpisodeOffset ::
  ( Reader.Reader MoeEnv :> es
  , IOE :> es
  ) =>
  Int64 ->
  Eff es (Maybe Double)
getEpisodeOffset subjectId = do
  env <- Reader.ask @MoeEnv
  let clientEnv = mkClientEnv env.httpManager bgmtvBaseUrl
  result <- liftIO $ runClientM (BgmtvClient.getEpisodes subjectId) clientEnv
  pure $ case result of
    Left _err -> Nothing
    Right response -> do
      ep <- viaNonEmpty head response.data_
      epNum <- ep.ep
      pure $ ep.sort - epNum

--------------------------------------------------------------------------------
-- TMDB Operations
--------------------------------------------------------------------------------

{- | Get poster URL from TMDB by tmdbId

Returns the full poster URL (with CDN prefix) or Nothing if not found.
-}
getTmdbPoster ::
  ( Reader.Reader MoeEnv :> es
  , IOE :> es
  ) =>
  Int ->
  Eff es (Either Text Text)
getTmdbPoster tmdbId = do
  env <- Reader.ask @MoeEnv
  settings :: Settings <- readTVarIO env.settingsVar
  let apiKey = (settings.tmdb :: TmdbSettings).apiKey
      clientEnv = mkClientEnv env.httpManager tmdbBaseUrl
  if T.null apiKey
    then pure $ Left "TMDB API key is empty"
    else do
      result <- liftIO $ runClientM (TmdbClient.getTvDetail apiKey (fromIntegral tmdbId)) clientEnv
      pure $ case result of
        Left err -> Left $ "TMDB API error: " <> show err
        Right tvDetail -> case tvDetail.posterPath of
          Nothing -> Left "No poster_path in TMDB response"
          Just p -> Right $ "https://image.tmdb.org/t/p/w500" <> p

{- | Get poster URL from BGM.tv by bgmtvId

Returns the large image URL from BGM.tv subject details.
-}
getBgmtvPoster ::
  ( Reader.Reader MoeEnv :> es
  , IOE :> es
  ) =>
  Int ->
  Eff es (Either Text Text)
getBgmtvPoster bgmtvId = do
  env <- Reader.ask @MoeEnv
  let clientEnv = mkClientEnv env.httpManager bgmtvBaseUrl
  result <- liftIO $ runClientM (BgmtvClient.getSubject (fromIntegral bgmtvId)) clientEnv
  pure $ case result of
    Left err -> Left $ "BGM.tv API error: " <> show err
    Right subject ->
      let imageUrl = subject.images.large
       in if T.null imageUrl
            then Left "No image in BGM.tv response"
            else Right imageUrl
