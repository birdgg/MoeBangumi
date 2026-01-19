{- | Calendar sync service

Fetches calendar data from bangumi-data and syncs to local database.
-}
module App.Calendar.Sync (
  -- * Sync Operations
  syncCalendar,
  getCalendarDays,
  getCurrentSeason,
) where

import Data.Map.Strict qualified as Map
import Data.Time (UTCTime, getCurrentTime, toGregorian, utctDay)
import Effectful (Eff, IOE, (:>))
import Effectful.Reader.Static qualified as Reader
import App.Metadata.Service qualified as MetadataService
import Infra.Database.Effect (DB)
import Infra.Database.Repository.Bangumi qualified as BangumiRepo
import Infra.Database.Repository.Calendar qualified as CalendarRepo
import Infra.Database.Repository.Subscription qualified as SubRepo
import Infra.Environment.Env (MoeEnv (..))
import Infra.External.BangumiData.Client qualified as BangumiData
import Infra.External.BangumiData.Types (CalendarItem (..))
import Data.Aeson (object, (.=))
import Effectful.Log (Log)
import Effectful.Log qualified as Log
import Moe.Bangumi (Bangumi (..), Platform (..))
import Moe.Calendar

--------------------------------------------------------------------------------
-- Sync Operations
--------------------------------------------------------------------------------

-- | Sync calendar data from bangumi-data for a specific year and season
syncCalendar ::
  ( DB :> es
  , Reader.Reader MoeEnv :> es
  , Log :> es
  , IOE :> es
  ) =>
  Int ->
  Season ->
  Eff es [CalendarDay]
syncCalendar year season = do
  env <- Reader.ask @MoeEnv
  Log.logInfo "Syncing calendar from bangumi-data" $
    object
      [ "year" .= year
      , "season" .= seasonToText season
      ]

  -- Fetch from bangumi-data
  result <- liftIO $ BangumiData.fetchCalendar env.httpManager year season
  case result of
    Left err -> do
      Log.logAttention "Failed to fetch bangumi-data" $
        object ["error" .= show err]
      -- Return existing data on error
      getCalendarDays year season
    Right items -> do
      Log.logInfo "Fetched bangumi-data" $
        object ["count" .= length items]

      -- Process each item: find or create bangumi, then upsert calendar
      forM_ items $ \item -> do
        mBangumi <- findOrCreateBangumi item
        case mBangumi of
          Nothing -> pass
          Just bangumi -> do
            CalendarRepo.upsert $
              CalendarRepo.CreateCalendar
                { year = year
                , season = season
                , bangumiId = bangumi.id
                }

      -- Fetch TMDB posters for bangumi that need them
      syncTmdbPosters year season

      -- Return updated calendar
      getCalendarDays year season

-- | Find existing bangumi or create new one from bangumi-data item
findOrCreateBangumi ::
  (DB :> es, Log :> es) =>
  CalendarItem ->
  Eff es (Maybe Bangumi)
findOrCreateBangumi item = do
  -- First try to find by mikan_id
  existingMikan <- case item.mikanId of
    Just mikanId -> BangumiRepo.findByMikanId mikanId
    Nothing -> pure Nothing
  case existingMikan of
    Just b -> case (item.tmdbId, b.tmdbId) of
      (Just newTmdbId, Nothing) ->
        updateBangumiIds b.id Nothing Nothing (Just newTmdbId)
      _ -> pure (Just b)
    Nothing -> do
      -- Try to find by bgmtv_id if available
      existingBgm <- case item.bgmtvId of
        Just bgmId -> BangumiRepo.findByBgmtvId bgmId
        Nothing -> pure Nothing
      case existingBgm of
        Just b -> updateBangumiIds b.id item.mikanId Nothing item.tmdbId
        Nothing -> do
          -- Try to find by tmdb_id if available
          existingTmdb <- case item.tmdbId of
            Just tmdbId -> BangumiRepo.findByTmdbId tmdbId
            Nothing -> pure Nothing
          case existingTmdb of
            Just b -> updateBangumiIds b.id item.mikanId item.bgmtvId Nothing
            Nothing -> createNewBangumi item

-- | Update only the external IDs on an existing bangumi
updateBangumiIds ::
  (DB :> es) =>
  Int ->
  Maybe Text ->
  Maybe Int ->
  Maybe Int ->
  Eff es (Maybe Bangumi)
updateBangumiIds bangumiId mikanId bgmtvId tmdbId = do
  _ <- BangumiRepo.update bangumiId $
    BangumiRepo.UpdateBangumi
      { mikanId = mikanId
      , bgmtvId = bgmtvId
      , tmdbId = tmdbId
      , titleChinese = Nothing
      , titleJapanese = Nothing
      , season = Nothing
      , platform = Nothing
      , totalEpisodes = Nothing
      , posterUrl = Nothing
      , airDate = Nothing
      , airWeek = Nothing
      }
  BangumiRepo.findById bangumiId

-- | Create a new bangumi from bangumi-data item (metadata only, no subscription)
createNewBangumi ::
  (DB :> es, Log :> es) =>
  CalendarItem ->
  Eff es (Maybe Bangumi)
createNewBangumi item = do
  Log.logInfo "Creating new bangumi from bangumi-data" $
    object
      [ "mikan_id" .= item.mikanId
      , "bgmtv_id" .= item.bgmtvId
      , "tmdb_id" .= item.tmdbId
      , "title" .= item.titleChinese
      ]
  bangumi <- BangumiRepo.create $
    BangumiRepo.CreateBangumi
      { mikanId = item.mikanId
      , bgmtvId = item.bgmtvId
      , tmdbId = item.tmdbId
      , titleChinese = item.titleChinese
      , titleJapanese = Just item.titleJapanese
      , season = 1
      , platform = itemTypeToPlatform item.itemType
      , totalEpisodes = 0
      , posterUrl = Nothing
      , airDate = item.airDate
      , airWeek = item.airWeekday
      }
  pure (Just bangumi)

-- | Convert bangumi-data item type to Platform
itemTypeToPlatform :: Text -> Platform
itemTypeToPlatform = \case
  "tv" -> TV
  "movie" -> Movie
  "ova" -> OVA
  "web" -> TV
  _ -> TV

--------------------------------------------------------------------------------
-- Query Operations
--------------------------------------------------------------------------------

-- | Get calendar days for a specific year and season
getCalendarDays ::
  (DB :> es) =>
  Int ->
  Season ->
  Eff es [CalendarDay]
getCalendarDays year season = do
  -- Get all calendar entries for this season
  entries <- CalendarRepo.findBySeason year season

  -- Get all bangumi for these entries with subscription status
  bangumisWithSub <- forM entries $ \entry -> do
    mBangumi <- BangumiRepo.findById entry.bangumiId
    mSub <- SubRepo.findByBangumiId entry.bangumiId
    pure (mBangumi, isJust mSub)

  -- Group by weekday
  let validBangumis = [(b, subscribed) | (Just b, subscribed) <- bangumisWithSub]
      grouped = groupByWeekday validBangumis

  pure $ map toCalendarDay (Map.toList grouped)

-- | Group bangumi by their air weekday
groupByWeekday :: [(Bangumi, Bool)] -> Map.Map Int [(Bangumi, Bool)]
groupByWeekday = foldr insertByWeek Map.empty
 where
  insertByWeek (b, subscribed) = Map.insertWith (++) (normalizeWeekday b.airWeek) [(b, subscribed)]
  -- Convert 0=Sunday to 7 for consistent ordering (1=Monday...7=Sunday)
  normalizeWeekday 0 = 7
  normalizeWeekday n = n

-- | Convert grouped data to CalendarDay
toCalendarDay :: (Int, [(Bangumi, Bool)]) -> CalendarDay
toCalendarDay (weekdayId, bangumis) =
  CalendarDay
    { weekday = Weekday
        { id = weekdayId
        , name = weekdayName weekdayId
        }
    , items = map (uncurry toCalendarSubject) bangumis
    }

-- | Convert Bangumi to CalendarSubject
toCalendarSubject :: Bangumi -> Bool -> CalendarSubject
toCalendarSubject b subscribed =
  CalendarSubject
    { mikanId = b.mikanId
    , bgmtvId = b.bgmtvId
    , tmdbId = b.tmdbId
    , titleChinese = b.titleChinese
    , titleJapanese = b.titleJapanese
    , posterUrl = b.posterUrl
    , airDate = b.airDate
    , airWeek = b.airWeek
    , totalEpisodes = b.totalEpisodes
    , season = b.season
    , platform = b.platform
    , subscribed = subscribed
    }

weekdayName :: Int -> Text
weekdayName = \case
  1 -> "Monday"
  2 -> "Tuesday"
  3 -> "Wednesday"
  4 -> "Thursday"
  5 -> "Friday"
  6 -> "Saturday"
  7 -> "Sunday"
  _ -> "Unknown"

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Get current year and season based on current date
getCurrentSeason :: IO (Int, Season)
getCurrentSeason = do
  now <- getCurrentTime
  pure $ dateToSeason now

-- | Convert a date to year and season
dateToSeason :: UTCTime -> (Int, Season)
dateToSeason time =
  let (year, month, _) = toGregorian (utctDay time)
      season = monthToSeason month
   in (fromInteger year, season)

monthToSeason :: Int -> Season
monthToSeason m
  | m >= 1 && m <= 3 = Winter
  | m >= 4 && m <= 6 = Spring
  | m >= 7 && m <= 9 = Summer
  | otherwise = Fall

--------------------------------------------------------------------------------
-- TMDB Poster Sync
--------------------------------------------------------------------------------

-- | Sync TMDB posters for bangumi that have tmdbId but no posterUrl
syncTmdbPosters ::
  ( DB :> es
  , Reader.Reader MoeEnv :> es
  , Log :> es
  , IOE :> es
  ) =>
  Int ->
  Season ->
  Eff es ()
syncTmdbPosters year season = do
  -- Get all calendar entries for this season
  entries <- CalendarRepo.findBySeason year season
  bangumis <- catMaybes <$> forM entries (BangumiRepo.findById . (.bangumiId))

  -- Filter to those needing poster fetch
  let needsPoster = filter shouldFetchPoster bangumis
  when (not $ null needsPoster) $ do
    Log.logInfo "Fetching TMDB posters for calendar bangumi" $
      object ["count" .= length needsPoster]

  -- Fetch and update each
  forM_ needsPoster fetchAndUpdatePoster

-- | Check if a bangumi needs poster fetch (has tmdbId or bgmtvId, but no posterUrl)
shouldFetchPoster :: Bangumi -> Bool
shouldFetchPoster bangumi =
  isNothing bangumi.posterUrl && (isJust bangumi.tmdbId || isJust bangumi.bgmtvId)

-- | Fetch poster from TMDB (with BGM.tv fallback) and update bangumi
fetchAndUpdatePoster ::
  ( DB :> es
  , Reader.Reader MoeEnv :> es
  , Log :> es
  , IOE :> es
  ) =>
  Bangumi ->
  Eff es ()
fetchAndUpdatePoster bangumi = do
  -- Try TMDB first, then BGM.tv as fallback
  posterResult <- case bangumi.tmdbId of
    Just tmdbId -> do
      result <- MetadataService.getTmdbPoster tmdbId
      case result of
        Right url -> pure $ Right ("tmdb", url)
        Left tmdbErr -> tryBgmtvFallback tmdbErr
    Nothing -> tryBgmtvFallback "No TMDB ID"

  case posterResult of
    Left err ->
      Log.logAttention "Failed to fetch poster" $
        object
          [ "bangumi_id" .= bangumi.id
          , "tmdb_id" .= bangumi.tmdbId
          , "bgmtv_id" .= bangumi.bgmtvId
          , "error" .= err
          ]
    Right (source, posterUrl) -> do
      _ <- BangumiRepo.update bangumi.id $
        BangumiRepo.UpdateBangumi
          { posterUrl = Just posterUrl
          , titleChinese = Nothing
          , titleJapanese = Nothing
          , mikanId = Nothing
          , bgmtvId = Nothing
          , tmdbId = Nothing
          , season = Nothing
          , platform = Nothing
          , totalEpisodes = Nothing
          , airDate = Nothing
          , airWeek = Nothing
          }
      Log.logInfo "Updated poster" $
        object
          [ "bangumi_id" .= bangumi.id
          , "source" .= (source :: Text)
          , "poster_url" .= posterUrl
          ]
 where
  tryBgmtvFallback prevErr = case bangumi.bgmtvId of
    Nothing -> pure $ Left prevErr
    Just bgmtvId -> do
      result <- MetadataService.getBgmtvPoster bgmtvId
      pure $ case result of
        Right url -> Right ("bgmtv", url)
        Left bgmtvErr -> Left $ prevErr <> "; BGM.tv: " <> bgmtvErr
