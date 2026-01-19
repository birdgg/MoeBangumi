{- | Calendar API handlers

Implements the calendar API endpoints for viewing and refreshing
seasonal bangumi calendar.
-}
module Web.API.Server.Calendar (
  calendarServer,
) where

import Effectful (IOE, (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
import Effectful.Log (Log)
import Effectful.Reader.Static qualified as Reader
import Servant hiding ((:>))

import App.Bangumi qualified as BangumiService
import App.Calendar.Sync (getCurrentSeason, getCalendarDays, syncCalendar)
import App.Metadata.Service qualified as MetadataService
import Infra.Database.Effect (DB)
import Infra.Database.Repository.Bangumi qualified as BangumiRepo
import Infra.Database.Repository.Subscription qualified as SubRepo
import Infra.Environment.Env (MoeEnv)
import Moe.Bangumi (Bangumi (..), SourceType (..))
import Moe.Bangumi.GenPath (extractYear, genPathFrom)
import Moe.Calendar (CalendarDay, Season)
import Moe.Monad (MoeM)
import Moe.Subscription (SubscribedBangumi)
import RequireCallStack
import Web.API.Routes.Bangumi.Types (QuickSubscribeDTO (..))
import Web.API.Routes.Calendar qualified as CalendarRoutes
import Web.Types (MoeEff)

-- | Calendar API server implementation
calendarServer :: (RequireCallStack) => ServerT CalendarRoutes.API MoeEff
calendarServer =
  CalendarRoutes.Routes'
    { getCalendar = getCalendarHandler
    , refreshCalendar = refreshCalendarHandler
    , quickSubscribe = quickSubscribeHandler
    }

-- | Get calendar for a specific year and season
-- If year/season not provided, uses current season
-- If database is empty for requested season, auto-sync from bangumi-data
getCalendarHandler ::
  ( DB :> es
  , Reader.Reader MoeEnv :> es
  , Log :> es
  , IOE :> es
  ) =>
  Maybe Int ->
  Maybe Season ->
  MoeM es [CalendarDay]
getCalendarHandler mYear mSeason = do
  (defaultYear, defaultSeason) <- liftIO getCurrentSeason
  let year = fromMaybe defaultYear mYear
      season = fromMaybe defaultSeason mSeason
  days <- getCalendarDays year season
  if null days
    then syncCalendar year season
    else pure days

-- | Refresh calendar data from Mikan
refreshCalendarHandler ::
  ( DB :> es
  , Reader.Reader MoeEnv :> es
  , Error ServerError :> es
  , Log :> es
  , Concurrent :> es
  , IOE :> es
  ) =>
  Maybe Int ->
  Maybe Season ->
  MoeM es [CalendarDay]
refreshCalendarHandler mYear mSeason = do
  (defaultYear, defaultSeason) <- liftIO getCurrentSeason
  let year = fromMaybe defaultYear mYear
      season = fromMaybe defaultSeason mSeason
  syncCalendar year season

-- | Quick subscribe from calendar
-- Creates bangumi and subscription in one call with auto-generated Mikan RSS URL
quickSubscribeHandler ::
  ( DB :> es
  , Reader.Reader MoeEnv :> es
  , Error ServerError :> es
  , IOE :> es
  ) =>
  QuickSubscribeDTO ->
  MoeM es SubscribedBangumi
quickSubscribeHandler dto = do
  -- Check if bangumi already exists by mikanId
  existingByMikan <- BangumiRepo.findByMikanId dto.mikanId

  -- Get or create bangumi
  bangumi <- case existingByMikan of
    Just existingBangumi -> do
      -- Check if already subscribed
      existingSub <- SubRepo.findByBangumiId existingBangumi.id
      case existingSub of
        Just _ ->
          Error.throwError $
            err409{errBody = "Already subscribed to this bangumi"}
        Nothing -> pure existingBangumi
    Nothing -> do
      -- Convert airWeek from 1-7 (Mon-Sun) to 0-6 (Sun-Sat) for storage
      let airWeekConverted = if dto.airWeek == 7 then 0 else dto.airWeek
          createBangumi = BangumiRepo.CreateBangumi
            { titleChinese = dto.titleChinese
            , titleJapanese = dto.titleJapanese
            , mikanId = Just dto.mikanId
            , bgmtvId = dto.bgmtvId
            , tmdbId = Nothing
            , season = dto.season
            , platform = dto.platform
            , totalEpisodes = dto.totalEpisodes
            , posterUrl = dto.posterUrl
            , airDate = dto.airDate
            , airWeek = airWeekConverted
            }
      BangumiRepo.create createBangumi

  -- Get episode offset from BGM.tv if bgmtvId is available
  episodeOffset <- case dto.bgmtvId of
    Just bgmtvId -> do
      mOffset <- MetadataService.getEpisodeOffset (fromIntegral bgmtvId)
      pure $ maybe 0 round mOffset
    Nothing -> pure 0

  -- Generate save path and Mikan RSS URL
  let savePath = genPathFrom
        bangumi.titleChinese
        (extractYear bangumi.airDate)
        bangumi.tmdbId
        bangumi.platform
        bangumi.season
      mikanRssUrl = "https://mikanani.me/RSS/Bangumi?bangumiId=" <> dto.mikanId

  -- Create subscription with RSS
  let createSub = SubRepo.CreateSubscription
        { bangumiId = bangumi.id
        , currentEpisode = 0
        , autoComplete = True
        , savePath = Just savePath
        , sourceType = Other
        , episodeOffset = episodeOffset
        , embyId = Nothing
        }

  result <- BangumiService.subscribe bangumi.id createSub (Just mikanRssUrl)
  case result of
    Nothing ->
      Error.throwError $
        err500{errBody = "Failed to create subscription"}
    Just subscribedBangumi -> pure subscribedBangumi
