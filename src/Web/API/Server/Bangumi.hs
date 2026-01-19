-- | Bangumi API handlers
module Web.API.Server.Bangumi
  ( bangumiServer
  )
where

import Effectful (IOE, (:>))
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
import Effectful.Reader.Static qualified as Reader
import Infra.Database.Effect (DB)
import Servant hiding ((:>))

import App.Jobs.RssFetch.Job qualified as RssFetch
import Infra.Environment.Env (MoeEnv)

import Moe.Bangumi.GenPath (extractYear, genPathFrom)
import Infra.Database.Repository.Bangumi qualified as BangumiRepo
import Infra.Database.Repository.Subscription qualified as SubRepo
import Infra.Database.Repository.Rss qualified as RssRepo
import App.Bangumi qualified as BangumiService
import Moe.Bangumi (Bangumi (..), Platform (..), SourceType (..))
import Moe.Rss (Rss (..))
import Moe.Subscription (Subscription (..), SubscribedBangumi (..))
import Moe.Monad (MoeM)
import RequireCallStack
import Web.API.Routes.Bangumi qualified as Bangumi
import Web.API.Routes.Bangumi.Types (CreateBangumiDTO (..), UpdateBangumiDTO (..), SubscribeDTO (..), UpdateSubscriptionDTO (..))
import Web.Types (MoeEff)

-- | Bangumi API server implementation
bangumiServer :: RequireCallStack => ServerT Bangumi.API MoeEff
bangumiServer =
  Bangumi.Routes'
    { getAllBangumi = getAllBangumiHandler
    , createBangumi = createBangumiHandler
    , getBangumiById = getBangumiByIdHandler
    , updateBangumi = updateBangumiHandler
    , deleteBangumi = deleteBangumiHandler
    , getAllSubscriptions = getAllSubscriptionsHandler
    , getSubscriptionById = getSubscriptionByIdHandler
    , subscribe = subscribeHandler
    , updateSubscription = updateSubscriptionHandler
    , unsubscribe = unsubscribeHandler
    }

--------------------------------------------------------------------------------
-- Bangumi Handlers
--------------------------------------------------------------------------------

-- | Get all bangumi (metadata only)
getAllBangumiHandler
  :: (DB :> es)
  => MoeM es [Bangumi]
getAllBangumiHandler = BangumiRepo.findAll

-- | Create a new bangumi (metadata only)
createBangumiHandler
  :: (DB :> es)
  => CreateBangumiDTO
  -> MoeM es Bangumi
createBangumiHandler dto =
  BangumiRepo.create (toCreateBangumi dto)

-- | Convert API DTO to Repository DTO
toCreateBangumi :: CreateBangumiDTO -> BangumiRepo.CreateBangumi
toCreateBangumi dto =
  BangumiRepo.CreateBangumi
    { titleChinese = dto.titleChinese
    , titleJapanese = dto.titleJapanese
    , mikanId = dto.mikanId
    , bgmtvId = dto.bgmtvId
    , tmdbId = dto.tmdbId
    , season = fromMaybe 1 dto.season
    , platform = fromMaybe TV dto.platform
    , totalEpisodes = fromMaybe 0 dto.totalEpisodes
    , posterUrl = dto.posterUrl
    , airDate = dto.airDate
    , airWeek = fromMaybe 0 dto.airWeek
    }

-- | Get bangumi by ID
getBangumiByIdHandler
  :: (DB :> es, Error ServerError :> es)
  => Int
  -> MoeM es Bangumi
getBangumiByIdHandler bangumiId = do
  maybeBangumi <- BangumiRepo.getOne bangumiId
  case maybeBangumi of
    Nothing ->
      Error.throwError $
        err404{errBody = "Bangumi not found with ID: " <> show bangumiId}
    Just bangumi -> pure bangumi

-- | Update bangumi metadata
updateBangumiHandler
  :: (DB :> es, Error ServerError :> es)
  => Int
  -> UpdateBangumiDTO
  -> MoeM es Bangumi
updateBangumiHandler bangumiId dto = do
  maybeBangumi <- BangumiRepo.update bangumiId (toUpdateBangumi dto)
  case maybeBangumi of
    Nothing ->
      Error.throwError $
        err404{errBody = "Bangumi not found with ID: " <> show bangumiId}
    Just bangumi -> pure bangumi

-- | Convert API DTO to Repository DTO
toUpdateBangumi :: UpdateBangumiDTO -> BangumiRepo.UpdateBangumi
toUpdateBangumi dto =
  BangumiRepo.UpdateBangumi
    { titleChinese = dto.titleChinese
    , titleJapanese = dto.titleJapanese
    , mikanId = dto.mikanId
    , bgmtvId = dto.bgmtvId
    , tmdbId = dto.tmdbId
    , season = dto.season
    , platform = dto.platform
    , totalEpisodes = dto.totalEpisodes
    , posterUrl = dto.posterUrl
    , airDate = dto.airDate
    , airWeek = dto.airWeek
    }

-- | Delete bangumi
deleteBangumiHandler
  :: (DB :> es)
  => Int
  -> MoeM es NoContent
deleteBangumiHandler bangumiId = do
  BangumiRepo.delete bangumiId
  pure NoContent

--------------------------------------------------------------------------------
-- Subscription Handlers
--------------------------------------------------------------------------------

-- | Get all subscriptions with bangumi and RSS
getAllSubscriptionsHandler
  :: (DB :> es)
  => MoeM es [SubscribedBangumi]
getAllSubscriptionsHandler = do
  subsWithBangumi <- SubRepo.findAllWithBangumi
  forM subsWithBangumi $ \(subscription, bangumi) -> do
    rssEntries <- RssRepo.getBySubscriptionId subscription.id
    pure SubscribedBangumi{subscription, bangumi, rssEntries}

-- | Get subscription by ID
getSubscriptionByIdHandler
  :: (DB :> es, Error ServerError :> es)
  => Int
  -> MoeM es SubscribedBangumi
getSubscriptionByIdHandler subscriptionId = do
  result <- SubRepo.findByIdWithBangumi subscriptionId
  case result of
    Nothing ->
      Error.throwError $
        err404{errBody = "Subscription not found with ID: " <> show subscriptionId}
    Just (subscription, bangumi) -> do
      rssEntries <- RssRepo.getBySubscriptionId subscriptionId
      pure SubscribedBangumi{subscription, bangumi, rssEntries}

-- | Subscribe to a bangumi
subscribeHandler
  :: (DB :> es, Error ServerError :> es, Reader.Reader MoeEnv :> es, IOE :> es)
  => Int
  -> SubscribeDTO
  -> MoeM es SubscribedBangumi
subscribeHandler bangumiId dto = do
  -- Check if already subscribed
  existing <- SubRepo.findByBangumiId bangumiId
  case existing of
    Just _ ->
      Error.throwError $
        err409{errBody = "Already subscribed to bangumi ID: " <> show bangumiId}
    Nothing -> do
      -- Get bangumi to generate save path
      maybeBangumi <- BangumiRepo.getOne bangumiId
      case maybeBangumi of
        Nothing ->
          Error.throwError $
            err404{errBody = "Bangumi not found with ID: " <> show bangumiId}
        Just bangumi -> do
          let savePath' = dto.savePath <|> Just (genPathFrom
                bangumi.titleChinese
                (extractYear bangumi.airDate)
                bangumi.tmdbId
                bangumi.platform
                bangumi.season)
              createSub = SubRepo.CreateSubscription
                { bangumiId = bangumiId
                , currentEpisode = fromMaybe 0 dto.currentEpisode
                , autoComplete = fromMaybe True dto.autoComplete
                , savePath = savePath'
                , sourceType = fromMaybe Other dto.sourceType
                , episodeOffset = fromMaybe 0 dto.episodeOffset
                , embyId = Nothing
                }

          result <- BangumiService.subscribe bangumiId createSub dto.rssUrl
          case result of
            Nothing ->
              Error.throwError $
                err404{errBody = "Bangumi not found with ID: " <> show bangumiId}
            Just subscribedBangumi -> do
              -- Trigger immediate RSS fetch for newly added RSS entries
              env <- Reader.ask @MoeEnv
              forM_ subscribedBangumi.rssEntries $ \rss ->
                liftIO $ RssFetch.triggerRssFetch env rss.id
              pure subscribedBangumi

-- | Update subscription
updateSubscriptionHandler
  :: (DB :> es, Error ServerError :> es)
  => Int
  -> UpdateSubscriptionDTO
  -> MoeM es SubscribedBangumi
updateSubscriptionHandler subscriptionId dto = do
  let updateDto = SubRepo.UpdateSubscription
        { currentEpisode = dto.currentEpisode
        , autoComplete = dto.autoComplete
        , savePath = dto.savePath
        , sourceType = dto.sourceType
        , episodeOffset = dto.episodeOffset
        , embyId = dto.embyId
        }
  result <- SubRepo.update subscriptionId updateDto
  case result of
    Nothing ->
      Error.throwError $
        err404{errBody = "Subscription not found with ID: " <> show subscriptionId}
    Just subscription -> do
      mBangumi <- BangumiRepo.getOne subscription.bangumiId
      case mBangumi of
        Nothing ->
          Error.throwError $
            err500{errBody = "Bangumi not found for subscription"}
        Just bangumi -> do
          rssEntries <- RssRepo.getBySubscriptionId subscriptionId
          pure SubscribedBangumi{subscription, bangumi, rssEntries}

-- | Unsubscribe from a bangumi
unsubscribeHandler
  :: (DB :> es, Error ServerError :> es)
  => Int
  -> MoeM es NoContent
unsubscribeHandler subscriptionId = do
  success <- BangumiService.unsubscribe subscriptionId
  if success
    then pure NoContent
    else Error.throwError $
      err404{errBody = "Subscription not found with ID: " <> show subscriptionId}
