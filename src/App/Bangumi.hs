{- | Bangumi service for business logic orchestration

This module provides high-level business operations that coordinate
multiple repositories and handle transactions.
-}
module App.Bangumi (
  -- * Subscription Operations
  subscribe,
  unsubscribe,
)
where

import Effectful (Eff, (:>))
import Infra.Database.Effect (DB, withTransaction)

import Infra.Database.Repository.Bangumi qualified as BangumiRepo
import Infra.Database.Repository.Subscription qualified as SubRepo
import Infra.Database.Repository.Rss qualified as RssRepo
import Moe.Subscription (Subscription (..), SubscribedBangumi (..))

--------------------------------------------------------------------------------
-- Service Operations
--------------------------------------------------------------------------------

{- | Subscribe to a Bangumi with optional RSS URL

This operation is transactional - if RSS creation fails, the Subscription
creation will be rolled back.

Returns the created SubscribedBangumi (subscription + bangumi + RSS entries).
-}
subscribe ::
  (DB :> es) =>
  -- | Bangumi ID to subscribe to
  Int ->
  -- | Subscription settings
  SubRepo.CreateSubscription ->
  -- | Optional RSS URL to create subscription
  Maybe Text ->
  Eff es (Maybe SubscribedBangumi)
subscribe bangumiId createSub rssUrl = do
  -- Check if bangumi exists
  mBangumi <- BangumiRepo.getOne bangumiId
  case mBangumi of
    Nothing -> pure Nothing
    Just bangumi -> withTransaction $ do
      -- Create the Subscription
      subscription <- SubRepo.create createSub

      -- Optionally create RSS if URL provided
      rssEntries <- case rssUrl of
        Nothing -> pure []
        Just url -> do
          rss <-
            RssRepo.create $
              RssRepo.CreateRss
                { subscriptionId = subscription.id
                , url = url
                , enabled = True
                }
          pure [rss]

      pure $
        Just
          SubscribedBangumi
            { subscription = subscription
            , bangumi = bangumi
            , rssEntries = rssEntries
            }

{- | Unsubscribe from a Bangumi

Deletes the subscription and all associated RSS entries (via CASCADE).
Returns True if successful, False if subscription not found.
-}
unsubscribe ::
  (DB :> es) =>
  -- | Subscription ID to delete
  Int ->
  Eff es Bool
unsubscribe subscriptionId = do
  mSub <- SubRepo.getOne subscriptionId
  case mSub of
    Nothing -> pure False
    Just _ -> do
      SubRepo.delete subscriptionId
      pure True
