module Web.API.Routes.Bangumi where

import Servant

import Moe.Bangumi (Bangumi)
import Moe.Subscription (SubscribedBangumi)
import Web.API.Routes.Bangumi.Types (CreateBangumiDTO, UpdateBangumiDTO, SubscribeDTO, UpdateSubscriptionDTO)

type API = NamedRoutes Routes'

data Routes' mode = Routes'
  { -- Bangumi (metadata) endpoints
    getAllBangumi
      :: mode
        :- Summary "Get all bangumi metadata"
          :> Get '[JSON] [Bangumi]
  , createBangumi
      :: mode
        :- Summary "Create a new bangumi (metadata only)"
          :> ReqBody '[JSON] CreateBangumiDTO
          :> Post '[JSON] Bangumi
  , getBangumiById
      :: mode
        :- Summary "Get bangumi by ID"
          :> Capture "id" Int
          :> Get '[JSON] Bangumi
  , updateBangumi
      :: mode
        :- Summary "Update bangumi metadata"
          :> Capture "id" Int
          :> ReqBody '[JSON] UpdateBangumiDTO
          :> Patch '[JSON] Bangumi
  , deleteBangumi
      :: mode
        :- Summary "Delete bangumi"
          :> Capture "id" Int
          :> DeleteNoContent
  -- Subscription endpoints
  , getAllSubscriptions
      :: mode
        :- "subscriptions"
          :> Summary "Get all subscribed bangumi"
          :> Get '[JSON] [SubscribedBangumi]
  , getSubscriptionById
      :: mode
        :- "subscriptions"
          :> Summary "Get subscription by ID"
          :> Capture "subscriptionId" Int
          :> Get '[JSON] SubscribedBangumi
  , subscribe
      :: mode
        :- Summary "Subscribe to a bangumi"
          :> Capture "id" Int
          :> "subscribe"
          :> ReqBody '[JSON] SubscribeDTO
          :> Post '[JSON] SubscribedBangumi
  , updateSubscription
      :: mode
        :- "subscriptions"
          :> Summary "Update a subscription"
          :> Capture "subscriptionId" Int
          :> ReqBody '[JSON] UpdateSubscriptionDTO
          :> Patch '[JSON] SubscribedBangumi
  , unsubscribe
      :: mode
        :- "subscriptions"
          :> Summary "Unsubscribe from a bangumi"
          :> Capture "subscriptionId" Int
          :> DeleteNoContent
  }
  deriving (Generic)
