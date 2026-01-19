module Web.API.Routes.Calendar where

import Moe.Calendar (CalendarDay, Season)
import Moe.Subscription (SubscribedBangumi)
import Servant
import Web.API.Routes.Bangumi.Types (QuickSubscribeDTO)

type API = NamedRoutes Routes'

data Routes' mode = Routes'
  { getCalendar ::
      mode
        :- Summary "Get seasonal calendar"
          :> Description "Get bangumi calendar for a specific year and season"
          :> QueryParam "year" Int
          :> QueryParam "season" Season
          :> Get '[JSON] [CalendarDay]
  , refreshCalendar ::
      mode
        :- Summary "Refresh calendar data"
          :> Description "Fetch fresh data from Mikan and update local calendar"
          :> "refresh"
          :> QueryParam "year" Int
          :> QueryParam "season" Season
          :> Post '[JSON] [CalendarDay]
  , quickSubscribe ::
      mode
        :- Summary "Quick subscribe from calendar"
          :> Description "Create bangumi and subscription in one call with auto-generated Mikan RSS URL"
          :> "quick-subscribe"
          :> ReqBody '[JSON] QuickSubscribeDTO
          :> Post '[JSON] SubscribedBangumi
  }
  deriving (Generic)
