module Web.API.Routes.Series where

import Data.Vector (Vector)
import Servant

import Moe.Series (Series)

type API = NamedRoutes Routes'

data Routes' mode = Routes'
  { getAllSeries
      :: mode
        :- Summary "Get all series"
          :> Get '[JSON] (Vector Series)
  }
  deriving (Generic)
