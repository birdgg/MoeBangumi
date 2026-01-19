-- | Series API handlers
module Web.API.Server.Series
  ( seriesServer
  )
where

import Data.Vector qualified as V
import Effectful

import Infra.Database.Repository.Series qualified as SeriesRepo
import Moe.Series (Series)
import Infra.Database.Effect (DB)
import Web.API.Routes.Series qualified as Series
import Web.Types (MoeEff)
import Moe.Monad (MoeM)
import RequireCallStack
import Servant hiding ((:>))

-- | Series API server implementation
seriesServer :: RequireCallStack => ServerT Series.API MoeEff
seriesServer =
  Series.Routes'
    { getAllSeries = getAllSeriesHandler
    }

-- | Get all series handler
getAllSeriesHandler :: (DB :> es) => (MoeM es) (V.Vector Series)
getAllSeriesHandler = V.fromList <$> SeriesRepo.findAll
