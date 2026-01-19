-- | BGM.tv API definition for servant-client
module Infra.External.Bgmtv.API
  ( -- * API Types
    BgmtvAPI
  , BgmtvRoutes (..)

    -- * Configuration
  , bgmtvBaseUrl
  , userAgent
  )
where

import Infra.External.Bgmtv.Types
import Servant.API
import Servant.Client (BaseUrl (..), Scheme (..))

-- | BGM.tv API v0
type BgmtvAPI = NamedRoutes BgmtvRoutes

-- | BGM.tv API routes using NamedRoutes pattern
data BgmtvRoutes mode = BgmtvRoutes
  { searchSubjects
      :: mode
        :- "v0"
          :> "search"
          :> "subjects"
          :> Header' '[Required, Strict] "User-Agent" Text
          :> ReqBody '[JSON] SearchSubjectsRequest
          :> Post '[JSON] SearchSubjectsResponse
  , getSubject
      :: mode
        :- "v0"
          :> "subjects"
          :> Capture "id" Int64
          :> Header' '[Required, Strict] "User-Agent" Text
          :> Get '[JSON] SubjectDetail
  , getEpisodes
      :: mode
        :- "v0"
          :> "episodes"
          :> Header' '[Required, Strict] "User-Agent" Text
          :> QueryParam' '[Required, Strict] "subject_id" Int64
          :> QueryParam "limit" Int64
          :> QueryParam "offset" Int64
          :> Get '[JSON] EpisodesResponse
  }
  deriving stock (Generic)

-- | Base URL for BGM.tv API
bgmtvBaseUrl :: BaseUrl
bgmtvBaseUrl = BaseUrl Https "api.bgm.tv" 443 ""

-- | User-Agent header value
userAgent :: Text
userAgent = "birdgg/moe-bangumi"
