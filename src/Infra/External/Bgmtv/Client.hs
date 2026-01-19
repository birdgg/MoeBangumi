-- | BGM.tv API client using servant-client
--
-- This module provides ClientM functions for interacting with the BGM.tv API.
--
-- == Usage
--
-- @
-- import Network.HTTP.Client (newManager)
-- import Network.HTTP.Client.TLS (tlsManagerSettings)
-- import Infra.External.Bgmtv.Client
-- import Servant.Client (mkClientEnv, runClientM)
--
-- main :: IO ()
-- main = do
--   manager <- newManager tlsManagerSettings
--   let env = mkClientEnv manager bgmtvBaseUrl
--   result <- runClientM (searchBangumi "葬送的芙莉蓮") env
--   case result of
--     Right subjects -> print subjects
--     Left err -> print err
-- @
module Infra.External.Bgmtv.Client
  ( -- * API Functions
    searchSubjects
  , searchBangumi
  , getSubject
  , getEpisodes
  , getEpisodesWithPagination

    -- * Re-exports
  , module Infra.External.Bgmtv.Types
  , bgmtvBaseUrl
  , userAgent
  )
where

import Infra.External.Bgmtv.API (BgmtvRoutes, bgmtvBaseUrl, userAgent)
import Infra.External.Bgmtv.API qualified as API
import Infra.External.Bgmtv.Types
import Servant.Client (ClientM)
import Servant.Client.Generic (AsClientT, genericClient)

-- | Generated client functions record (internal)
client' :: BgmtvRoutes (AsClientT ClientM)
client' = genericClient

-- | Search subjects with custom request
searchSubjects :: SearchSubjectsRequest -> ClientM SearchSubjectsResponse
searchSubjects req = API.searchSubjects client' userAgent req

-- | Search for Japanese anime (convenience method)
searchBangumi :: Text -> ClientM [Subject]
searchBangumi keyword = do
  let req =
        SearchSubjectsRequest
          { keyword = keyword
          , filter =
              Just
                SearchFilter
                  { subjectType = Just [Anime]
                  , metaTags = Just ["日本"]
                  , airDate = Nothing
                  }
          }
  response <- searchSubjects req
  pure response.data_

-- | Get subject details by ID
getSubject :: Int64 -> ClientM SubjectDetail
getSubject subjectId = API.getSubject client' subjectId userAgent

-- | Get episodes by subject ID
getEpisodes :: Int64 -> ClientM EpisodesResponse
getEpisodes subjectId =
  API.getEpisodes client' userAgent subjectId Nothing Nothing

-- | Get episodes with pagination
getEpisodesWithPagination
  :: Int64
  -> Maybe Int64
  -> Maybe Int64
  -> ClientM EpisodesResponse
getEpisodesWithPagination subjectId limit offset =
  API.getEpisodes client' userAgent subjectId limit offset
