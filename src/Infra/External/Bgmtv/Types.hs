-- | Types for BGM.tv API
module Infra.External.Bgmtv.Types
  ( -- * Enums
    SubjectType (..)
  , EpisodeType (..)

    -- * Search
  , SearchSubjectsRequest (..)
  , SearchFilter (..)
  , SearchSubjectsResponse (..)

    -- * Subject
  , Subject (..)
  , SubjectDetail (..)
  , SubjectImages (..)

    -- * Episode
  , Episode (..)
  , EpisodesResponse (..)

    -- * Errors
  , BgmtvError (..)
  )
where

import Data.Aeson

-- | BGM.tv subject type
data SubjectType
  = Book
  | Anime
  | Music
  | Game
  | Real
  deriving stock (Show, Eq, Generic)

instance ToJSON SubjectType where
  toJSON = \case
    Book -> Number 1
    Anime -> Number 2
    Music -> Number 3
    Game -> Number 4
    Real -> Number 6

instance FromJSON SubjectType where
  parseJSON = withScientific "SubjectType" $ \n ->
    case round n :: Int of
      1 -> pure Book
      2 -> pure Anime
      3 -> pure Music
      4 -> pure Game
      6 -> pure Real
      _ -> fail $ "Unknown SubjectType: " <> show n

-- | Episode type
data EpisodeType
  = -- | 本篇
    Main
  | -- | SP
    Special
  | -- | OP
    Opening
  | -- | ED
    Ending
  deriving stock (Show, Eq, Generic)

instance ToJSON EpisodeType where
  toJSON = \case
    Main -> Number 0
    Special -> Number 1
    Opening -> Number 2
    Ending -> Number 3

instance FromJSON EpisodeType where
  parseJSON = withScientific "EpisodeType" $ \n ->
    case round n :: Int of
      0 -> pure Main
      1 -> pure Special
      2 -> pure Opening
      3 -> pure Ending
      _ -> fail $ "Unknown EpisodeType: " <> show n

-- | Search request body for POST /v0/search/subjects
data SearchSubjectsRequest = SearchSubjectsRequest
  { keyword :: Text
  , filter :: Maybe SearchFilter
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON SearchSubjectsRequest where
  toJSON req =
    object $
      ["keyword" .= req.keyword]
        <> maybe [] (\f -> ["filter" .= f]) req.filter

-- | Search filter options
data SearchFilter = SearchFilter
  { subjectType :: Maybe [SubjectType]
  , metaTags :: Maybe [Text]
  , airDate :: Maybe [Text]
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON SearchFilter where
  toJSON f =
    object $
      catMaybes
        [ ("type" .=) <$> f.subjectType
        , ("meta_tags" .=) <$> f.metaTags
        , ("air_date" .=) <$> f.airDate
        ]
    where
      catMaybes = foldr (\x acc -> maybe acc (: acc) x) []

-- | Search response from POST /v0/search/subjects
data SearchSubjectsResponse = SearchSubjectsResponse
  { total :: Int64
  , limit :: Int64
  , offset :: Int64
  , data_ :: [Subject]
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON SearchSubjectsResponse where
  parseJSON = withObject "SearchSubjectsResponse" $ \o ->
    SearchSubjectsResponse
      <$> o .: "total"
      <*> o .: "limit"
      <*> o .: "offset"
      <*> o .: "data"

-- | Subject images from BGM.tv API
data SubjectImages = SubjectImages
  { small :: Text
  , grid :: Text
  , large :: Text
  , medium :: Text
  , common :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Subject item in search results
data Subject = Subject
  { id :: Int64
  , name :: Text
  , nameCn :: Text
  , date :: Text
  , platform :: Text
  , images :: SubjectImages
  , image :: Text
  , eps :: Int64
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON Subject where
  parseJSON = withObject "Subject" $ \o ->
    Subject
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "name_cn"
      <*> o .: "date"
      <*> o .: "platform"
      <*> o .: "images"
      <*> o .: "image"
      <*> o .: "eps"

-- | Detailed subject information from GET /v0/subjects/{id}
data SubjectDetail = SubjectDetail
  { id :: Int64
  , subjectType :: SubjectType
  , name :: Text
  , nameCn :: Text
  , summary :: Text
  , nsfw :: Bool
  , locked :: Bool
  , date :: Maybe Text
  , platform :: Maybe Text
  , images :: SubjectImages
  , volumes :: Int64
  , eps :: Int64
  , totalEpisodes :: Int64
  , metaTags :: [Text]
  , series :: Bool
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON SubjectDetail where
  parseJSON = withObject "SubjectDetail" $ \o ->
    SubjectDetail
      <$> o .: "id"
      <*> o .: "type"
      <*> o .: "name"
      <*> o .: "name_cn"
      <*> o .: "summary"
      <*> o .: "nsfw"
      <*> o .: "locked"
      <*> o .:? "date"
      <*> o .:? "platform"
      <*> o .: "images"
      <*> o .: "volumes"
      <*> o .: "eps"
      <*> o .: "total_episodes"
      <*> o .:? "meta_tags" .!= []
      <*> o .: "series"

-- | Episode item
data Episode = Episode
  { id :: Int64
  , episodeType :: EpisodeType
  , name :: Text
  , nameCn :: Text
  , sort :: Double
  , ep :: Maybe Double
  , airdate :: Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON Episode where
  parseJSON = withObject "Episode" $ \o ->
    Episode
      <$> o .: "id"
      <*> o .: "type"
      <*> o .: "name"
      <*> o .: "name_cn"
      <*> o .: "sort"
      <*> o .:? "ep"
      <*> o .: "airdate"

-- | Episodes API response from GET /v0/episodes
data EpisodesResponse = EpisodesResponse
  { data_ :: [Episode]
  , total :: Int64
  , limit :: Int64
  , offset :: Int64
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON EpisodesResponse where
  parseJSON = withObject "EpisodesResponse" $ \o ->
    EpisodesResponse
      <$> o .: "data"
      <*> o .: "total"
      <*> o .: "limit"
      <*> o .: "offset"

-- | Errors that can occur during BGM.tv API operations
data BgmtvError
  = -- | Network request failed
    NetworkError Text
  | -- | Failed to parse JSON response
    ParseError Text
  | -- | API returned an error status (status code, message)
    ApiError Int Text
  deriving stock (Show, Eq)
