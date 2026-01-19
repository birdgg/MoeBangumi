{-# LANGUAGE QuasiQuotes #-}

module Infra.Database.Repository.Bangumi (
  -- * DTOs
  CreateBangumi (..),
  UpdateBangumi (..),

  -- * Repository Operations
  create,
  createMany,
  findAll,
  findById,
  findByMikanId,
  findByBgmtvId,
  findByTmdbId,
  getOne,
  update,
  delete,
)
where

import Data.Text qualified as T
import Database.SQLite.Simple (NamedParam ((:=)))
import Database.SQLite.Simple.ToField (ToField (..))
import Effectful (Eff, (:>))
import Infra.Database.Effect (DB, Only (..), Query, execute, executeNamed, query, query_)
import Moe.Bangumi (Bangumi (..), Platform (..))
import NeatInterpolation (text)

--------------------------------------------------------------------------------
-- DTOs
--------------------------------------------------------------------------------

-- | Data for creating a new Bangumi
data CreateBangumi = CreateBangumi
  { titleChinese :: Text
  -- ^ Chinese title (required)
  , titleJapanese :: Maybe Text
  -- ^ Japanese original name
  , mikanId :: Maybe Text
  -- ^ Mikan bangumi ID
  , bgmtvId :: Maybe Int
  -- ^ BGM.tv subject ID
  , tmdbId :: Maybe Int
  -- ^ TMDB ID
  , season :: Int
  -- ^ Season number, defaults to 1
  , platform :: Platform
  -- ^ Platform type (TV, Movie, OVA), defaults to TV
  , totalEpisodes :: Int
  -- ^ Total episodes, defaults to 0 (unknown)
  , posterUrl :: Maybe Text
  -- ^ Poster image URL
  , airDate :: Maybe Text
  -- ^ First air date (YYYY-MM-DD format)
  , airWeek :: Int
  -- ^ Day of week when new episodes air (0=Sunday ~ 6=Saturday)
  }
  deriving stock (Show, Eq, Generic)

{- | Data for updating a Bangumi

All fields are optional. Nothing means "don't update this field".
-}
data UpdateBangumi = UpdateBangumi
  { titleChinese :: Maybe Text
  , titleJapanese :: Maybe Text
  , mikanId :: Maybe Text
  , bgmtvId :: Maybe Int
  , tmdbId :: Maybe Int
  , season :: Maybe Int
  , platform :: Maybe Platform
  , totalEpisodes :: Maybe Int
  , posterUrl :: Maybe Text
  , airDate :: Maybe Text
  , airWeek :: Maybe Int
  }
  deriving stock (Show, Eq, Generic)

--------------------------------------------------------------------------------
-- SQL Queries
--------------------------------------------------------------------------------

-- | SQL query for selecting all bangumi columns
selectBangumiSql :: Query
selectBangumiSql =
  toQuery
    [text|
      SELECT
        id, created_at, updated_at, mikan_id, bgmtv_id, tmdb_id,
        title_chinese, title_japanese, season, platform, total_episodes,
        poster_url, air_date, air_week, tmdb_lookup_at
      FROM bangumi
    |]

insertBangumiSql :: Query
insertBangumiSql =
  toQuery
    [text|
      INSERT INTO bangumi (
        title_chinese, title_japanese, mikan_id, bgmtv_id, tmdb_id,
        season, platform, total_episodes, poster_url, air_date, air_week
      ) VALUES (
        :title_chinese, :title_japanese, :mikan_id, :bgmtv_id, :tmdb_id,
        :season, :platform, :total_episodes, :poster_url, :air_date, :air_week
      )
    |]

--------------------------------------------------------------------------------
-- Repository Operations
--------------------------------------------------------------------------------

{- | Create a new Bangumi

Creates a new Bangumi and returns it with generated id and timestamps.
-}
create :: (DB :> es) => CreateBangumi -> Eff es Bangumi
create dto = do
  executeNamed
    insertBangumiSql
    [ ":title_chinese" := dto.titleChinese
    , ":title_japanese" := dto.titleJapanese
    , ":mikan_id" := dto.mikanId
    , ":bgmtv_id" := dto.bgmtvId
    , ":tmdb_id" := dto.tmdbId
    , ":season" := dto.season
    , ":platform" := dto.platform
    , ":total_episodes" := dto.totalEpisodes
    , ":poster_url" := dto.posterUrl
    , ":air_date" := dto.airDate
    , ":air_week" := dto.airWeek
    ]
  rows <- query_ (selectBangumiSql <> " WHERE id = last_insert_rowid()")
  case rows of
    [bangumi] -> pure bangumi
    _ -> error "create: unexpected result after insert"

{- | Create multiple Bangumi

Creates multiple Bangumi and returns them with generated ids and timestamps.
-}
createMany :: (DB :> es) => [CreateBangumi] -> Eff es [Bangumi]
createMany = mapM create

-- | Get all Bangumi ordered by creation time (newest first)
findAll :: (DB :> es) => Eff es [Bangumi]
findAll = query_ (selectBangumiSql <> " ORDER BY created_at DESC")

-- | Find Bangumi by ID (alias for getOne)
findById :: (DB :> es) => Int -> Eff es (Maybe Bangumi)
findById = getOne

{- | Find Bangumi by Mikan ID

Returns Nothing if no Bangumi has the given Mikan ID.
-}
findByMikanId :: (DB :> es) => Text -> Eff es (Maybe Bangumi)
findByMikanId mikanId = do
  rows <- query (selectBangumiSql <> " WHERE mikan_id = ?") (Only mikanId)
  pure $ listToMaybe rows

{- | Find Bangumi by BGM.tv ID

Returns Nothing if no Bangumi has the given BGM.tv ID.
-}
findByBgmtvId :: (DB :> es) => Int -> Eff es (Maybe Bangumi)
findByBgmtvId bgmtvId = do
  rows <- query (selectBangumiSql <> " WHERE bgmtv_id = ?") (Only bgmtvId)
  pure $ listToMaybe rows

{- | Find Bangumi by TMDB ID

Returns Nothing if no Bangumi has the given TMDB ID.
-}
findByTmdbId :: (DB :> es) => Int -> Eff es (Maybe Bangumi)
findByTmdbId tmdbId = do
  rows <- query (selectBangumiSql <> " WHERE tmdb_id = ?") (Only tmdbId)
  pure $ listToMaybe rows

{- | Get a single Bangumi by ID

Returns Nothing if the Bangumi doesn't exist.
-}
getOne :: (DB :> es) => Int -> Eff es (Maybe Bangumi)
getOne bangumiId = do
  rows <- query (selectBangumiSql <> " WHERE id = ?") (Only bangumiId)
  pure $ listToMaybe rows

{- | Update a Bangumi by ID

Only updates fields that are Just. Returns Nothing if the Bangumi doesn't exist.
Returns the updated Bangumi on success.
-}
update :: (DB :> es) => Int -> UpdateBangumi -> Eff es (Maybe Bangumi)
update bangumiId dto = do
  let namedParams =
        catMaybes
          [ mkParam ":title_chinese" dto.titleChinese
          , mkParam ":title_japanese" dto.titleJapanese
          , mkParam ":mikan_id" dto.mikanId
          , mkParam ":bgmtv_id" dto.bgmtvId
          , mkParam ":tmdb_id" dto.tmdbId
          , mkParam ":season" dto.season
          , mkParam ":platform" dto.platform
          , mkParam ":total_episodes" dto.totalEpisodes
          , mkParam ":poster_url" dto.posterUrl
          , mkParam ":air_date" dto.airDate
          , mkParam ":air_week" dto.airWeek
          ]
  case namedParams of
    [] -> getOne bangumiId -- No updates, just return current
    _ -> do
      let setClauses = map paramToSetClause namedParams
          setClause = T.intercalate ", " setClauses
          sql = toQuery $ "UPDATE bangumi SET " <> setClause <> ", updated_at = CURRENT_TIMESTAMP WHERE id = :id"
          allParams = (":id" := bangumiId) : namedParams
      executeNamed sql allParams
      getOne bangumiId
 where
  mkParam :: (ToField a) => Text -> Maybe a -> Maybe NamedParam
  mkParam name = fmap (name :=)

  paramToSetClause :: NamedParam -> Text
  paramToSetClause (name := _) = columnFromParam name <> " = " <> name

  columnFromParam :: Text -> Text
  columnFromParam name = T.replace ":" "" name

-- | Delete a Bangumi by ID
delete :: (DB :> es) => Int -> Eff es ()
delete bangumiId =
  execute "DELETE FROM bangumi WHERE id = ?" (Only bangumiId)

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Convert Text to Query
toQuery :: Text -> Query
toQuery = fromString . T.unpack
