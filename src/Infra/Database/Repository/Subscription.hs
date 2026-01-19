{-# LANGUAGE QuasiQuotes #-}

module Infra.Database.Repository.Subscription (
  -- * DTOs
  CreateSubscription (..),
  UpdateSubscription (..),

  -- * Repository Operations
  create,
  findAll,
  findById,
  findByBangumiId,
  findByEmbyId,
  getOne,
  update,
  delete,

  -- * Composite Queries
  findAllWithBangumi,
  findByIdWithBangumi,
)
where

import Data.Text qualified as T
import Database.SQLite.Simple (FromRow (..), NamedParam ((:=)), field)
import Database.SQLite.Simple.ToField (ToField (..))
import Effectful (Eff, (:>))
import Infra.Database.Effect (DB, Only (..), Query, execute, executeNamed, query, query_)
import Moe.Bangumi (Bangumi (..), SourceType (..))
import Moe.Subscription (Subscription (..))
import NeatInterpolation (text)

--------------------------------------------------------------------------------
-- DTOs
--------------------------------------------------------------------------------

-- | Data for creating a new Subscription
data CreateSubscription = CreateSubscription
  { bangumiId :: Int
  -- ^ Reference to the bangumi being tracked (required)
  , currentEpisode :: Int
  -- ^ Current downloaded episode
  , autoComplete :: Bool
  -- ^ Only download first matching episode per RSS check
  , savePath :: Maybe Text
  -- ^ Save path (required for downloading)
  , sourceType :: SourceType
  -- ^ Source type: other or bdrip
  , episodeOffset :: Int
  -- ^ Episode offset for season-relative numbering
  , embyId :: Maybe Text
  -- ^ Emby item ID
  }
  deriving stock (Show, Eq, Generic)

{- | Data for updating a Subscription

All fields are optional. Nothing means "don't update this field".
-}
data UpdateSubscription = UpdateSubscription
  { currentEpisode :: Maybe Int
  , autoComplete :: Maybe Bool
  , savePath :: Maybe Text
  , sourceType :: Maybe SourceType
  , episodeOffset :: Maybe Int
  , embyId :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

--------------------------------------------------------------------------------
-- SQL Queries
--------------------------------------------------------------------------------

selectSubscriptionSql :: Query
selectSubscriptionSql =
  toQuery
    [text|
      SELECT
        id, created_at, updated_at, bangumi_id, current_episode,
        auto_complete, save_path, source_type, episode_offset, emby_id
      FROM subscription
    |]

insertSubscriptionSql :: Query
insertSubscriptionSql =
  toQuery
    [text|
      INSERT INTO subscription (
        bangumi_id, current_episode, auto_complete, save_path,
        source_type, episode_offset, emby_id
      ) VALUES (
        :bangumi_id, :current_episode, :auto_complete, :save_path,
        :source_type, :episode_offset, :emby_id
      )
    |]

--------------------------------------------------------------------------------
-- Repository Operations
--------------------------------------------------------------------------------

{- | Create a new Subscription

Creates a new Subscription and returns it with generated id and timestamps.
-}
create :: (DB :> es) => CreateSubscription -> Eff es Subscription
create dto = do
  executeNamed
    insertSubscriptionSql
    [ ":bangumi_id" := dto.bangumiId
    , ":current_episode" := dto.currentEpisode
    , ":auto_complete" := boolToInt dto.autoComplete
    , ":save_path" := dto.savePath
    , ":source_type" := dto.sourceType
    , ":episode_offset" := dto.episodeOffset
    , ":emby_id" := dto.embyId
    ]
  rows <- query_ (selectSubscriptionSql <> " WHERE id = last_insert_rowid()")
  case rows of
    [sub] -> pure sub
    _ -> error "create: unexpected result after insert"

-- | Get all Subscriptions ordered by creation time (newest first)
findAll :: (DB :> es) => Eff es [Subscription]
findAll = query_ (selectSubscriptionSql <> " ORDER BY created_at DESC")

-- | Find Subscription by ID (alias for getOne)
findById :: (DB :> es) => Int -> Eff es (Maybe Subscription)
findById = getOne

{- | Find Subscription by Bangumi ID

Returns Nothing if no Subscription exists for the given Bangumi.
-}
findByBangumiId :: (DB :> es) => Int -> Eff es (Maybe Subscription)
findByBangumiId bangumiId = do
  rows <- query (selectSubscriptionSql <> " WHERE bangumi_id = ?") (Only bangumiId)
  pure $ listToMaybe rows

{- | Find Subscription by Emby ID

Returns Nothing if no Subscription has the given Emby ID.
-}
findByEmbyId :: (DB :> es) => Text -> Eff es (Maybe Subscription)
findByEmbyId embyId = do
  rows <- query (selectSubscriptionSql <> " WHERE emby_id = ?") (Only embyId)
  pure $ listToMaybe rows

{- | Get a single Subscription by ID

Returns Nothing if the Subscription doesn't exist.
-}
getOne :: (DB :> es) => Int -> Eff es (Maybe Subscription)
getOne subId = do
  rows <- query (selectSubscriptionSql <> " WHERE id = ?") (Only subId)
  pure $ listToMaybe rows

{- | Update a Subscription by ID

Only updates fields that are Just. Returns Nothing if the Subscription doesn't exist.
Returns the updated Subscription on success.
-}
update :: (DB :> es) => Int -> UpdateSubscription -> Eff es (Maybe Subscription)
update subId dto = do
  let namedParams =
        catMaybes
          [ mkParam ":current_episode" dto.currentEpisode
          , mkParamBool ":auto_complete" dto.autoComplete
          , mkParam ":save_path" dto.savePath
          , mkParam ":source_type" dto.sourceType
          , mkParam ":episode_offset" dto.episodeOffset
          , mkParam ":emby_id" dto.embyId
          ]
  case namedParams of
    [] -> getOne subId -- No updates, just return current
    _ -> do
      let setClauses = map paramToSetClause namedParams
          setClause = T.intercalate ", " setClauses
          sql = toQuery $ "UPDATE subscription SET " <> setClause <> ", updated_at = CURRENT_TIMESTAMP WHERE id = :id"
          allParams = (":id" := subId) : namedParams
      executeNamed sql allParams
      getOne subId
 where
  mkParam :: (ToField a) => Text -> Maybe a -> Maybe NamedParam
  mkParam name = fmap (name :=)

  mkParamBool :: Text -> Maybe Bool -> Maybe NamedParam
  mkParamBool name = fmap (\v -> name := boolToInt v)

  paramToSetClause :: NamedParam -> Text
  paramToSetClause (name := _) = T.replace ":" "" name <> " = " <> name

-- | Delete a Subscription by ID
delete :: (DB :> es) => Int -> Eff es ()
delete subId =
  execute "DELETE FROM subscription WHERE id = ?" (Only subId)

--------------------------------------------------------------------------------
-- Composite Queries
--------------------------------------------------------------------------------

-- | Subscription with Bangumi metadata (for JOIN queries)
data SubscriptionWithBangumi = SubscriptionWithBangumi
  { subscription :: Subscription
  , bangumi :: Bangumi
  }
  deriving stock (Show, Eq, Generic)

instance FromRow SubscriptionWithBangumi where
  fromRow = do
    -- Subscription fields
    subId <- field
    subCreatedAt <- field
    subUpdatedAt <- field
    bangumiId <- field
    currentEpisode <- field
    autoComplete <- (/= (0 :: Int)) <$> field
    savePath <- field
    sourceType <- field
    episodeOffset <- field
    embyId <- field
    -- Bangumi fields
    bgId <- field
    bgCreatedAt <- field
    bgUpdatedAt <- field
    mikanId <- field
    bgmtvId <- field
    tmdbId <- field
    titleChinese <- field
    titleJapanese <- field
    season <- field
    platform <- field
    totalEpisodes <- field
    posterUrl <- field
    airDate <- field
    airWeek <- field
    tmdbLookupAt <- field
    pure
      SubscriptionWithBangumi
        { subscription =
            Subscription
              { id = subId
              , createdAt = subCreatedAt
              , updatedAt = subUpdatedAt
              , bangumiId = bangumiId
              , currentEpisode = currentEpisode
              , autoComplete = autoComplete
              , savePath = savePath
              , sourceType = sourceType
              , episodeOffset = episodeOffset
              , embyId = embyId
              }
        , bangumi =
            Bangumi
              { id = bgId
              , createdAt = bgCreatedAt
              , updatedAt = bgUpdatedAt
              , mikanId = mikanId
              , bgmtvId = bgmtvId
              , tmdbId = tmdbId
              , titleChinese = titleChinese
              , titleJapanese = titleJapanese
              , season = season
              , platform = platform
              , totalEpisodes = totalEpisodes
              , posterUrl = posterUrl
              , airDate = airDate
              , airWeek = airWeek
              , tmdbLookupAt = tmdbLookupAt
              }
        }

selectWithBangumiSql :: Query
selectWithBangumiSql =
  toQuery
    [text|
      SELECT
        s.id, s.created_at, s.updated_at, s.bangumi_id, s.current_episode,
        s.auto_complete, s.save_path, s.source_type, s.episode_offset, s.emby_id,
        b.id, b.created_at, b.updated_at, b.mikan_id, b.bgmtv_id, b.tmdb_id,
        b.title_chinese, b.title_japanese, b.season, b.platform, b.total_episodes,
        b.poster_url, b.air_date, b.air_week, b.tmdb_lookup_at
      FROM subscription s
      JOIN bangumi b ON s.bangumi_id = b.id
    |]

-- | Get all Subscriptions with Bangumi metadata
findAllWithBangumi :: (DB :> es) => Eff es [(Subscription, Bangumi)]
findAllWithBangumi = do
  rows :: [SubscriptionWithBangumi] <- query_ (selectWithBangumiSql <> " ORDER BY s.created_at DESC")
  pure $ map (\r -> (r.subscription, r.bangumi)) rows

-- | Find Subscription by ID with Bangumi metadata
findByIdWithBangumi :: (DB :> es) => Int -> Eff es (Maybe (Subscription, Bangumi))
findByIdWithBangumi subId = do
  rows :: [SubscriptionWithBangumi] <- query (selectWithBangumiSql <> " WHERE s.id = ?") (Only subId)
  pure $ fmap (\r -> (r.subscription, r.bangumi)) (listToMaybe rows)

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

toQuery :: Text -> Query
toQuery = fromString . T.unpack
