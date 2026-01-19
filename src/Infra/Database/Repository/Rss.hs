{-# LANGUAGE QuasiQuotes #-}

module Infra.Database.Repository.Rss (
  -- * DTOs
  CreateRss (..),
  UpdateRss (..),

  -- * Repository Operations
  create,
  createMany,
  findAll,
  findAllEnabled,
  getOne,
  getBySubscriptionId,
  update,
  updateLastProcessedPubDate,
  delete,
)
where

import Data.Text qualified as T
import Database.SQLite.Simple (NamedParam ((:=)))
import Database.SQLite.Simple.ToField (ToField (..))
import Effectful (Eff, (:>))
import Infra.Database.Effect (DB, Only (..), Query, execute, executeNamed, query, query_)
import Moe.Rss (Rss (..))
import NeatInterpolation (text)

--------------------------------------------------------------------------------
-- DTOs
--------------------------------------------------------------------------------

-- | Data for creating a new RSS subscription
data CreateRss = CreateRss
  { subscriptionId :: Int
  -- ^ Associated subscription ID (required)
  , url :: Text
  -- ^ RSS feed URL (required)
  , enabled :: Bool
  -- ^ Whether the subscription is enabled
  }
  deriving stock (Show, Eq, Generic)

{- | Data for updating an RSS subscription

All fields are optional. Nothing means "don't update this field".
-}
data UpdateRss = UpdateRss
  { subscriptionId :: Maybe Int
  , url :: Maybe Text
  , enabled :: Maybe Bool
  }
  deriving stock (Show, Eq, Generic)

--------------------------------------------------------------------------------
-- SQL Queries
--------------------------------------------------------------------------------

selectRssSql :: Query
selectRssSql =
  toQuery
    [text|
      SELECT id, created_at, updated_at, subscription_id, url, enabled, last_processed_pub_date
      FROM rss
    |]

insertRssSql :: Query
insertRssSql =
  toQuery
    [text|
      INSERT INTO rss (subscription_id, url, enabled)
      VALUES (:subscription_id, :url, :enabled)
    |]

--------------------------------------------------------------------------------
-- Repository Operations
--------------------------------------------------------------------------------

-- | Create a new RSS subscription
create :: (DB :> es) => CreateRss -> Eff es Rss
create dto = do
  executeNamed
    insertRssSql
    [ ":subscription_id" := dto.subscriptionId
    , ":url" := dto.url
    , ":enabled" := boolToInt dto.enabled
    ]
  rows <- query_ (selectRssSql <> " WHERE id = last_insert_rowid()")
  case rows of
    [rss] -> pure rss
    _ -> error "create: unexpected result after insert"

-- | Create multiple RSS subscriptions
createMany :: (DB :> es) => [CreateRss] -> Eff es [Rss]
createMany = mapM create

-- | Get all RSS subscriptions ordered by creation time (newest first)
findAll :: (DB :> es) => Eff es [Rss]
findAll = query_ (selectRssSql <> " ORDER BY created_at DESC")

-- | Get all enabled RSS subscriptions ordered by creation time (newest first)
findAllEnabled :: (DB :> es) => Eff es [Rss]
findAllEnabled = query_ (selectRssSql <> " WHERE enabled = 1 ORDER BY created_at DESC")

-- | Get a single RSS subscription by ID
getOne :: (DB :> es) => Int -> Eff es (Maybe Rss)
getOne rssId = do
  rows <- query (selectRssSql <> " WHERE id = ?") (Only rssId)
  pure $ listToMaybe rows

-- | Get all RSS subscriptions for a specific subscription
getBySubscriptionId :: (DB :> es) => Int -> Eff es [Rss]
getBySubscriptionId subscriptionId =
  query
    (selectRssSql <> " WHERE subscription_id = ? ORDER BY created_at DESC")
    (Only subscriptionId)

-- | Update an RSS subscription by ID
update :: (DB :> es) => Int -> UpdateRss -> Eff es (Maybe Rss)
update rssId dto = do
  let namedParams =
        catMaybes
          [ mkParam ":subscription_id" dto.subscriptionId
          , mkParam ":url" dto.url
          , mkParamBool ":enabled" dto.enabled
          ]
  case namedParams of
    [] -> getOne rssId
    _ -> do
      let setClauses = map paramToSetClause namedParams
          setClause = T.intercalate ", " setClauses
          sql = toQuery $ "UPDATE rss SET " <> setClause <> ", updated_at = CURRENT_TIMESTAMP WHERE id = :id"
          allParams = (":id" := rssId) : namedParams
      executeNamed sql allParams
      getOne rssId
 where
  mkParam :: (ToField a) => Text -> Maybe a -> Maybe NamedParam
  mkParam name = fmap (name :=)

  mkParamBool :: Text -> Maybe Bool -> Maybe NamedParam
  mkParamBool name = fmap (\v -> name := boolToInt v)

  paramToSetClause :: NamedParam -> Text
  paramToSetClause (name := _) = T.replace ":" "" name <> " = " <> name

{- | Update only the last_processed_pub_date field for an RSS subscription

This is a convenience function for the common case of updating
just the last processed pubDate after fetching RSS items.
-}
updateLastProcessedPubDate :: (DB :> es) => Int -> Maybe Text -> Eff es ()
updateLastProcessedPubDate rssId pubDate =
  executeNamed
    (toQuery "UPDATE rss SET last_processed_pub_date = :pub_date, updated_at = CURRENT_TIMESTAMP WHERE id = :id")
    [ ":id" := rssId
    , ":pub_date" := pubDate
    ]

-- | Delete an RSS subscription by ID
delete :: (DB :> es) => Int -> Eff es ()
delete rssId =
  execute "DELETE FROM rss WHERE id = ?" (Only rssId)

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

toQuery :: Text -> Query
toQuery = fromString . T.unpack
