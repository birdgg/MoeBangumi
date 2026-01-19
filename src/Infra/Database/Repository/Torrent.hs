{-# LANGUAGE QuasiQuotes #-}

-- | Torrent repository for database operations
--
-- This module provides CRUD operations for Torrent using the DB effect.
module Infra.Database.Repository.Torrent
  ( -- * DTOs
    CreateTorrent (..)
  , UpdateTorrent (..)

    -- * Repository Operations
  , create
  , createMany
  , findAll
  , getOne
  , findBySubscriptionId
  , findBySubscriptionAndEpisode
  , findByInfoHash
  , update
  , delete
  )
where

import Data.Text qualified as T
import Database.SQLite.Simple (NamedParam ((:=)))
import Database.SQLite.Simple.ToField (ToField (..))
import Effectful (Eff, (:>))
import Infra.Database.Effect (Only (..), Query, DB, execute, executeNamed, query, query_)
import Moe.Torrent (Torrent (..))
import NeatInterpolation (text)

--------------------------------------------------------------------------------
-- DTOs
--------------------------------------------------------------------------------

-- | Data for creating a new Torrent
data CreateTorrent = CreateTorrent
  { subscriptionId :: Int
  -- ^ Associated subscription ID
  , rssId :: Maybe Int
  -- ^ Optional reference to source RSS
  , infoHash :: Text
  -- ^ BitTorrent info hash (required)
  , torrentUrl :: Text
  -- ^ Torrent URL or magnet link (required)
  , episodeNumber :: Maybe Int
  -- ^ Episode number
  , group :: Maybe Text
  -- ^ Subtitle group name
  , languages :: Maybe Text
  -- ^ Subtitle languages (JSON array string)
  }
  deriving stock (Show, Eq, Generic)

-- | Data for updating a Torrent
--
-- All fields are optional. Nothing means "don't update this field".
data UpdateTorrent = UpdateTorrent
  { subscriptionId :: Maybe Int
  , rssId :: Maybe Int
  , infoHash :: Maybe Text
  , torrentUrl :: Maybe Text
  , episodeNumber :: Maybe Int
  , group :: Maybe Text
  , languages :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

--------------------------------------------------------------------------------
-- SQL Queries
--------------------------------------------------------------------------------

selectTorrentSql :: Query
selectTorrentSql =
  toQuery
    [text|
      SELECT
        id, created_at, updated_at, subscription_id, rss_id,
        info_hash, torrent_url, episode_number, "group", languages
      FROM torrent
    |]

insertTorrentSql :: Query
insertTorrentSql =
  toQuery
    [text|
      INSERT INTO torrent (
        subscription_id, rss_id, info_hash, torrent_url,
        episode_number, "group", languages
      ) VALUES (
        :subscription_id, :rss_id, :info_hash, :torrent_url,
        :episode_number, :group, :languages
      )
    |]

--------------------------------------------------------------------------------
-- Repository Operations
--------------------------------------------------------------------------------

-- | Create a new Torrent
create :: (DB :> es) => CreateTorrent -> Eff es Torrent
create dto = do
  executeNamed
    insertTorrentSql
    [ ":subscription_id" := dto.subscriptionId
    , ":rss_id" := dto.rssId
    , ":info_hash" := dto.infoHash
    , ":torrent_url" := dto.torrentUrl
    , ":episode_number" := dto.episodeNumber
    , ":group" := dto.group
    , ":languages" := dto.languages
    ]
  rows <- query_ (selectTorrentSql <> " WHERE id = last_insert_rowid()")
  case rows of
    [torrent] -> pure torrent
    _ -> error "create: unexpected result after insert"

-- | Create multiple Torrents
createMany :: (DB :> es) => [CreateTorrent] -> Eff es [Torrent]
createMany = mapM create

-- | Get all Torrents ordered by creation time (newest first)
findAll :: (DB :> es) => Eff es [Torrent]
findAll = query_ (selectTorrentSql <> " ORDER BY created_at DESC")

-- | Get a single Torrent by ID
getOne :: (DB :> es) => Int -> Eff es (Maybe Torrent)
getOne torrentId = do
  rows <- query (selectTorrentSql <> " WHERE id = ?") (Only torrentId)
  pure $ listToMaybe rows

-- | Find all torrents by subscription ID
--
-- Returns all torrents for a subscription, ordered by creation time (newest first).
findBySubscriptionId :: (DB :> es) => Int -> Eff es [Torrent]
findBySubscriptionId subId =
  query
    (selectTorrentSql <> " WHERE subscription_id = ? ORDER BY created_at DESC")
    (Only subId)

-- | Find torrents by subscription ID and episode number
--
-- Returns all matching torrents ordered by creation time (newest first).
findBySubscriptionAndEpisode :: (DB :> es) => Int -> Int -> Eff es [Torrent]
findBySubscriptionAndEpisode subId episodeNum =
  query
    ( selectTorrentSql
        <> " WHERE subscription_id = ? AND episode_number = ? ORDER BY created_at DESC"
    )
    (subId, episodeNum)

-- | Find a torrent by its info hash
--
-- Returns the first matching torrent (should be unique due to index).
findByInfoHash :: (DB :> es) => Text -> Eff es (Maybe Torrent)
findByInfoHash hash = do
  rows <- query (selectTorrentSql <> " WHERE info_hash = ?") (Only hash)
  pure $ listToMaybe rows

-- | Update a Torrent by ID
update :: (DB :> es) => Int -> UpdateTorrent -> Eff es (Maybe Torrent)
update torrentId dto = do
  let namedParams =
        catMaybes
          [ mkParam ":subscription_id" dto.subscriptionId
          , mkParam ":rss_id" dto.rssId
          , mkParam ":info_hash" dto.infoHash
          , mkParam ":torrent_url" dto.torrentUrl
          , mkParam ":episode_number" dto.episodeNumber
          , mkParam ":group" dto.group
          , mkParam ":languages" dto.languages
          ]
  case namedParams of
    [] -> getOne torrentId
    _ -> do
      let setClauses = map paramToSetClause namedParams
          setClause = T.intercalate ", " setClauses
          sql = toQuery $ "UPDATE torrent SET " <> setClause <> ", updated_at = CURRENT_TIMESTAMP WHERE id = :id"
          allParams = (":id" := torrentId) : namedParams
      executeNamed sql allParams
      getOne torrentId
 where
  mkParam :: (ToField a) => Text -> Maybe a -> Maybe NamedParam
  mkParam name = fmap (name :=)

  paramToSetClause :: NamedParam -> Text
  paramToSetClause (name := _) =
    let col = T.replace ":" "" name
        -- Handle reserved word "group"
        quotedCol = if col == "group" then "\"group\"" else col
     in quotedCol <> " = " <> name

-- | Delete a Torrent by ID
delete :: (DB :> es) => Int -> Eff es ()
delete torrentId =
  execute "DELETE FROM torrent WHERE id = ?" (Only torrentId)

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

toQuery :: Text -> Query
toQuery = fromString . T.unpack
