{-# LANGUAGE QuasiQuotes #-}

module Infra.Database.Repository.Calendar (
  -- * DTOs
  CreateCalendar (..),

  -- * Repository Operations
  create,
  upsert,
  findBySeason,
  findByBangumiId,
  deleteBySeason,
  delete,
) where

import Data.Text qualified as T
import Database.SQLite.Simple (NamedParam ((:=)))
import Effectful (Eff, (:>))
import Infra.Database.Effect (DB, Only (..), Query, execute, executeNamed, query, query_)
import Moe.Calendar (Calendar (..), Season)
import NeatInterpolation (text)

--------------------------------------------------------------------------------
-- DTOs
--------------------------------------------------------------------------------

-- | Data for creating a new calendar entry
data CreateCalendar = CreateCalendar
  { year :: Int
  , season :: Season
  , bangumiId :: Int
  }
  deriving stock (Show, Eq, Generic)

--------------------------------------------------------------------------------
-- SQL Queries
--------------------------------------------------------------------------------

selectCalendarSql :: Query
selectCalendarSql =
  toQuery
    [text|
      SELECT id, created_at, updated_at, year, season, bangumi_id
      FROM calendar
    |]

insertCalendarSql :: Query
insertCalendarSql =
  toQuery
    [text|
      INSERT INTO calendar (year, season, bangumi_id)
      VALUES (:year, :season, :bangumi_id)
    |]

upsertCalendarSql :: Query
upsertCalendarSql =
  toQuery
    [text|
      INSERT INTO calendar (year, season, bangumi_id)
      VALUES (:year, :season, :bangumi_id)
      ON CONFLICT (year, season, bangumi_id) DO UPDATE SET
        updated_at = CURRENT_TIMESTAMP
    |]

--------------------------------------------------------------------------------
-- Repository Operations
--------------------------------------------------------------------------------

-- | Create a new calendar entry
create :: (DB :> es) => CreateCalendar -> Eff es Calendar
create dto = do
  executeNamed
    insertCalendarSql
    [ ":year" := dto.year
    , ":season" := dto.season
    , ":bangumi_id" := dto.bangumiId
    ]
  rows <- query_ (selectCalendarSql <> " WHERE id = last_insert_rowid()")
  case rows of
    [cal] -> pure cal
    _ -> error "create: unexpected result after insert"

-- | Upsert a calendar entry (insert or update timestamp)
upsert :: (DB :> es) => CreateCalendar -> Eff es ()
upsert dto =
  executeNamed
    upsertCalendarSql
    [ ":year" := dto.year
    , ":season" := dto.season
    , ":bangumi_id" := dto.bangumiId
    ]

-- | Find all calendar entries for a specific year and season
findBySeason :: (DB :> es) => Int -> Season -> Eff es [Calendar]
findBySeason year season =
  query
    (selectCalendarSql <> " WHERE year = ? AND season = ? ORDER BY bangumi_id")
    (year, season)

-- | Find all calendar entries for a specific bangumi
findByBangumiId :: (DB :> es) => Int -> Eff es [Calendar]
findByBangumiId bangumiId =
  query
    (selectCalendarSql <> " WHERE bangumi_id = ? ORDER BY year DESC, season DESC")
    (Only bangumiId)

-- | Delete all calendar entries for a specific year and season
deleteBySeason :: (DB :> es) => Int -> Season -> Eff es ()
deleteBySeason year season =
  execute "DELETE FROM calendar WHERE year = ? AND season = ?" (year, season)

-- | Delete a calendar entry by ID
delete :: (DB :> es) => Int -> Eff es ()
delete calId =
  execute "DELETE FROM calendar WHERE id = ?" (Only calId)

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

toQuery :: Text -> Query
toQuery = fromString . T.unpack
