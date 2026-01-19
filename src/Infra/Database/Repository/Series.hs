module Infra.Database.Repository.Series (
  -- * DTOs
  CreateSeries (..),
  UpdateSeries (..),

  -- * Repository Operations
  create,
  findAll,
  getOne,
  update,
)
where

import Data.Text qualified as T
import Effectful (Eff, (:>))
import Infra.Database.Effect (DB, Only (..), Query, execute, query, query_)
import Moe.Series (Series (..))

--------------------------------------------------------------------------------
-- DTOs
--------------------------------------------------------------------------------

-- | Data for creating a new Series
data CreateSeries = CreateSeries
  { bangumiIds :: [Int]
  -- ^ Bangumi IDs to associate with this series (required, at least one)
  , titleChinese :: Text
  -- ^ Chinese title (required)
  , titleJapanese :: Maybe Text
  -- ^ Japanese original name (optional)
  , posterUrl :: Maybe Text
  -- ^ Series poster image URL (optional)
  }
  deriving stock (Show, Eq, Generic)

{- | Data for updating a Series

All fields are optional. Nothing means "don't update this field".
-}
data UpdateSeries = UpdateSeries
  { titleChinese :: Maybe Text
  -- ^ New Chinese title
  , titleJapanese :: Maybe Text
  -- ^ New Japanese title
  , posterUrl :: Maybe Text
  -- ^ New poster URL
  }
  deriving stock (Show, Eq, Generic)

--------------------------------------------------------------------------------
-- Repository Operations
--------------------------------------------------------------------------------

{- | Create a new Series

Creates a new Series and associates the given Bangumi IDs with it.
Returns the newly created Series with generated id and timestamps.
-}
create :: (DB :> es) => CreateSeries -> Eff es Series
create CreateSeries{bangumiIds, titleChinese, titleJapanese, posterUrl} = do
  -- Insert the series
  execute
    "INSERT INTO series (title_chinese, title_japanese, poster_url) VALUES (?, ?, ?)"
    (titleChinese, titleJapanese, posterUrl)
  rows <-
    query_
      "SELECT id, created_at, updated_at, title_chinese, title_japanese, poster_url \
      \FROM series WHERE id = last_insert_rowid()"
  case rows of
    [series@Series{id = seriesId'}] -> do
      -- Associate bangumi with this series
      mapM_ (linkBangumi seriesId') bangumiIds
      pure series
    _ -> error "create: unexpected result after insert"

-- | Link a Bangumi to a Series by updating its series_id
linkBangumi :: (DB :> es) => Int -> Int -> Eff es ()
linkBangumi seriesId bangumiId =
  execute
    "UPDATE bangumi SET series_id = ?, updated_at = CURRENT_TIMESTAMP WHERE id = ?"
    (seriesId, bangumiId)

-- | Get all Series ordered by creation time (newest first)
findAll :: (DB :> es) => Eff es [Series]
findAll =
  query_
    "SELECT id, created_at, updated_at, title_chinese, title_japanese, poster_url \
    \FROM series ORDER BY created_at DESC"

{- | Get a single Series by ID

Returns Nothing if the Series doesn't exist.
-}
getOne :: (DB :> es) => Int -> Eff es (Maybe Series)
getOne seriesId = do
  rows <-
    query
      "SELECT id, created_at, updated_at, title_chinese, title_japanese, poster_url \
      \FROM series WHERE id = ?"
      (Only seriesId)
  pure $ listToMaybe rows

{- | Update a Series by ID

Only updates fields that are Just. Returns Nothing if the Series doesn't exist.
Returns the updated Series on success.
-}
update :: (DB :> es) => Int -> UpdateSeries -> Eff es (Maybe Series)
update seriesId UpdateSeries{titleChinese, titleJapanese, posterUrl} = do
  -- Build dynamic SET clause
  let updates =
        catMaybes
          [ ("title_chinese = ?",) <$> titleChinese
          , ("title_japanese = ?",) <$> titleJapanese
          , ("poster_url = ?",) <$> posterUrl
          ]
  case updates of
    [] -> getOne seriesId -- No updates, just return current
    _ -> do
      -- Execute update with dynamic fields
      executeUpdate seriesId updates
      getOne seriesId

-- | Execute a dynamic UPDATE query
executeUpdate :: (DB :> es) => Int -> [(Text, Text)] -> Eff es ()
executeUpdate seriesId updates = do
  let setClauses = map fst updates
      values = map snd updates
      setClause = T.intercalate ", " setClauses
      sql = toQuery $ "UPDATE series SET " <> setClause <> ", updated_at = CURRENT_TIMESTAMP WHERE id = ?"
  case values of
    [v1] -> execute sql (v1, seriesId)
    [v1, v2] -> execute sql (v1, v2, seriesId)
    [v1, v2, v3] -> execute sql (v1, v2, v3, seriesId)
    _ -> pure () -- No updates

-- | Convert Text to Query (Query has IsString instance)
toQuery :: Text -> Query
toQuery = fromString . T.unpack
