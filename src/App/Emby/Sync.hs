{- | Emby import service

This module provides the business logic for importing Bangumi
from Emby media library.

= Import Strategy

1. Get all TV show libraries from Emby
2. For each library, get all Series items
3. For each Series, check if it already exists (by embyId in subscription)
4. If not exists, create a new Bangumi and Subscription record
5. If exists, skip
-}
module App.Emby.Sync
  ( -- * Import Operations
    importFromEmby

    -- * Types
  , ImportResult (..)
  , ImportRecord (..)
  , ImportStats (..)
  )
where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Map.Strict qualified as Map
import Data.OpenApi (ToSchema)
import Data.Text qualified as T
import Effectful (Eff, (:>))
import Effectful.Log (Log)
import Effectful.Log qualified as Log

import Infra.Database.Effect (DB)
import Infra.Database.Repository.Bangumi qualified as BangumiRepo
import Infra.Database.Repository.Subscription qualified as SubRepo
import Infra.External.Emby.Effect
import Moe.Bangumi (Bangumi (..), Platform (..), SourceType (..))
import Moe.Setting (EmbySettings (..))

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Import statistics
data ImportStats = ImportStats
  { total :: Int
  -- ^ Total Emby items found
  , imported :: Int
  -- ^ Successfully imported
  , skipped :: Int
  -- ^ Skipped (already exists)
  , failed :: Int
  -- ^ Failed to import
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, ToSchema)

-- | Individual import record
data ImportRecord = ImportRecord
  { embyId :: Text
  , title :: Text
  , status :: ImportStatus
  , bangumiId :: Maybe Int
  -- ^ Bangumi ID if created
  , error_ :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToSchema)

data ImportStatus = Imported | Skipped | Failed
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, ToSchema)

instance ToJSON ImportRecord where
  toJSON r =
    object
      [ "embyId" .= r.embyId
      , "title" .= r.title
      , "status" .= r.status
      , "bangumiId" .= r.bangumiId
      , "error" .= r.error_
      ]

-- | Complete import result
data ImportResult = ImportResult
  { stats :: ImportStats
  , records :: [ImportRecord]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, ToSchema)

--------------------------------------------------------------------------------
-- Import Operations
--------------------------------------------------------------------------------

{- | Import Bangumi from Emby

Strategy:
1. Get all TV show libraries from Emby (or specified libraries)
2. Get all Series items from those libraries
3. For each item, check if already exists (by embyId in subscription)
4. If not exists, create a new Bangumi and Subscription record
5. If exists, skip
-}
importFromEmby ::
  ( Emby :> es
  , DB :> es
  , Log :> es
  ) =>
  EmbySettings ->
  Maybe [Text] ->
  -- ^ Optional list of library IDs to import from. If Nothing or empty, imports from all TV libraries.
  Eff es ImportResult
importFromEmby settings mLibraryIds = do
  Log.logInfo_ "Starting Emby import"

  -- Get all Emby items
  embyItemsResult <- getAllEmbySeriesItems mLibraryIds

  case embyItemsResult of
    Left err -> do
      Log.logAttention "Failed to get Emby items" $
        object ["error" .= show err]
      pure $
        ImportResult
          { stats = ImportStats 0 0 0 0
          , records = []
          }
    Right embyItems -> do
      Log.logInfo "Got Emby items" $
        object ["count" .= length embyItems]

      -- Import each item
      records <- forM embyItems $ \item ->
        importOneItem settings item

      let stats = calculateStats records

      Log.logInfo "Emby import completed" $
        object
          [ "total" .= stats.total
          , "imported" .= stats.imported
          , "skipped" .= stats.skipped
          , "failed" .= stats.failed
          ]

      pure $ ImportResult stats records

--------------------------------------------------------------------------------
-- Internal Functions
--------------------------------------------------------------------------------

-- | Import a single Emby item
importOneItem ::
  ( Emby :> es
  , DB :> es
  , Log :> es
  ) =>
  EmbySettings ->
  EmbyItem ->
  Eff es ImportRecord
importOneItem settings item = do
  -- Check if already exists (by embyId in subscription)
  existing <- SubRepo.findByEmbyId item.id
  case existing of
    Just _ -> do
      Log.logTrace "Skipping existing item" $
        object ["emby_id" .= item.id, "title" .= item.name]
      pure $
        ImportRecord
          { embyId = item.id
          , title = item.name
          , status = Skipped
          , bangumiId = Nothing
          , error_ = Nothing
          }
    Nothing -> do
      -- Create new Bangumi (metadata)
      let posterUrl = buildPosterUrl settings item
          tmdbId = extractTmdbId item
          airDate = item.premiereDate >>= parseAirDate

      -- Get episode count from Emby (actual number of episode files)
      episodeCount <- getEmbyEpisodeCount item.id >>= \case
        Right count -> pure count
        Left _ -> pure 0 -- Fall back to 0 if API call fails

      let createBangumiDto =
            BangumiRepo.CreateBangumi
              { titleChinese = item.name
              , titleJapanese = Nothing
              , mikanId = Nothing
              , bgmtvId = Nothing
              , tmdbId = tmdbId
              , season = 1
              , platform = TV
              , totalEpisodes = fromMaybe 0 item.childCount
              , posterUrl = posterUrl
              , airDate = airDate
              , airWeek = 0
              }

      bangumi <- BangumiRepo.create createBangumiDto

      -- Create Subscription (with embyId, currentEpisode = actual episode count)
      let createSubDto =
            SubRepo.CreateSubscription
              { bangumiId = bangumi.id
              , currentEpisode = episodeCount
              , autoComplete = False
              , savePath = Nothing
              , sourceType = Other
              , episodeOffset = 0
              , embyId = Just item.id
              }

      _ <- SubRepo.create createSubDto

      Log.logInfo "Imported Bangumi from Emby" $
        object
          [ "bangumi_id" .= bangumi.id
          , "title" .= item.name
          , "emby_id" .= item.id
          , "episode_count" .= episodeCount
          ]

      pure $
        ImportRecord
          { embyId = item.id
          , title = item.name
          , status = Imported
          , bangumiId = Just bangumi.id
          , error_ = Nothing
          }

-- | Get all Series items from TV show libraries
-- If libraryIds is provided and non-empty, only get items from those libraries
-- Otherwise, get items from all TV libraries
getAllEmbySeriesItems ::
  (Emby :> es) =>
  Maybe [Text] ->
  Eff es (Either EmbyError [EmbyItem])
getAllEmbySeriesItems mLibraryIds = do
  libsResult <- getEmbyLibraries
  case libsResult of
    Left err -> pure $ Left err
    Right libs -> do
      -- Filter libraries based on input
      let tvLibs = case mLibraryIds of
            Just ids | not (null ids) ->
              -- Use specified library IDs
              filter (\lib -> lib.itemId `elem` map Just ids) libs
            _ ->
              -- Use all TV show libraries (tvshows or mixed)
              filter isTvLibrary libs

      -- Get items from each library
      allItems <- forM tvLibs $ \lib ->
        case lib.itemId of
          Nothing -> pure $ Right []
          Just libId -> do
            result <- getEmbySeriesItems libId
            pure $ fmap (.items) result

      -- Combine results
      case partitionEithers allItems of
        (err : _, _) -> pure $ Left err
        ([], itemLists) -> pure $ Right $ concat itemLists

-- | Check if a library is a TV show library
isTvLibrary :: EmbyLibrary -> Bool
isTvLibrary lib =
  case lib.collectionType of
    Just "tvshows" -> True
    Just "mixed" -> True
    _ -> False

-- | Extract TMDB ID from Emby item
extractTmdbId :: EmbyItem -> Maybe Int
extractTmdbId item = do
  providerIds <- item.providerIds
  tmdbText <- providerIds.tmdb
  readMaybe @Int (T.unpack tmdbText)

-- | Parse air date from Emby format to YYYY-MM-DD
parseAirDate :: Text -> Maybe Text
parseAirDate dateStr =
  -- Emby returns dates like "2024-01-15T00:00:00.0000000Z"
  -- We just need the YYYY-MM-DD part
  let datePart = T.take 10 dateStr
   in if T.length datePart == 10 && T.index datePart 4 == '-' && T.index datePart 7 == '-'
        then Just datePart
        else Nothing

-- | Build poster URL for an Emby item
buildPosterUrl :: EmbySettings -> EmbyItem -> Maybe Text
buildPosterUrl settings item =
  case item.imageTags of
    Just tags | Map.member "Primary" tags ->
      Just $ T.dropWhileEnd (== '/') settings.url <> "/Items/" <> item.id <> "/Images/Primary"
    _ -> Nothing

-- | Calculate import statistics from records
calculateStats :: [ImportRecord] -> ImportStats
calculateStats records =
  ImportStats
    { total = length records
    , imported = length $ filter ((== Imported) . (.status)) records
    , skipped = length $ filter ((== Skipped) . (.status)) records
    , failed = length $ filter ((== Failed) . (.status)) records
    }
