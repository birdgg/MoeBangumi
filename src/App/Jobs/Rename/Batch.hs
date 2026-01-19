{- | Batch rename operations for collections

This module handles batch renaming for torrents with multiple video files,
including TMDB metadata lookup and Bangumi creation.
-}
module App.Jobs.Rename.Batch
  ( -- * Batch Rename
    performBatchRename
  )
where

import Control.Monad (foldM)
import Data.Aeson (object, (.=))
import Data.Text qualified as T
import Data.Text.Display (display)
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Log (Log)
import Effectful.Log qualified as Log
import Effectful.Reader.Static qualified as Reader
import Servant.Client (mkClientEnv, runClientM)

import App.Jobs.Rename.Utils
  ( buildEpisodePath
  , buildNewPath
  , extractExtension
  , getDirectory
  , isSubtitleFile
  )
import Moe.Subtitle (extractSubtitleSuffix)
import Infra.Database.Effect (DB)
import Infra.Database.Repository.Bangumi qualified as BangumiRepo
import Infra.Downloader.Effect
import Infra.Environment.Env (MoeEnv (..))
import Infra.External.Tmdb.API (tmdbBaseUrl)
import Infra.External.Tmdb.Client qualified as TmdbClient
import Infra.External.Tmdb.Types qualified as Tmdb
import Moe.Bangumi (Bangumi (..), Platform (..))
import Moe.Metadata (TmdbMediaType (..), TmdbSearchItem (..))
import Moe.Parsing.Anime qualified as Anime
import Moe.Parsing.Collection qualified as Collection
import Moe.Setting (Settings (..), TmdbSettings (..))

--------------------------------------------------------------------------------
-- Batch Rename
--------------------------------------------------------------------------------

{- | Perform batch rename for torrents with multiple video files

Flow:
1. Parse collection info from torrent name
2. Search TMDB for metadata
3. Create Bangumi in database (if metadata found)
4. Rename each video file to standard format
5. Rename associated subtitle files
6. Remove "rename" tag
-}
performBatchRename
  :: ( Downloader :> es
     , Log :> es
     , IOE :> es
     , Reader.Reader MoeEnv :> es
     , DB :> es
     , Concurrent :> es
     )
  => SimpleTorrentInfo
  -> [SimpleTorrentFile]
  -- ^ Video files
  -> [SimpleTorrentFile]
  -- ^ All files (for subtitle renaming)
  -> Eff es ()
performBatchRename torrent videoFiles allFiles = do
  -- Parse collection info from torrent name
  let filePaths = map (.name) allFiles
      collectionInfo = Collection.parseCollectionInfo torrent.name filePaths

  Log.logTrace "Parsed collection info" $
    object
      [ "hash" .= torrent.hash
      , "content_type" .= (show collectionInfo.contentType :: Text)
      , "title" .= collectionInfo.title
      , "season" .= collectionInfo.season
      ]

  case collectionInfo.title of
    Nothing -> do
      Log.logAttention "Failed to parse title from torrent name, skipping" $
        object
          [ "hash" .= torrent.hash
          , "name" .= torrent.name
          ]
    Just title -> do
      -- Search TMDB for metadata
      tmdbResult <- searchTmdbForTitle title

      case tmdbResult of
        Nothing -> do
          Log.logAttention "No TMDB match found, skipping" $
            object
              [ "hash" .= torrent.hash
              , "title" .= title
              ]
        Just tmdbItem -> do
          let seasonNum = fromMaybe 1 collectionInfo.season

          -- Create Bangumi in database
          bangumiResult <- createBangumiFromTmdb title tmdbItem seasonNum torrent.savePath

          case bangumiResult of
            Nothing -> do
              Log.logAttention "Failed to create Bangumi" $
                object
                  [ "hash" .= torrent.hash
                  , "title" .= title
                  ]
            Just bangumi -> do
              Log.logInfo "Created Bangumi from TMDB" $
                object
                  [ "hash" .= torrent.hash
                  , "bangumi_id" .= bangumi.id
                  , "title" .= bangumi.titleChinese
                  , "season" .= bangumi.season
                  ]

              -- Rename each video file
              renameCollectionFiles torrent bangumi videoFiles allFiles

--------------------------------------------------------------------------------
-- TMDB Integration
--------------------------------------------------------------------------------

{- | Search TMDB for a title

Returns the best matching TV show result.
-}
searchTmdbForTitle
  :: ( Reader.Reader MoeEnv :> es
     , IOE :> es
     )
  => Text
  -> Eff es (Maybe TmdbSearchItem)
searchTmdbForTitle title = do
  env <- Reader.ask @MoeEnv
  settings :: Settings <- readTVarIO env.settingsVar
  let apiKey = (settings.tmdb :: TmdbSettings).apiKey
      clientEnv = mkClientEnv env.httpManager tmdbBaseUrl

  result <- liftIO $ runClientM (TmdbClient.searchTv apiKey title) clientEnv

  case result of
    Left _err -> pure Nothing
    Right response -> do
      -- Select best match (first result for now, could add similarity scoring later)
      pure $ listToMaybe $ map tmdbShowToSearchItem response.results
 where
  tmdbShowToSearchItem :: Tmdb.TvShow -> TmdbSearchItem
  tmdbShowToSearchItem s =
    TmdbSearchItem
      { id = s.id
      , name = s.name
      , mediaType = TmdbTv
      , airDate = s.firstAirDate
      , posterPath = s.posterPath
      }

--------------------------------------------------------------------------------
-- Bangumi Creation
--------------------------------------------------------------------------------

{- | Create a Bangumi entry from TMDB metadata

Note: This only creates the Bangumi metadata. No subscription is created
for batch imports - users can manually subscribe later if needed.
-}
createBangumiFromTmdb
  :: ( DB :> es
     , IOE :> es
     )
  => Text
  -- ^ Title (Chinese)
  -> TmdbSearchItem
  -> Int
  -- ^ Season number
  -> Text
  -- ^ Save path (stored for reference, not used without subscription)
  -> Eff es (Maybe Bangumi)
createBangumiFromTmdb title tmdbItem seasonNum _savePath = do
  let createDto =
        BangumiRepo.CreateBangumi
          { titleChinese = title
          , titleJapanese = Nothing
          , mikanId = Nothing
          , bgmtvId = Nothing
          , tmdbId = Just (fromIntegral tmdbItem.id)
          , season = seasonNum
          , platform = TV
          , totalEpisodes = 0
          , posterUrl = fmap (\p -> "https://image.tmdb.org/t/p/w500" <> p) tmdbItem.posterPath
          , airDate = tmdbItem.airDate
          , airWeek = 0
          }

  bangumi <- BangumiRepo.create createDto
  pure $ Just bangumi

--------------------------------------------------------------------------------
-- Collection File Rename
--------------------------------------------------------------------------------

{- | Rename all files in a collection to standard format -}
renameCollectionFiles
  :: forall es
   . ( Downloader :> es
     , Log :> es
     , IOE :> es
     )
  => SimpleTorrentInfo
  -> Bangumi
  -> [SimpleTorrentFile]
  -- ^ Video files
  -> [SimpleTorrentFile]
  -- ^ All files
  -> Eff es ()
renameCollectionFiles torrent bangumi videoFiles allFiles = do
  -- Process each video file
  successCount <- foldM processVideoFile 0 videoFiles

  Log.logInfo "Collection rename completed" $
    object
      [ "hash" .= torrent.hash
      , "total_files" .= length videoFiles
      , "success_count" .= successCount
      ]

  -- Remove "rename" tag if at least one file was renamed
  when (successCount > 0) $ do
    Log.logTrace "Removing rename tag" $
      object ["hash" .= torrent.hash]

    removeResult <- removeTags [torrent.hash] [display RenameTag]

    case removeResult of
      Left err ->
        Log.logAttention "Failed to remove tag" $
          object
            [ "hash" .= torrent.hash
            , "error" .= display err
            ]
      Right () ->
        Log.logTrace "Tag removed successfully" $
          object ["hash" .= torrent.hash]
 where
  processVideoFile :: Int -> SimpleTorrentFile -> Eff es Int
  processVideoFile count videoFile = do
    -- Detect if this is a special (SP/OVA) file
    let isSpecial = Collection.isSpecialDirectory `any` T.splitOn "/" videoFile.name
        seasonNum = if isSpecial then 0 else bangumi.season

    -- Extract episode number from filename
    case Anime.extractEpisode (getFileName' videoFile.name) of
      Nothing -> do
        Log.logTrace "Could not extract episode number, skipping" $
          object ["filename" .= videoFile.name]
        pure count
      Just episodeNum -> do
        case extractExtension videoFile.name of
          Nothing -> pure count
          Just ext -> do
            let newBaseName = buildEpisodePath bangumi.titleChinese seasonNum episodeNum
                newPath = buildNewPath videoFile.name newBaseName ext

            -- Skip if already renamed
            if videoFile.name == newPath
              then pure count
              else do
                let renameOp =
                      RenameOperation
                        { torrentHash = torrent.hash
                        , oldPath = videoFile.name
                        , newPath = newPath
                        }

                Log.logInfo "Renaming collection file" $
                  object
                    [ "hash" .= torrent.hash
                    , "old_path" .= videoFile.name
                    , "new_path" .= newPath
                    ]

                renameResult <- batchRenameFiles [renameOp]

                case renameResult of
                  Left err -> do
                    Log.logAttention "Collection file rename failed" $
                      object
                        [ "hash" .= torrent.hash
                        , "filename" .= videoFile.name
                        , "error" .= display err
                        ]
                    pure count
                  Right () -> do
                    -- Rename associated subtitle files
                    renameCollectionSubtitles torrent videoFile newBaseName allFiles
                    pure (count + 1)

  -- Extract just the filename from a path (local helper to avoid name conflict)
  getFileName' :: Text -> Text
  getFileName' path =
    case T.breakOnEnd "/" path of
      ("", name) -> name
      (_, name) -> name

--------------------------------------------------------------------------------
-- Collection Subtitle Rename
--------------------------------------------------------------------------------

{- | Rename subtitle files for a collection video file -}
renameCollectionSubtitles
  :: ( Downloader :> es
     , Log :> es
     , IOE :> es
     )
  => SimpleTorrentInfo
  -> SimpleTorrentFile
  -- ^ Original video file
  -> Text
  -- ^ New base name (without extension)
  -> [SimpleTorrentFile]
  -- ^ All files
  -> Eff es ()
renameCollectionSubtitles torrent videoFile newBaseName allFiles = do
  -- Find subtitle files in the same directory as the video
  let videoDir = getDirectory videoFile.name
      subtitleFiles = filter (isSubtitleInSameDir videoDir) allFiles

  forM_ subtitleFiles $ \subFile -> do
    case extractSubtitleSuffix subFile.name of
      Nothing -> pure ()
      Just suffix -> do
        let newPath = buildNewPath subFile.name newBaseName suffix

        when (subFile.name /= newPath) $ do
          let renameOp =
                RenameOperation
                  { torrentHash = torrent.hash
                  , oldPath = subFile.name
                  , newPath = newPath
                  }

          Log.logTrace "Renaming collection subtitle" $
            object
              [ "old_path" .= subFile.name
              , "new_path" .= newPath
              ]

          void $ batchRenameFiles [renameOp]
 where
  isSubtitleInSameDir :: Text -> SimpleTorrentFile -> Bool
  isSubtitleInSameDir dir file =
    isSubtitleFile file && getDirectory file.name == dir
