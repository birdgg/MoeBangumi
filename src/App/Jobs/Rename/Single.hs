{- | Single file rename operations

This module handles renaming for torrents with a single video file,
including associated subtitle files.
-}
module App.Jobs.Rename.Single
  ( -- * Rename Operations
    performRename
  , renameSubtitleFiles
  )
where

import Data.Aeson (object, (.=))
import Data.Text qualified as T
import Data.Text.Display (display)
import Effectful
import Effectful.Log (Log)
import Effectful.Log qualified as Log

import App.Jobs.Rename.Utils
  ( buildNewPath
  , extractExtension
  , isSubtitleFile
  )
import Infra.Downloader.Effect
import Infra.Notification.Effect (Notification, sendNotification)
import Moe.Subtitle (extractSubtitleSuffix)

--------------------------------------------------------------------------------
-- Single File Rename
--------------------------------------------------------------------------------

{- | Perform the actual rename operation for video and subtitle files -}
performRename
  :: ( Downloader :> es
     , Notification :> es
     , Log :> es
     , IOE :> es
     )
  => SimpleTorrentInfo
  -> SimpleTorrentFile
  -> [SimpleTorrentFile]
  -- ^ All files in torrent (for subtitle renaming)
  -> Eff es ()
performRename torrent videoFile allFiles = do
  case extractExtension videoFile.name of
    Nothing ->
      Log.logAttention "Could not extract extension" $
        object
          [ "hash" .= torrent.hash
          , "filename" .= videoFile.name
          ]
    Just ext -> do
      -- Build new path preserving directory structure
      let newPath = buildNewPath videoFile.name torrent.name ext
          newVideoBaseName = torrent.name  -- Base name for subtitle renaming
          renameOp =
            RenameOperation
              { torrentHash = torrent.hash
              , oldPath = videoFile.name
              , newPath = newPath
              }

      -- Skip if already renamed
      when (videoFile.name /= newPath) $ do
        Log.logInfo "Renaming file" $
          object
            [ "hash" .= torrent.hash
            , "old_path" .= videoFile.name
            , "new_path" .= newPath
            ]

        -- Execute rename
        renameResult <- batchRenameFiles [renameOp]

        case renameResult of
          Left err ->
            Log.logAttention "Rename failed" $
              object
                [ "hash" .= torrent.hash
                , "error" .= display err
                ]
          Right () -> do
            Log.logInfo "Video rename succeeded" $
              object ["hash" .= torrent.hash]

            -- Rename subtitle files to match
            renameSubtitleFiles torrent newVideoBaseName allFiles

            -- Send notification
            let notificationMsg =
                  "✅ <b>文件重命名完成</b>\n\n"
                    <> "📁 " <> escapeHtml torrent.name
            notifyResult <- sendNotification notificationMsg Nothing
            case notifyResult of
              Left err ->
                Log.logTrace "Notification failed (non-critical)" $
                  object ["error" .= display err]
              Right () ->
                Log.logTrace_ "Notification sent"

            -- Remove "rename" tag
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

--------------------------------------------------------------------------------
-- HTML Escaping
--------------------------------------------------------------------------------

-- | Escape HTML special characters for Telegram messages
escapeHtml :: Text -> Text
escapeHtml =
  T.replace "&" "&amp;"
    . T.replace "<" "&lt;"
    . T.replace ">" "&gt;"
    . T.replace "\"" "&quot;"

--------------------------------------------------------------------------------
-- Subtitle Rename
--------------------------------------------------------------------------------

{- | Rename subtitle files to match the new video name

After a video is renamed, this function renames all subtitle files
in the same torrent to match, preserving language tags.

Example:
- Video renamed to: "葬送的芙莉莲 s01e05.mkv"
- Subtitle "[SubGroup] EP05.chs.ass" -> "葬送的芙莉莲 s01e05.chs.ass"
-}
renameSubtitleFiles
  :: ( Downloader :> es
     , Log :> es
     , IOE :> es
     )
  => SimpleTorrentInfo
  -> Text
  -- ^ New video base name (without extension), e.g. "葬送的芙莉莲 s01e05"
  -> [SimpleTorrentFile]
  -- ^ All files in the torrent
  -> Eff es ()
renameSubtitleFiles torrent newVideoBaseName allFiles = do
  let subtitleFiles = filter isSubtitleFile allFiles

  forM_ subtitleFiles $ \subFile -> do
    case extractSubtitleSuffix subFile.name of
      Nothing ->
        Log.logTrace "Could not extract subtitle suffix" $
          object ["filename" .= subFile.name]
      Just suffix -> do
        let newPath = buildNewPath subFile.name newVideoBaseName suffix
            renameOp =
              RenameOperation
                { torrentHash = torrent.hash
                , oldPath = subFile.name
                , newPath = newPath
                }

        -- Skip if already renamed
        when (subFile.name /= newPath) $ do
          Log.logInfo "Renaming subtitle file" $
            object
              [ "hash" .= torrent.hash
              , "old_path" .= subFile.name
              , "new_path" .= newPath
              ]

          renameResult <- batchRenameFiles [renameOp]

          case renameResult of
            Left err ->
              Log.logAttention "Subtitle rename failed" $
                object
                  [ "hash" .= torrent.hash
                  , "filename" .= subFile.name
                  , "error" .= display err
                  ]
            Right () ->
              Log.logTrace "Subtitle renamed successfully" $
                object ["new_path" .= newPath]
