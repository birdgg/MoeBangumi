module Moe.Subtitle
  ( Subtitle (..)
  , SubtitlePattern (..)
  , matchesPattern
  , subtitleExtensions
  , isSubtitleExtension
  , extractSubtitleSuffix
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.List qualified as L
import Data.OpenApi (ToSchema (..))
import Data.OpenApi qualified as OpenApi
import Data.OpenApi.Optics ()
import Data.Text qualified as T
import Optics.Core ((?~))

data Subtitle = CHT | CHS | JAP | ENG
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

--------------------------------------------------------------------------------
-- Subtitle Pattern (for washing priority)
--------------------------------------------------------------------------------

-- | Subtitle pattern for washing priority comparison
-- Represents a combination of subtitle languages that should be matched exactly
-- Serialized as array: ["CHS", "JAP"] for 简日
newtype SubtitlePattern = SubtitlePattern {unSubtitlePattern :: [Subtitle]}
  deriving stock (Show, Eq, Generic)
  deriving newtype (FromJSON, ToJSON)

instance ToSchema SubtitlePattern where
  declareNamedSchema _ = do
    -- SubtitlePattern is serialized as an array of Subtitle
    -- So we define it as an array type with Subtitle items
    pure $
      OpenApi.NamedSchema (Just "SubtitlePattern") $
        mempty
          & #type ?~ OpenApi.OpenApiArray
          & #items ?~ OpenApi.OpenApiItemsObject (OpenApi.Ref (OpenApi.Reference "Subtitle"))
          & #description ?~ "Subtitle pattern as array of language codes, e.g. [\"CHS\", \"JAP\"] for 简日"

-- | Check if a list of subtitles exactly matches a pattern
-- Exact match means the sets are equal (order doesn't matter)
matchesPattern :: SubtitlePattern -> [Subtitle] -> Bool
matchesPattern (SubtitlePattern pattern) subs =
  L.sort pattern == L.sort subs

--------------------------------------------------------------------------------
-- Subtitle Extensions
--------------------------------------------------------------------------------

-- | Supported subtitle file extensions (lowercase, with dot)
subtitleExtensions :: [Text]
subtitleExtensions =
  [ ".ass"
  , ".ssa"
  , ".srt"
  , ".sub"
  , ".idx"
  , ".vtt"
  ]

-- | Check if an extension is a subtitle extension
isSubtitleExtension :: Text -> Bool
isSubtitleExtension ext = T.toLower ext `elem` subtitleExtensions

--------------------------------------------------------------------------------
-- Subtitle Suffix Extraction
--------------------------------------------------------------------------------

{- | Extract subtitle suffix (language tag + extension) from filename

Looks for patterns like ".chs.ass", ".cht.srt", ".en.ass", ".sc.ass", ".tc.ass"
If no language tag found, returns just the extension.

Examples:
- "video.chs.ass" -> Just ".chs.ass"
- "video.cht.srt" -> Just ".cht.srt"
- "video.en.ass" -> Just ".en.ass"
- "video.sc.ass" -> Just ".sc.ass"
- "video.ass" -> Just ".ass"
- "video.txt" -> Nothing (not a subtitle)
-}
extractSubtitleSuffix :: Text -> Maybe Text
extractSubtitleSuffix filename = do
  -- First check if it's a subtitle file
  ext <- extractExtension filename
  guard $ isSubtitleExtension ext

  -- Try to find language tag before extension
  let withoutExt = T.dropEnd (T.length ext) filename
  case extractExtension withoutExt of
    Just langTag | isLanguageTag langTag ->
      Just $ langTag <> ext
    _ ->
      Just ext
 where
  -- Common language tags used in subtitle filenames
  isLanguageTag :: Text -> Bool
  isLanguageTag tag =
    T.toLower tag `elem`
      [ ".chs", ".cht", ".sc", ".tc"  -- Chinese variants
      , ".en", ".eng", ".jp", ".jpn"  -- English/Japanese
      , ".zh", ".zho", ".zh-hans", ".zh-hant"  -- ISO codes
      , ".简", ".繁", ".简体", ".繁体"  -- Chinese labels
      , ".chi", ".spa", ".fre", ".ger", ".kor"  -- Other languages
      ]

-- | Extract file extension from filename (internal helper)
extractExtension :: Text -> Maybe Text
extractExtension filename =
  case T.breakOnEnd "." filename of
    ("", _) -> Nothing
    (_, ext) | T.null ext -> Nothing
    (_, ext) -> Just $ "." <> ext
