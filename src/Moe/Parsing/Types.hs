{- | Anime filename parsing types

This module defines types for parsing anime torrent filenames.
-}
module Moe.Parsing.Types (
  -- * Parse Result
  ParseResult (..),

  -- * Chinese Number Mapping
  chineseNumberMap,
)
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map.Strict qualified as Map
import Moe.Subtitle

--------------------------------------------------------------------------------
-- Parse Result
--------------------------------------------------------------------------------

{- | Complete parsing result from anime filename

Example input:
@
[Lilith-Raws] Sousou no Frieren / 葬送的芙莉蓮 / 葬送のフリーレン - 01 [Baha][WEB-DL][1080p][AVC AAC][CHT][MP4]
@

Result:
@
ParseResult
  { nameEnglish = Just "Sousou no Frieren"
  , nameChinese = Just "葬送的芙莉蓮"
  , nameJapanese = Just "葬送のフリーレン"
  , episode = Just 1
  , season = Nothing
  , subtitleGroup = Just "Lilith-Raws"
  , resolution = Just "1080P"
  , subtitles = [CHT]
  }
@
-}
data ParseResult = ParseResult
  { nameEnglish :: Maybe Text
  -- ^ English title
  , nameChinese :: Maybe Text
  -- ^ Chinese title (simplified or traditional)
  , nameJapanese :: Maybe Text
  -- ^ Japanese title (hiragana/katakana/kanji)
  , episode :: Maybe Int
  -- ^ Episode number
  , season :: Maybe Int
  -- ^ Season number (1-based, e.g., "S2" = 2, "第二季" = 2)
  , subtitleGroup :: Maybe Text
  -- ^ Fansub group name (e.g., "Lilith-Raws", "ANi")
  , resolution :: Maybe Text
  -- ^ Video resolution ("480P", "720P", "1080P", "2160P")
  , subtitles :: [Subtitle]
  -- ^ Available subtitle languages
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

--------------------------------------------------------------------------------
-- Chinese Number Mapping
--------------------------------------------------------------------------------

-- | Mapping from Chinese numerals to integers (1-10)
chineseNumberMap :: Map Text Int
chineseNumberMap =
  Map.fromList
    [ ("一", 1)
    , ("二", 2)
    , ("三", 3)
    , ("四", 4)
    , ("五", 5)
    , ("六", 6)
    , ("七", 7)
    , ("八", 8)
    , ("九", 9)
    , ("十", 10)
    ]
