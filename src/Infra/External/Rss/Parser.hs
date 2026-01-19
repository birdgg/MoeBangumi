{- | RSS parser selection based on URL domain

This module provides URL-based parser selection for different RSS sources.
Each source (Mikan, Nyaa, ACG.rip, DMHY) has different XML structures and
info_hash extraction methods.

== Usage

@
import Infra.External.Rss.Parser (selectParser)

let parser = selectParser rssUrl
let items = parser.parseItems rawItems
@

== Adding a New Source

1. Create a new parser function following the pattern of 'mikanParser'
2. Add the domain mapping in 'selectParser'
-}
module Infra.External.Rss.Parser (
  -- * Types
  RssParser (..),
  ParsedRssItem (..),

  -- * Parser Selection
  selectParser,
  RssSource (..),
  detectSource,

  -- * Individual Parsers
  mikanParser,
  nyaaParser,
  acgripParser,
  dmhyParser,
  genericParser,

  -- * Utilities (for testing)
  extractInfoHashFromUrl,
  extractInfoHashFromMagnet,
)
where

import Data.Char (isHexDigit)
import Data.Text qualified as T
import Infra.External.Rss.Types (RawItem (..))
import Moe.Parsing.Anime (parseFilename)
import Moe.Parsing.Types (ParseResult)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

{- | RSS source identifier

Used for logging and debugging purposes.
-}
data RssSource
  = Mikan
  | Nyaa
  | Acgrip
  | Dmhy
  | Unknown
  deriving stock (Show, Eq)

{- | RSS item with parsed metadata and torrent information

Combines the ParseResult (title parsing) with torrent-specific data
extracted from the RawItem.
-}
data ParsedRssItem = ParsedRssItem
  { parseResult :: ParseResult
  -- ^ Parsed metadata from title
  , originalTitle :: Text
  -- ^ Original title (for washing quality comparison)
  , torrentUrl :: Text
  -- ^ Torrent URL (from RawItem.enclosure or link)
  , infoHash :: Text
  -- ^ Info hash extracted from URL or XML element
  , pubDate :: Maybe Text
  -- ^ Publication date for filtering
  }
  deriving stock (Show, Eq)

{- | RSS parser with source-specific extraction logic

Each RSS source may have different:
- XML structure (enclosure vs link for torrent URL)
- Info hash extraction method (URL path, magnet link, XML element)
- Title format (may need preprocessing)
-}
data RssParser = RssParser
  { parseItems :: [RawItem] -> [ParsedRssItem]
  -- ^ Parse raw items into domain items with info_hash
  , sourceName :: RssSource
  -- ^ Source identifier for logging
  }

--------------------------------------------------------------------------------
-- Parser Selection
--------------------------------------------------------------------------------

{- | Detect RSS source from URL domain

Checks if the URL contains known domain patterns.
Returns 'Unknown' if no match is found.
-}
detectSource :: Text -> RssSource
detectSource url
  | "mikanani.me" `T.isInfixOf` url = Mikan
  | "mikan.moe" `T.isInfixOf` url = Mikan
  | "nyaa.si" `T.isInfixOf` url = Nyaa
  | "acg.rip" `T.isInfixOf` url = Acgrip
  | "share.dmhy.org" `T.isInfixOf` url = Dmhy
  | "dmhy.org" `T.isInfixOf` url = Dmhy
  | otherwise = Unknown

{- | Select parser based on RSS feed URL

Automatically detects the RSS source from the URL domain
and returns the appropriate parser.

Falls back to 'genericParser' for unknown sources.
-}
selectParser :: Text -> RssParser
selectParser url =
  case detectSource url of
    Mikan -> mikanParser
    Nyaa -> nyaaParser
    Acgrip -> acgripParser
    Dmhy -> dmhyParser
    Unknown -> genericParser

--------------------------------------------------------------------------------
-- Individual Parsers
--------------------------------------------------------------------------------

{- | Parser for Mikan (mikanani.me)

Mikan provides torrent URLs in the format:
@https://mikanani.me/Download/YYYYMMDD/\<40-char-hash\>.torrent@

Info hash is extracted from the filename in the URL.
-}
mikanParser :: RssParser
mikanParser =
  RssParser
    { parseItems = mapMaybe extractMikan
    , sourceName = Mikan
    }
 where
  extractMikan :: RawItem -> Maybe ParsedRssItem
  extractMikan raw = do
    title <- raw.title
    result <- parseFilename title
    url <- raw.enclosure
    hash <- extractInfoHashFromUrl url
    pure
      ParsedRssItem
        { parseResult = result
        , originalTitle = title
        , torrentUrl = url
        , infoHash = hash
        , pubDate = raw.pubDate
        }

{- | Parser for Nyaa.si

Nyaa provides:
- Info hash in @\<nyaa:infoHash\>@ XML element (most reliable)
- Magnet links in enclosure (fallback)
- Torrent file links

Priority: XML element > magnet link > URL extraction
-}
nyaaParser :: RssParser
nyaaParser =
  RssParser
    { parseItems = mapMaybe extractNyaa
    , sourceName = Nyaa
    }
 where
  extractNyaa :: RawItem -> Maybe ParsedRssItem
  extractNyaa raw = do
    title <- raw.title
    result <- parseFilename title
    -- Nyaa uses link for torrent page, enclosure for magnet/torrent
    url <- raw.enclosure <|> raw.link
    -- Priority: XML infoHash > magnet extraction > URL extraction
    hash <-
      raw.nyaaInfoHash
        <|> extractInfoHashFromMagnet url
        <|> extractInfoHashFromUrl url
    pure
      ParsedRssItem
        { parseResult = result
        , originalTitle = title
        , torrentUrl = url
        , infoHash = hash
        , pubDate = raw.pubDate
        }

{- | Parser for ACG.rip

ACG.rip typically provides magnet links.
Info hash is extracted from the magnet URI.
-}
acgripParser :: RssParser
acgripParser =
  RssParser
    { parseItems = mapMaybe extractMagnetBased
    , sourceName = Acgrip
    }

{- | Parser for 动漫花园 (share.dmhy.org)

DMHY typically provides magnet links in the enclosure.
-}
dmhyParser :: RssParser
dmhyParser =
  RssParser
    { parseItems = mapMaybe extractMagnetBased
    , sourceName = Dmhy
    }

{- | Generic parser for unknown sources

Tries both magnet and URL-based extraction.
This is a safe fallback for sources we don't recognize.
-}
genericParser :: RssParser
genericParser =
  RssParser
    { parseItems = mapMaybe extractMagnetBased
    , sourceName = Unknown
    }

{- | Common extraction logic for magnet-based sources

Used by ACG.rip, DMHY, and generic parser.
Extracts info hash from magnet URI first, falls back to URL extraction.
-}
extractMagnetBased :: RawItem -> Maybe ParsedRssItem
extractMagnetBased raw = do
  title <- raw.title
  result <- parseFilename title
  url <- raw.enclosure <|> raw.link
  hash <- extractInfoHashFromMagnet url <|> extractInfoHashFromUrl url
  pure
    ParsedRssItem
      { parseResult = result
      , originalTitle = title
      , torrentUrl = url
      , infoHash = hash
      , pubDate = raw.pubDate
      }

--------------------------------------------------------------------------------
-- Info Hash Extraction Utilities
--------------------------------------------------------------------------------

{- | Extract info hash from torrent URL

Supports URL formats like:
@https://mikanani.me/Download/20260115/\<40-char-hash\>.torrent@

Returns the 40-character hex string before .torrent extension.
-}
extractInfoHashFromUrl :: Text -> Maybe Text
extractInfoHashFromUrl url =
  let filename = T.takeWhileEnd (/= '/') url
      -- Remove .torrent extension if present
      base = fromMaybe filename $ T.stripSuffix ".torrent" filename
   in if T.length base == 40 && T.all isHexDigit base
        then Just (T.toLower base)
        else Nothing

{- | Extract info hash from magnet URI

Magnet format: @magnet:?xt=urn:btih:\<info_hash\>&...@

Only supports 40-character hex string (SHA-1) info hashes.
Base32 encoded hashes (32 characters) are not currently supported.
-}
extractInfoHashFromMagnet :: Text -> Maybe Text
extractInfoHashFromMagnet url
  | not ("magnet:?" `T.isPrefixOf` url) = Nothing
  | otherwise =
      let params = T.split (== '&') (T.drop 8 url) -- drop "magnet:?"
          btihParam = find ("xt=urn:btih:" `T.isPrefixOf`) params
       in case btihParam of
            Nothing -> Nothing
            Just param ->
              let hash = T.drop 12 param -- drop "xt=urn:btih:"
                  -- Handle potential trailing parameters
                  cleanHash = T.takeWhile (/= '&') hash
               in if T.length cleanHash == 40 && T.all isHexDigit cleanHash
                    then Just (T.toLower cleanHash)
                    else Nothing
