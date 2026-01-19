{- | RSS item filtering and date processing

This module provides filtering logic for RSS items:
- Filter new items based on publication date
- Apply global regex patterns to exclude items
- Filter to keep only latest episode items
- RFC 822 date parsing utilities
-}
module App.Jobs.RssFetch.Filter (
  -- * Main Filtering
  filterAndFindNewest,
  filterNewItems,
  filterByGlobalPatterns,

  -- * Episode Filtering
  filterLatestEpisode,
  filterLatestEpisodeParsed,

  -- * Date Utilities
  findNewestPubDate,
  parseRfc822,
)
where

import Data.Time (UTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Text.Regex.TDFA ((=~))

import Infra.External.Rss.Parser (ParsedRssItem (..))
import Infra.External.Rss.Types (RawItem (..))
import Moe.Parsing.Types (ParseResult (..))

--------------------------------------------------------------------------------
-- Main Filtering
--------------------------------------------------------------------------------

{- | Filter new items and find newest pubDate in a single pass

This is an optimized version that:
1. Filters items to only include new ones based on pubDate
2. Finds the newest pubDate among new items
3. Does both in a single traversal

Items are considered new if:
- They have a pubDate AND
- lastProcessedPubDate is Nothing (never processed) OR
- Their parsed pubDate is greater than lastProcessedPubDate

Items without pubDate or with unparseable pubDate are skipped.
-}
filterAndFindNewest :: Maybe Text -> [RawItem] -> ([RawItem], Maybe Text)
filterAndFindNewest _lastPubDateText items =
  -- TODO: temporarily disabled date filtering for debugging
  (items, Nothing)

{-
let lastPubDate = lastPubDateText >>= parseRfc822

    -- Single pass: filter and find max in one traversal
    go :: ([RawItem], Maybe (UTCTime, Text)) -> RawItem -> ([RawItem], Maybe (UTCTime, Text))
    go (acc, maxDate) item =
      case item.pubDate >>= (\pd -> (,pd) <$> parseRfc822 pd) of
        Nothing -> (acc, maxDate) -- Skip items without valid pubDate
        Just (itemTime, itemPubDateText) ->
          let isNew = case lastPubDate of
                Nothing -> True -- Never processed, all items are new
                Just lastTime -> itemTime > lastTime
              newMax = case maxDate of
                Nothing -> Just (itemTime, itemPubDateText)
                Just (maxTime, _) ->
                  if itemTime > maxTime
                    then Just (itemTime, itemPubDateText)
                    else maxDate
           in if isNew
                then (item : acc, newMax)
                else (acc, maxDate)

    (newItems, newestDate) = foldl' go ([], Nothing) items
 in (reverse newItems, snd <$> newestDate)
-}

{- | Filter items to only include new ones based on pubDate

Items are considered new if:

1. They have a pubDate AND
2. lastProcessedPubDate is Nothing (never processed) OR
3. Their parsed pubDate is greater than lastProcessedPubDate

Items without pubDate or with unparseable pubDate are skipped.
-}
filterNewItems :: Maybe Text -> [RawItem] -> [RawItem]
filterNewItems lastPubDateText items =
  fst $ filterAndFindNewest lastPubDateText items

{- | Filter RSS items by global regex patterns

Excludes items whose title matches any of the provided regex patterns.
Items without a title pass through (not excluded).
-}
filterByGlobalPatterns :: [Text] -> [RawItem] -> [RawItem]
filterByGlobalPatterns patterns items
  | null patterns = items
  | otherwise = filter (not . matchesAnyPattern) items
 where
  matchesAnyPattern :: RawItem -> Bool
  matchesAnyPattern item =
    case item.title of
      Nothing -> False
      Just title -> any (\pat -> (title =~ pat :: Bool)) patterns

--------------------------------------------------------------------------------
-- Episode Filtering
--------------------------------------------------------------------------------

{- | Filter to keep only items with the highest episode number

Used when autoComplete=False to only process the latest episode.
Items without an episode number are excluded.
Returns all items that share the maximum episode number.
-}
filterLatestEpisode :: [ParseResult] -> [ParseResult]
filterLatestEpisode results =
  case maxEpisode of
    Nothing -> []
    Just maxEp -> filter (\r -> r.episode == Just maxEp) results
 where
  -- Find the maximum episode number using foldl'
  episodes = mapMaybe (.episode) results
  maxEpisode = case episodes of
    [] -> Nothing
    (x : xs) -> Just $ foldl' max x xs

{- | Filter ParsedRssItems to keep only items with the highest episode number

Similar to 'filterLatestEpisode' but operates on 'ParsedRssItem'.
-}
filterLatestEpisodeParsed :: [ParsedRssItem] -> [ParsedRssItem]
filterLatestEpisodeParsed items =
  case maxEpisode of
    Nothing -> []
    Just maxEp -> filter (\r -> r.parseResult.episode == Just maxEp) items
 where
  episodes = mapMaybe (.parseResult.episode) items
  maxEpisode = case episodes of
    [] -> Nothing
    (x : xs) -> Just $ foldl' max x xs

--------------------------------------------------------------------------------
-- Date Utilities
--------------------------------------------------------------------------------

{- | Find the newest pubDate from a list of items

Parses all pubDates and returns the one with latest timestamp.
Returns Nothing if no items have valid pubDates.
-}
findNewestPubDate :: [RawItem] -> Maybe Text
findNewestPubDate items =
  let withParsed =
        mapMaybe
          ( \item -> do
              pd <- item.pubDate
              time <- parseRfc822 pd
              pure (time, pd)
          )
          items
   in case withParsed of
        [] -> Nothing
        (x : xs) -> Just $ snd $ foldl' maxByTime x xs
 where
  maxByTime acc@(maxTime, _) cur@(time, _)
    | time > maxTime = cur
    | otherwise = acc

{- | Parse date string to UTCTime

Supports multiple formats:
- RFC 822: "Thu, 16 Jan 2026 12:00:00 +0000"
- ISO 8601: "2026-01-16T12:00:00" (used by Mikan)

Returns Nothing if parsing fails.
-}
parseRfc822 :: Text -> Maybe UTCTime
parseRfc822 txt =
  parseTimeM True defaultTimeLocale rfc822Format (toString txt)
    <|> parseTimeM True defaultTimeLocale rfc822FormatNoTz (toString txt)
    <|> parseTimeM True defaultTimeLocale iso8601Format (toString txt)
    <|> parseTimeM True defaultTimeLocale iso8601FormatWithTz (toString txt)
 where
  rfc822Format = "%a, %d %b %Y %H:%M:%S %z"
  rfc822FormatNoTz = "%a, %d %b %Y %H:%M:%S %Z"
  iso8601Format = "%Y-%m-%dT%H:%M:%S"
  iso8601FormatWithTz = "%Y-%m-%dT%H:%M:%S%z"
