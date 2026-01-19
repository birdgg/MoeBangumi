{- | Core RSS fetch logic

This module provides the core RSS fetching functionality:
- Concurrent fetching with semaphore-based throttling
- Single feed processing with error handling
- RSS item processing and parsing

== Architecture

The fetch flow is:
1. 'fetchAllEnabledRss' - Entry point, fetches all enabled RSS feeds concurrently
2. 'fetchSingleRss' - Processes a single RSS feed
3. 'processRssItems' - Parses and processes items from a feed

== Extension Points

This module coordinates between:
- Filter module (date and pattern filtering)
- Washing module (quality selection)
- Download module (torrent downloading)
-}
module App.Jobs.RssFetch.Fetch
  ( -- * Main Fetch Operations
    fetchAllEnabledRss
  , fetchSingleRss
  , processRssItems

    -- * Parsing
  , parseRssItems

    -- * Concurrency Helpers
  , withSem
  )
where

import Data.Aeson (object, (.=))
import Data.Text.Display (display)
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.Async (forConcurrently_)
import Effectful.Concurrent.QSem (QSem, newQSem, signalQSem, waitQSem)
import Effectful.Exception (bracket)
import Effectful.Log (Log)
import Effectful.Log qualified as Log
import Effectful.Reader.Static qualified as Reader

import App.Jobs.RssFetch.Download (downloadSelectedItems)
import App.Jobs.RssFetch.Filter (filterAndFindNewest, filterByGlobalPatterns, filterLatestEpisodeParsed)
import App.Jobs.RssFetch.Types (maxConcurrentFetches)
import App.Jobs.RssFetch.Washing (selectBestByWashing)
import Infra.Database.Effect (DB)
import Infra.Database.Repository.Bangumi qualified as BangumiRepo
import Infra.Database.Repository.Rss qualified as RssRepo
import Infra.Database.Repository.Subscription qualified as SubRepo
import Infra.Downloader.Effect (Downloader)
import Infra.Environment.Env (MoeEnv (..))
import Infra.External.Rss.Effect (Rss)
import Infra.External.Rss.Effect qualified as RssEffect
import Infra.External.Rss.Parser (ParsedRssItem (..), RssParser (..), selectParser)
import Infra.External.Rss.Types (RawItem (..))
import Moe.Subscription (Subscription (..))
import Moe.Parsing.Anime (parseFilename)
import Moe.Parsing.Types (ParseResult)
import Moe.Rss qualified
import Moe.Setting (FilterSettings (globalRssFilters), Settings (..))

--------------------------------------------------------------------------------
-- Core Fetch Logic
--------------------------------------------------------------------------------

{- | Fetch all enabled RSS subscriptions concurrently (with limit)

Queries the database for enabled RSS entries and fetches them in parallel,
with a maximum of 'maxConcurrentFetches' concurrent requests.
Individual fetch failures are logged but don't stop the overall process.
-}
fetchAllEnabledRss ::
  ( DB :> es
  , Rss :> es
  , Downloader :> es
  , Log :> es
  , Concurrent :> es
  , Reader.Reader MoeEnv :> es
  , IOE :> es
  ) =>
  Eff es ()
fetchAllEnabledRss = do
  rssEntries <- RssRepo.findAllEnabled
  let !totalFeeds = length rssEntries

  Log.logInfo "Starting RSS fetch cycle" $
    object
      [ "total_feeds" .= totalFeeds
      , "max_concurrent" .= maxConcurrentFetches
      ]

  -- Create semaphore to limit concurrent fetches
  sem <- newQSem maxConcurrentFetches

  -- Fetch feeds concurrently with semaphore-based throttling
  forConcurrently_ rssEntries $ \rss ->
    withSem sem $ fetchSingleRss rss

  Log.logInfo "Completed RSS fetch cycle" $
    object ["processed_feeds" .= totalFeeds]

{- | Run an action with semaphore-based concurrency control

Ensures the semaphore is released even if the action throws.
-}
withSem :: (Concurrent :> es) => QSem -> Eff es a -> Eff es a
withSem sem action =
  bracket
    (waitQSem sem)
    (\_ -> signalQSem sem)
    (const action)

{- | Fetch a single RSS feed with error handling

Attempts to fetch the RSS feed and logs the outcome.
On success, filters new items and passes them to processRssItems.
On failure, logs the error and continues.
-}
fetchSingleRss ::
  ( DB :> es
  , Rss :> es
  , Downloader :> es
  , Log :> es
  , Reader.Reader MoeEnv :> es
  , IOE :> es
  ) =>
  Moe.Rss.Rss ->
  Eff es ()
fetchSingleRss rss = do
  Log.logTrace "Fetching RSS feed" $
    object
      [ "rss_id" .= rss.id
      , "subscription_id" .= rss.subscriptionId
      , "url" .= rss.url
      , "last_processed_pub_date" .= rss.lastProcessedPubDate
      ]

  result <- RssEffect.fetchRss rss.url

  case result of
    Left err ->
      Log.logAttention "RSS fetch failed" $
        object
          [ "rss_id" .= rss.id
          , "url" .= rss.url
          , "error" .= display err
          ]
    Right items -> do
      -- Get global RSS filters from settings
      env <- Reader.ask @MoeEnv
      settings :: Settings <- readTVarIO env.settingsVar
      let globalFilters = settings.filter_.globalRssFilters

      -- Select parser based on URL
      let parser = selectParser rss.url

      -- Apply filters: first by pubDate, then by regex patterns
      let (newItems, newestPubDate) = filterAndFindNewest rss.lastProcessedPubDate items
          filteredItems = filterByGlobalPatterns globalFilters newItems
          !totalItems = length items
          !newItemCount = length newItems
          !filteredCount = length filteredItems
          !dateFilteredOut = totalItems - newItemCount
          !regexFilteredOut = newItemCount - filteredCount

      -- Log filter statistics for debugging
      Log.logTrace "RSS filter statistics" $
        object
          [ "rss_id" .= rss.id
          , "total_items" .= totalItems
          , "date_filtered_out" .= dateFilteredOut
          , "after_date_filter" .= newItemCount
          , "regex_filtered_out" .= regexFilteredOut
          , "after_regex_filter" .= filteredCount
          , "global_filters" .= globalFilters
          ]

      -- Parse titles and process items using source-specific parser
      parsed <- processRssItems rss parser filteredItems newestPubDate
      let !parsedCount = length parsed
          !parseFailedCount = filteredCount - parsedCount

      -- Log parsing statistics for debugging
      when (parseFailedCount > 0) $
        Log.logTrace "RSS parsing statistics" $
          object
            [ "rss_id" .= rss.id
            , "items_to_parse" .= filteredCount
            , "parse_failed" .= parseFailedCount
            , "parsed_count" .= parsedCount
            ]

      Log.logInfo "RSS fetched and parsed" $
        object
          [ "rss_id" .= rss.id
          , "subscription_id" .= rss.subscriptionId
          , "url" .= rss.url
          , "source" .= (show parser.sourceName :: String)
          , "total_items" .= totalItems
          , "new_items" .= newItemCount
          , "after_filter" .= filteredCount
          , "parsed_count" .= parsedCount
          ]

{- | Process fetched RSS items

Uses the source-specific parser to extract metadata and info_hash,
updates the lastProcessedPubDate, and returns the successfully parsed results.

Items without a title or with unparseable titles are silently skipped.

If the associated Subscription has autoComplete=False, only items with the
highest episode number are returned.
-}
processRssItems ::
  (DB :> es, Downloader :> es, Log :> es, Reader.Reader MoeEnv :> es, IOE :> es) =>
  Moe.Rss.Rss ->
  RssParser ->
  -- ^ Source-specific parser selected based on URL
  [RawItem] ->
  -- | Newest pubDate to update
  Maybe Text ->
  Eff es [ParsedRssItem]
processRssItems rss parser items newestPubDate = do
  -- Parse all items using source-specific parser
  let parsed = parser.parseItems items

  -- Check if autoComplete is disabled for this subscription
  subscription <- SubRepo.getOne rss.subscriptionId
  let autoComplete = maybe True (.autoComplete) subscription
      filteredResults =
        if autoComplete
          then parsed
          else filterLatestEpisodeParsed parsed

  -- Update lastProcessedPubDate to the newest pubDate
  case newestPubDate of
    Nothing -> pass
    Just pubDate ->
      RssRepo.updateLastProcessedPubDate rss.id (Just pubDate)

  -- Apply washing comparison and trigger downloads
  case subscription of
    Nothing -> do
      Log.logAttention "Subscription not found, skipping downloads" $
        object
          [ "subscription_id" .= rss.subscriptionId
          , "rss_id" .= rss.id
          ]
      pure filteredResults
    Just sub -> do
      -- Get bangumi for this subscription
      bangumi <- BangumiRepo.getOne sub.bangumiId
      case bangumi of
        Nothing -> do
          Log.logAttention "Bangumi not found for subscription, skipping downloads" $
            object
              [ "subscription_id" .= sub.id
              , "bangumi_id" .= sub.bangumiId
              , "rss_id" .= rss.id
              ]
          pure filteredResults
        Just bg -> do
          -- Get priority settings for washing comparison
          env <- Reader.ask @MoeEnv
          settings :: Settings <- readTVarIO env.settingsVar
          let priorities = settings.priority

          -- Select best items via washing comparison
          toDownload <- selectBestByWashing sub.id priorities filteredResults

          -- Download selected items
          downloadSelectedItems sub bg rss.id toDownload

          pure filteredResults

--------------------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------------------

{- | Parse RSS items' titles into ParseResults

Items without a title or with unparseable titles are silently skipped.
Uses 'mapMaybe' for efficient filter + map in a single pass.
-}
parseRssItems :: [RawItem] -> [ParseResult]
parseRssItems = mapMaybe $ \item ->
  item.title >>= parseFilename
