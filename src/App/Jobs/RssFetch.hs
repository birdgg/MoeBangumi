{- | RSS fetch job for periodic feed updates

This module re-exports the RSS fetch job functionality from submodules.

== Module Structure

- "App.Jobs.RssFetch.Job" - Job definition entry point
- "App.Jobs.RssFetch.Fetch" - Core fetch logic
- "App.Jobs.RssFetch.Filter" - Date and pattern filtering
- "App.Jobs.RssFetch.Washing" - Torrent quality selection
- "App.Jobs.RssFetch.Download" - qBittorrent integration
- "App.Jobs.RssFetch.Types" - Configuration constants

== Usage

@
import App.Jobs.RssFetch (rssFetchJob)
import App.Jobs.Scheduler (runJobsConcurrently)

-- In your server startup:
let jobs = [rssFetchJob logger env]
runJobsConcurrently jobs
@
-}
module App.Jobs.RssFetch
  ( -- * Job Definition
    rssFetchJob

    -- * Core Logic (for testing and extension)
  , fetchAllEnabledRss
  , fetchSingleRss
  , processRssItems

    -- * Parsed RSS Item (re-exported from Parser)
  , ParsedRssItem (..)
  , selectBestByWashing

    -- * Helpers (exported for testing)
  , filterNewItems
  , findNewestPubDate
  , parseRfc822
  , filterByGlobalPatterns
  , parseRssItems
  , filterLatestEpisode
  )
where

-- Re-export from submodules
import App.Jobs.RssFetch.Download ()
import App.Jobs.RssFetch.Fetch (fetchAllEnabledRss, fetchSingleRss, parseRssItems, processRssItems)
import App.Jobs.RssFetch.Filter (filterByGlobalPatterns, filterLatestEpisode, filterNewItems, findNewestPubDate, parseRfc822)
import App.Jobs.RssFetch.Job (rssFetchJob)
import App.Jobs.RssFetch.Washing (selectBestByWashing)

-- Re-export ParsedRssItem from Parser
import Infra.External.Rss.Parser (ParsedRssItem (..))
