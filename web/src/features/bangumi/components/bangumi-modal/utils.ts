import type { Platform, Rss, ApiRssEntry, RssFormEntry } from "./types";

// Map external platform strings to our Platform enum
export function normalizePlatform(platform: string | null | undefined): Platform {
  if (!platform) return "tv";
  const lower = platform.toLowerCase();
  if (lower === "tv" || lower === "web") return "tv";
  if (lower === "movie" || lower === "剧场版" || lower === "劇場版") return "movie";
  if (lower === "ova" || lower === "oad") return "ova";
  return "tv";
}

// Convert Rss to RssFormEntry
export function rssToFormEntry(rss: Rss): RssFormEntry {
  return {
    url: rss.url,
    filters: rss.exclude_filters,
    include_filters: rss.include_filters,
    group: rss.group,
  };
}

// Convert RssFormEntry to ApiRssEntry
export function formEntryToApiEntry(entry: RssFormEntry): ApiRssEntry {
  return {
    url: entry.url,
    filters: entry.filters,
    include_filters: entry.include_filters,
    group: entry.group,
  };
}
