import type { Platform, Rss, RssFormEntry } from "./types";

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
// Note: The current API Rss type only has url, it doesn't have filter fields
export function rssToFormEntry(rss: Rss): RssFormEntry {
  return {
    url: rss.url,
    filters: [],
    include_filters: [],
    subtitle_group: null,
  };
}

// Convert RssFormEntry to just a URL string for the API
export function formEntryToRssUrl(entry: RssFormEntry): string {
  return entry.url;
}
