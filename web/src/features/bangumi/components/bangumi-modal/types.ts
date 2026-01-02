import type { Rss, Platform, RssEntry as ApiRssEntry } from "@/lib/api";

// Unified data interface for both add and edit modes
export interface BangumiModalData {
  // Identity
  id?: number; // Internal DB id (edit mode only)
  bgmtvId: number; // BGM.tv ID
  tmdbId?: number | null;

  // Titles
  titleChinese: string;
  titleJapanese?: string | null;

  // Metadata
  posterUrl?: string | null;
  year?: number | null;
  season?: number;
  totalEpisodes?: number;
  platform?: string | null;
  airDate?: string | null;
  airWeek?: number | null;
  finished?: boolean;
  sourceType?: "webrip" | "bdrip";

  // Mikan
  mikanId?: string | null;

  // Form data (for edit mode prefill)
  episodeOffset?: number;
  autoComplete?: boolean;
  rssEntries?: Rss[];
}

export interface RssFormEntry {
  url: string;
  filters: string[];
  include_filters: string[];
  group?: string | null;
}

export interface BangumiModalProps {
  open: boolean;
  onOpenChange: (open: boolean) => void;
  mode: "add" | "edit";
  data: BangumiModalData;
  onSuccess?: () => void;
}

export interface BangumiFormValues {
  title_chinese: string;
  title_japanese: string;
  episode_offset: number;
  auto_complete: boolean;
  rss_entries: RssFormEntry[];
}

// Re-export API types for convenience
export type { Rss, Platform, ApiRssEntry };
