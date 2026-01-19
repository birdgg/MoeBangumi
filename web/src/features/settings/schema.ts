import { z } from "zod";

/**
 * Subtitle language types (must match backend Subtitle enum)
 */
export const subtitleLanguages = ["CHS", "CHT", "JAP", "ENG"] as const;
export type SubtitleLanguage = (typeof subtitleLanguages)[number];

/**
 * Subtitle pattern - a combination of subtitle languages
 * e.g. ["CHS", "JAP"] for 简日
 */
export type SubtitlePattern = SubtitleLanguage[];

/**
 * Default subtitle patterns in priority order
 * These are the 4 fixed options for the UI
 */
export const defaultSubtitlePatterns: SubtitlePattern[] = [
  ["CHS", "JAP"], // 简日
  ["CHT", "JAP"], // 繁日
  ["CHS"],        // 简
  ["CHT"],        // 繁
];

/**
 * Get display label for a subtitle pattern
 */
export function subtitlePatternLabel(pattern: SubtitlePattern): string {
  const sorted = [...pattern].sort();
  const key = sorted.join("+");
  const labels: Record<string, string> = {
    "CHS": "简",
    "CHT": "繁",
    "CHS+JAP": "简日",
    "CHT+JAP": "繁日",
    "JAP": "日",
    "ENG": "英",
  };
  return labels[key] ?? pattern.join("+");
}

/**
 * Check if two patterns are equal (order doesn't matter)
 */
export function patternsEqual(a: SubtitlePattern, b: SubtitlePattern): boolean {
  if (a.length !== b.length) return false;
  const sortedA = [...a].sort();
  const sortedB = [...b].sort();
  return sortedA.every((v, i) => v === sortedB[i]);
}

/**
 * Get unique key for a pattern (for React keys)
 */
export function patternKey(pattern: SubtitlePattern): string {
  return [...pattern].sort().join("+");
}

/**
 * Merge saved patterns with default patterns.
 * Always returns exactly 4 fixed patterns, reordered based on saved order.
 * Patterns not in defaults are ignored, missing patterns are appended.
 */
export function mergeWithDefaultPatterns(saved: SubtitlePattern[]): SubtitlePattern[] {
  const result: SubtitlePattern[] = [];
  const usedKeys = new Set<string>();

  // First, add patterns from saved order that exist in defaults
  for (const pattern of saved) {
    const key = patternKey(pattern);
    const defaultPattern = defaultSubtitlePatterns.find(p => patternKey(p) === key);
    if (defaultPattern && !usedKeys.has(key)) {
      result.push(defaultPattern);
      usedKeys.add(key);
    }
  }

  // Then, append any missing default patterns
  for (const pattern of defaultSubtitlePatterns) {
    const key = patternKey(pattern);
    if (!usedKeys.has(key)) {
      result.push(pattern);
      usedKeys.add(key);
    }
  }

  return result;
}

/**
 * Downloader settings schema (matches backend DownloaderSettings)
 */
export const downloaderSchema = z.object({
  url: z.string(),
  username: z.string(),
  password: z.string(),
  savePath: z.string(),
});

/**
 * Filter configuration schema (matches backend FilterSettings)
 */
export const filterSchema = z.object({
  globalRssFilters: z.array(z.string()),
});

/**
 * Telegram configuration schema (matches backend TelegramConfig)
 */
export const telegramConfigSchema = z.object({
  botToken: z.string(),
  chatId: z.string(),
});

/**
 * Notification settings schema (matches backend NotificationSettings)
 */
export const notificationSchema = z.object({
  telegram: telegramConfigSchema,
});

/**
 * Priority settings schema (matches backend PrioritySettings)
 * languages is an array of subtitle patterns, e.g. [["CHS", "JAP"], ["CHT", "JAP"], ["CHS"], ["CHT"]]
 */
export const prioritySchema = z.object({
  groups: z.array(z.string()),
  languages: z.array(z.array(z.enum(subtitleLanguages))),
});

/**
 * TMDB configuration schema (matches backend TmdbSettings)
 */
export const tmdbSchema = z.object({
  apiKey: z.string(),
});

/**
 * Emby configuration schema (matches backend EmbySettings)
 * Emby is considered enabled when both url and apiKey are non-empty.
 */
export const embySchema = z.object({
  url: z.string(),
  apiKey: z.string(),
});

/**
 * Complete settings form schema (matches backend Settings)
 */
export const settingsFormSchema = z.object({
  downloader: downloaderSchema,
  filter_: filterSchema,
  notification: notificationSchema,
  priority: prioritySchema,
  tmdb: tmdbSchema,
  emby: embySchema,
});

/**
 * TypeScript types inferred from schemas
 */
export type DownloaderFormData = z.infer<typeof downloaderSchema>;
export type FilterFormData = z.infer<typeof filterSchema>;
export type TelegramConfigFormData = z.infer<typeof telegramConfigSchema>;
export type NotificationFormData = z.infer<typeof notificationSchema>;
export type PriorityFormData = z.infer<typeof prioritySchema>;
export type TmdbFormData = z.infer<typeof tmdbSchema>;
export type EmbyFormData = z.infer<typeof embySchema>;
export type SettingsFormData = z.infer<typeof settingsFormSchema>;
