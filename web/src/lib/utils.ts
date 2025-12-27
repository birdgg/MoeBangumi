import { clsx, type ClassValue } from "clsx"
import { twMerge } from "tailwind-merge"

export function cn(...inputs: ClassValue[]) {
  return twMerge(clsx(inputs))
}

/**
 * Characters that are illegal in file/directory names on Windows and Unix
 */
const ILLEGAL_CHARS = /[<>:"/\\|?*]/g;

/**
 * Sanitize a path component by removing illegal characters
 * Mirrors the logic from crates/pathgen/src/sanitizer.rs
 */
function sanitizePathComponent(component: string): string {
  let result = component.replace(ILLEGAL_CHARS, " ");
  result = result.trim();
  // Compress multiple consecutive spaces
  result = result.replace(/\s+/g, " ");
  // Remove trailing dots (Windows restriction)
  result = result.replace(/\.+$/, "");
  return result || "Unknown";
}

/**
 * Generate a save path for bangumi downloads
 * Mirrors the logic from crates/pathgen/src/formatter.rs
 *
 * Format for TV shows:
 * `{basePath}/{title} ({year}) {tmdb-ID}/Season XX`
 *
 * Format for movies:
 * `{basePath}/{title} ({year}) {tmdb-ID}`
 */
export function generateSavePath(options: {
  basePath: string;
  title: string;
  year: number;
  season: number;
  tmdbId?: number | null;
  kind?: string | null;
}): string {
  const { basePath, title, year, season, tmdbId, kind } = options;
  const sanitizedTitle = sanitizePathComponent(title);
  const isMovie = kind === "Movie";

  // Format base directory: "迷宫饭 (2024) {tmdb-119121}" or "迷宫饭 (2024)"
  const baseDir = tmdbId
    ? `${sanitizedTitle} (${year}) {tmdb-${tmdbId}}`
    : `${sanitizedTitle} (${year})`;

  // Movies don't need Season directory
  if (isMovie) {
    return `${basePath}/${baseDir}`;
  }

  // TV shows: add Season directory
  const seasonDir = `Season ${season.toString().padStart(2, "0")}`;
  return `${basePath}/${baseDir}/${seasonDir}`;
}
