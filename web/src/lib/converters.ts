import type { SearchedMetadata, CalendarSubject, BangumiWithMetadata } from "@/lib/api";
import type { BangumiModalData } from "@/features/bangumi/components";

/**
 * Convert SearchedMetadata to BangumiModalData for the add/edit modal
 */
export function subjectToModalData(subject: SearchedMetadata): BangumiModalData {
  return {
    bgmtvId: parseInt(subject.external_id, 10) || 0,
    titleChinese: subject.title_chinese || subject.title_japanese || "",
    titleJapanese: subject.title_chinese ? subject.title_japanese : null,
    posterUrl: subject.poster_url,
    year: subject.air_date ? parseInt(subject.air_date.split("-")[0], 10) || null : null,
    season: subject.season ?? undefined,
    totalEpisodes: subject.total_episodes,
    platform: subject.platform,
    airDate: subject.air_date,
    airWeek: subject.air_date ? new Date(subject.air_date).getDay() : null,
  };
}

/**
 * Convert CalendarSubject to BangumiModalData for the add modal
 */
export function calendarSubjectToModalData(
  subject: CalendarSubject
): BangumiModalData {
  return {
    bgmtvId: subject.bgmtv_id ?? 0,
    titleChinese: subject.title_chinese,
    titleJapanese: subject.title_japanese,
    posterUrl: subject.poster_url,
    year: subject.air_date
      ? parseInt(subject.air_date.split("-")[0], 10) || null
      : null,
    season: subject.season,
    totalEpisodes: subject.total_episodes,
    platform: subject.platform,
    airDate: subject.air_date,
    airWeek: subject.air_week,
    mikanId: subject.mikan_id,
  };
}

/**
 * Convert BangumiWithMetadata to BangumiModalData for the edit modal
 */
export function bangumiToModalData(
  bangumi: BangumiWithMetadata
): BangumiModalData {
  const { metadata } = bangumi;
  return {
    id: bangumi.id,
    bgmtvId: metadata.bgmtv_id ?? 0,
    tmdbId: metadata.tmdb_id,
    titleChinese: metadata.title_chinese,
    titleJapanese: metadata.title_japanese,
    posterUrl: metadata.poster_url,
    year: metadata.year,
    season: metadata.season,
    totalEpisodes: metadata.total_episodes,
    platform: metadata.platform,
    airDate: metadata.air_date,
    airWeek: metadata.air_week,
    mikanId: metadata.mikan_id,
    episodeOffset: metadata.episode_offset,
    autoComplete: bangumi.auto_complete,
  };
}
