import type { SearchedMetadata, CalendarSubject } from "@/lib/api";
import type { BangumiModalData } from "@/features/bangumi/components";

/**
 * Convert SearchedMetadata to BangumiModalData for the add/edit modal
 */
export function subjectToModalData(subject: SearchedMetadata): BangumiModalData {
  return {
    bgmtvId: parseInt(subject.external_id, 10) || 0,
    titleChinese: subject.title_chinese || subject.title_original || "",
    titleJapanese: subject.title_chinese ? subject.title_original : null,
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
