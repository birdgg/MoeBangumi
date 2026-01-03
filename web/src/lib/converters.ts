import type { ParsedSubject, CalendarSubject } from "@/lib/api";
import type { BangumiModalData } from "@/features/bangumi/components";

/**
 * Convert ParsedSubject to BangumiModalData for the add/edit modal
 */
export function subjectToModalData(subject: ParsedSubject): BangumiModalData {
  return {
    bgmtvId: subject.bgmtv_id,
    titleChinese: subject.title_chinese || subject.title_japanese || "",
    titleJapanese: subject.title_chinese ? subject.title_japanese : null,
    posterUrl: subject.poster_url,
    year: subject.air_date ? parseInt(subject.air_date.split("-")[0], 10) || null : null,
    season: subject.season,
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
    titleChinese: subject.parsed_name,
    titleJapanese: subject.title_chinese ? subject.title_japanese : null,
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
