import type { Subject, CalendarSubject } from "@/lib/api";
import type { BangumiModalData } from "@/features/bangumi/components";
import { parseBgmtvName } from "@/lib/parser";

/**
 * Convert Subject to BangumiModalData for the add/edit modal
 */
export function subjectToModalData(subject: Subject): BangumiModalData {
  const parsedChinese = parseBgmtvName(subject.name_cn || subject.name || "");
  const parsedJapanese = parseBgmtvName(subject.name || "");
  const season = parsedChinese.season ?? parsedJapanese.season ?? 1;

  return {
    bgmtvId: subject.id,
    titleChinese: parsedChinese.name,
    titleJapanese: subject.name_cn ? parsedJapanese.name : null,
    posterUrl: subject.image,
    year: subject.date ? parseInt(subject.date.split("-")[0], 10) || null : null,
    season,
    totalEpisodes: subject.eps,
    platform: subject.platform,
    airDate: subject.date,
    airWeek: subject.date ? new Date(subject.date).getDay() : null,
  };
}

/**
 * Convert CalendarSubject to BangumiModalData for the add modal
 */
export function calendarSubjectToModalData(
  subject: CalendarSubject
): BangumiModalData {
  const parsedChinese = parseBgmtvName(
    subject.title_chinese || subject.title_japanese || ""
  );
  const parsedJapanese = parseBgmtvName(subject.title_japanese || "");
  const season = parsedChinese.season ?? parsedJapanese.season ?? 1;

  return {
    bgmtvId: subject.bgmtv_id ?? 0,
    titleChinese: parsedChinese.name,
    titleJapanese: subject.title_chinese ? parsedJapanese.name : null,
    posterUrl: subject.poster_url,
    year: subject.air_date
      ? parseInt(subject.air_date.split("-")[0], 10) || null
      : null,
    season,
    totalEpisodes: subject.total_episodes,
    platform: subject.platform,
    airDate: subject.air_date,
    airWeek: subject.air_week,
    mikanId: subject.mikan_id,
  };
}
