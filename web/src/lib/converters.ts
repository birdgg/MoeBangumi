import type { BgmtvSearchItem, CalendarSubject, SubscribedBangumi } from "@/lib/api";
import type { BangumiModalData } from "@/features/bangumi/components";

/**
 * Convert BgmtvSearchItem to BangumiModalData for the add modal
 */
export function subjectToModalData(subject: BgmtvSearchItem): BangumiModalData {
  return {
    bgmtvId: subject.id,
    titleChinese: subject.titleChinese || subject.titleJapanese || "",
    titleJapanese: subject.titleJapanese || null,
    posterUrl: subject.posterUrl,
    year: subject.airDate ? parseInt(subject.airDate.split("-")[0], 10) || null : null,
    season: subject.season ?? undefined,
    totalEpisodes: subject.totalEpisodes,
    platform: null,
    airDate: subject.airDate,
    airWeek: subject.airDate ? new Date(subject.airDate).getDay() : null,
  };
}

/**
 * Convert CalendarSubject to BangumiModalData for the add modal
 */
export function calendarSubjectToModalData(
  subject: CalendarSubject
): BangumiModalData {
  return {
    bgmtvId: subject.bgmtvId ?? 0,
    tmdbId: subject.tmdbId,
    titleChinese: subject.titleChinese,
    titleJapanese: subject.titleJapanese,
    posterUrl: subject.posterUrl,
    year: subject.airDate
      ? parseInt(subject.airDate.split("-")[0], 10) || null
      : null,
    season: subject.season,
    totalEpisodes: subject.totalEpisodes,
    platform: subject.platform,
    airDate: subject.airDate,
    airWeek: subject.airWeek,
    mikanId: subject.mikanId,
  };
}

/**
 * Convert SubscribedBangumi to BangumiModalData for the edit modal
 */
export function subscribedBangumiToModalData(
  subscribedBangumi: SubscribedBangumi
): BangumiModalData {
  const { bangumi, subscription, rssEntries } = subscribedBangumi;
  return {
    id: bangumi.id,
    subscriptionId: subscription.id,
    bgmtvId: bangumi.bgmtvId ?? 0,
    tmdbId: bangumi.tmdbId,
    titleChinese: bangumi.titleChinese,
    titleJapanese: bangumi.titleJapanese,
    posterUrl: bangumi.posterUrl,
    year: bangumi.airDate ? parseInt(bangumi.airDate.split("-")[0], 10) : null,
    season: bangumi.season,
    totalEpisodes: bangumi.totalEpisodes,
    platform: bangumi.platform,
    airDate: bangumi.airDate,
    airWeek: bangumi.airWeek,
    mikanId: bangumi.mikanId,
    episodeOffset: subscription.episodeOffset,
    autoComplete: subscription.autoComplete,
    sourceType: subscription.sourceType,
    rssEntries: rssEntries,
  };
}
