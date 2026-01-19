import { useQuery, useMutation, useQueryClient } from "@tanstack/react-query";
import {
  getCalendarOptions,
  getCalendarQueryKey,
  refreshCalendarMutation,
  postApiCalendarQuickSubscribeMutation,
  getApiBangumiSubscriptionsQueryKey,
} from "@/lib/api";
import { toast } from "sonner";
import type { Season } from "./use-season-selection";
import type { CalendarSubject } from "@/lib/api";

export interface CalendarParams {
  year?: number;
  season?: Season;
}

export function useCalendar(params?: CalendarParams) {
  return useQuery({
    ...getCalendarOptions({
      query: {
        year: params?.year,
        season: params?.season,
      },
    }),
  });
}

export function useRefreshCalendar(params?: CalendarParams) {
  const queryClient = useQueryClient();
  return useMutation({
    ...refreshCalendarMutation({
      query: {
        year: params?.year,
        season: params?.season,
      },
    }),
    onSuccess: (data) => {
      // Update the calendar query cache with the new data
      queryClient.setQueryData(
        getCalendarQueryKey({
          query: {
            year: params?.year,
            season: params?.season,
          },
        }),
        data
      );
      toast.success("日历数据已刷新");
    },
    onError: () => {
      toast.error("刷新失败，请稍后重试");
    },
  });
}

export function useQuickSubscribe(params?: CalendarParams) {
  const queryClient = useQueryClient();
  return useMutation({
    ...postApiCalendarQuickSubscribeMutation(),
    onSuccess: (data) => {
      // Invalidate calendar and subscriptions queries
      queryClient.invalidateQueries({
        queryKey: getCalendarQueryKey({
          query: {
            year: params?.year,
            season: params?.season,
          },
        }),
      });
      queryClient.invalidateQueries({
        queryKey: getApiBangumiSubscriptionsQueryKey(),
      });
      toast.success("订阅成功", {
        description: `「${data.bangumi.titleChinese}」已添加到追番列表`,
      });
    },
    onError: (error) => {
      const message = error instanceof Error ? error.message : "未知错误";
      if (message.includes("409")) {
        toast.error("订阅失败", { description: "该番剧已订阅" });
      } else {
        toast.error("订阅失败", { description: message });
      }
    },
  });
}

// Helper to convert CalendarSubject to QuickSubscribeDto
export function calendarSubjectToQuickSubscribeDto(subject: CalendarSubject) {
  return {
    mikanId: subject.mikanId!,
    bgmtvId: subject.bgmtvId,
    titleChinese: subject.titleChinese,
    titleJapanese: subject.titleJapanese,
    posterUrl: subject.posterUrl,
    airDate: subject.airDate,
    airWeek: subject.airWeek,
    totalEpisodes: subject.totalEpisodes,
    season: subject.season,
    platform: subject.platform,
  };
}
