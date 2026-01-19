import { useInfiniteQuery } from "@tanstack/react-query";
import {
  getLogsInfiniteOptions,
  getLogsInfiniteQueryKey,
  type LogsResponse,
} from "@/lib/api";
import type { LogLevel } from "./use-log-filters";

export interface UseLogsParams {
  date: string;
  level?: LogLevel;
  pageSize?: number;
}

export function useLogs(params: UseLogsParams) {
  const { date, level, pageSize = 100 } = params;

  return useInfiniteQuery({
    ...getLogsInfiniteOptions({
      query: {
        date,
        level,
        pageSize,
      },
    }),
    initialPageParam: 1,
    getNextPageParam: (lastPage: LogsResponse) => {
      const currentPage = lastPage.page;
      const totalPages = Math.ceil(lastPage.total / lastPage.pageSize);
      return currentPage < totalPages ? currentPage + 1 : undefined;
    },
    queryKey: getLogsInfiniteQueryKey({
      query: { date, level, pageSize },
    }),
  });
}
