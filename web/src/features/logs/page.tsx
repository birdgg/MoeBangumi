import { useMemo } from "react";
import { IconAlertCircle } from "@tabler/icons-react";
import { Button } from "@/components/ui/button";
import { LogFilters, LogList, LogSkeleton } from "./components";
import { useLogs, useLogFilters, useLogStream, getUtcDateString } from "./hooks";

export function LogsPage() {
  const { date, level, setDate, setLevel } = useLogFilters();

  const {
    data,
    isLoading,
    error,
    fetchNextPage,
    hasNextPage,
    isFetchingNextPage,
    refetch,
  } = useLogs({ date, level });

  // Check if viewing today's logs (UTC)
  const isToday = date === getUtcDateString();

  // SSE real-time stream (only for today's logs, always enabled)
  const { entries: streamEntries } = useLogStream({
    enabled: isToday,
    levelFilter: level,
  });

  // Merge historical and real-time logs
  const entries = useMemo(() => {
    const historicalEntries = data?.pages.flatMap((page) => page.entries) ?? [];

    // If viewing today, prepend stream entries
    if (isToday) {
      return [...streamEntries, ...historicalEntries];
    }

    return historicalEntries;
  }, [data, streamEntries, isToday]);

  return (
    <div className="min-h-full bg-background">
      <div className="px-6 py-6 md:px-8">
        <div className="mb-6 flex items-center justify-between">
          <h1 className="text-lg font-medium text-foreground">日志</h1>
          <LogFilters
            date={date}
            level={level}
            onDateChange={setDate}
            onLevelChange={setLevel}
            disabled={isLoading}
          />
        </div>

        {isLoading && (
          <div className="space-y-2">
            {Array.from({ length: 5 }).map((_, i) => (
              <LogSkeleton key={i} />
            ))}
          </div>
        )}

        {error && (
          <div className="py-16">
            <div className="flex flex-col items-center justify-center text-center">
              <div className="mb-4 flex size-20 items-center justify-center rounded-full bg-destructive/10">
                <IconAlertCircle className="size-10 text-destructive" />
              </div>
              <h3 className="mb-2 text-lg font-semibold text-foreground">
                加载失败
              </h3>
              <p className="mb-6 max-w-sm text-sm text-muted-foreground">
                无法获取日志数据，请稍后重试
              </p>
              <Button variant="outline" onClick={() => refetch()}>
                重试
              </Button>
            </div>
          </div>
        )}

        {!isLoading && !error && (
          <LogList
            entries={entries}
            hasMore={!!hasNextPage}
            isFetchingMore={isFetchingNextPage}
            onLoadMore={fetchNextPage}
          />
        )}
      </div>
    </div>
  );
}
