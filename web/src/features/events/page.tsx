import { useMemo, useState } from "react";
import { IconLoader2 } from "@tabler/icons-react";
import { cn } from "@/lib/utils";
import { EventList } from "./components";
import { useEvents } from "./hooks/use-events";

const filters = [
  { key: undefined, label: "全部" },
  { key: "error", label: "错误" },
  { key: "warning", label: "警告" },
  { key: "info", label: "信息" },
] as const;

export function EventsPage() {
  const [levelFilter, setLevelFilter] = useState<string | undefined>();
  const {
    data,
    isLoading,
    error,
    fetchNextPage,
    hasNextPage,
    isFetchingNextPage,
  } = useEvents({ level: levelFilter });

  // Flatten pages into single array
  const events = useMemo(
    () => data?.pages.flatMap((page) => page) ?? [],
    [data]
  );

  return (
    <div className="min-h-full bg-background">
      <div className="px-6 py-6 md:px-8">
        {/* Header */}
        <div className="mb-4 flex items-center justify-between">
          <h1 className="text-lg font-medium text-foreground">日志</h1>

          {/* Filter tabs */}
          <div className="flex gap-1 text-sm">
            {filters.map((f) => (
              <button
                key={f.key ?? "all"}
                onClick={() => setLevelFilter(f.key)}
                className={cn(
                  "px-3 py-1 rounded-md transition-colors",
                  levelFilter === f.key
                    ? "bg-muted text-foreground"
                    : "text-muted-foreground hover:text-foreground"
                )}
              >
                {f.label}
              </button>
            ))}
          </div>
        </div>

        {/* Loading state */}
        {isLoading && (
          <div className="flex items-center justify-center py-12">
            <IconLoader2 className="size-5 animate-spin text-muted-foreground" />
          </div>
        )}

        {/* Error state */}
        {error && (
          <div className="py-12 text-center text-sm text-muted-foreground">
            加载失败
          </div>
        )}

        {/* Event list */}
        {!isLoading && !error && <EventList events={events} />}

        {/* Load more button */}
        {hasNextPage && (
          <div className="mt-4 flex justify-center">
            <button
              onClick={() => fetchNextPage()}
              disabled={isFetchingNextPage}
              className="px-4 py-2 text-sm text-muted-foreground hover:text-foreground transition-colors disabled:opacity-50"
            >
              {isFetchingNextPage ? (
                <span className="flex items-center gap-2">
                  <IconLoader2 className="size-4 animate-spin" />
                  加载中...
                </span>
              ) : (
                "加载更多"
              )}
            </button>
          </div>
        )}
      </div>
    </div>
  );
}
