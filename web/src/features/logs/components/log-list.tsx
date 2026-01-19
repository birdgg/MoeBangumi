import { useEffect, useRef } from "react";
import { LogItem } from "./log-item";
import type { LogEntry } from "@/lib/api";

export interface LogListProps {
  entries: LogEntry[];
  hasMore: boolean;
  isFetchingMore: boolean;
  onLoadMore: () => void;
}

export function LogList({
  entries,
  hasMore,
  isFetchingMore,
  onLoadMore,
}: LogListProps) {
  const sentinelRef = useRef<HTMLDivElement>(null);

  useEffect(() => {
    if (!hasMore || isFetchingMore) return;

    const sentinel = sentinelRef.current;
    if (!sentinel) return;

    const observer = new IntersectionObserver(
      (entries) => {
        if (entries[0].isIntersecting) {
          onLoadMore();
        }
      },
      { rootMargin: "200px" }
    );

    observer.observe(sentinel);
    return () => observer.disconnect();
  }, [hasMore, isFetchingMore, onLoadMore]);

  if (entries.length === 0) {
    return (
      <div className="py-16 text-center text-sm text-muted-foreground">
        暂无日志
      </div>
    );
  }

  return (
    <div className="space-y-1">
      {entries.map((entry, index) => (
        <LogItem key={`${entry.timestamp}-${index}`} entry={entry} />
      ))}

      {hasMore && <div ref={sentinelRef} className="h-px" />}

      {isFetchingMore && (
        <div className="py-4 text-center text-sm text-muted-foreground">
          加载中...
        </div>
      )}
    </div>
  );
}
