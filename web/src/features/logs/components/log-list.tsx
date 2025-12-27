import { useEffect, useRef, useState } from "react";
import { LogItem } from "./log-item";

interface Log {
  id: number;
  created_at: string;
  level: "info" | "warning" | "error";
  message: string;
}

interface LogListProps {
  logs: Log[];
}

const ITEMS_PER_PAGE = 20;

export function LogList({ logs }: LogListProps) {
  const [visibleCount, setVisibleCount] = useState(ITEMS_PER_PAGE);
  const sentinelRef = useRef<HTMLDivElement>(null);

  // Reset visible count when logs change (e.g., filter change)
  useEffect(() => {
    setVisibleCount(ITEMS_PER_PAGE);
  }, [logs.length]);

  // Intersection observer to load more when scrolling to bottom
  useEffect(() => {
    const sentinel = sentinelRef.current;
    if (!sentinel) return;

    const observer = new IntersectionObserver(
      (entries) => {
        if (entries[0].isIntersecting) {
          setVisibleCount((prev) => Math.min(prev + ITEMS_PER_PAGE, logs.length));
        }
      },
      { rootMargin: "100px" }
    );

    observer.observe(sentinel);
    return () => observer.disconnect();
  }, [logs.length]);

  if (logs.length === 0) {
    return (
      <div className="py-12 text-center text-muted-foreground font-mono text-sm">
        暂无日志记录
      </div>
    );
  }

  const visibleLogs = logs.slice(0, visibleCount);
  const hasMore = visibleCount < logs.length;

  return (
    <div className="divide-y divide-muted/30">
      {visibleLogs.map((log) => (
        <LogItem key={log.id} log={log} />
      ))}
      {hasMore && (
        <div ref={sentinelRef} className="py-2 text-center text-xs text-muted-foreground">
          加载更多...
        </div>
      )}
    </div>
  );
}
