import { useMemo, useState } from "react";
import { IconLoader2, IconTrash } from "@tabler/icons-react";
import { cn } from "@/lib/utils";
import { useMutation, useQueryClient } from "@tanstack/react-query";
import { toast } from "sonner";
import { clearAllLogsMutation } from "@/lib/api";
import { LogList } from "./components";
import { useLogs } from "./hooks/use-logs";

const filters = [
  { key: undefined, label: "全部" },
  { key: "error", label: "错误" },
  { key: "warning", label: "警告" },
  { key: "info", label: "信息" },
] as const;

export function LogsPage() {
  const [levelFilter, setLevelFilter] = useState<string | undefined>();
  const queryClient = useQueryClient();
  const {
    data,
    isLoading,
    error,
    fetchNextPage,
    hasNextPage,
    isFetchingNextPage,
  } = useLogs({ level: levelFilter });

  const { mutate: clearLogs, isPending: isClearing } = useMutation({
    ...clearAllLogsMutation(),
    onSuccess: (deleted) => {
      queryClient.invalidateQueries({ queryKey: ["logs"] });
      toast.success(`已清除 ${deleted} 条日志`);
    },
    onError: () => {
      toast.error("清除日志失败");
    },
  });

  // Flatten pages into single array
  const logs = useMemo(
    () => data?.pages.flatMap((page) => page) ?? [],
    [data]
  );

  return (
    <div className="min-h-full bg-background">
      <div className="px-6 py-6 md:px-8">
        {/* Header */}
        <div className="mb-4 flex items-center justify-between">
          <h1 className="text-lg font-medium text-foreground">日志</h1>

          <div className="flex items-center gap-4">
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

            {/* Clear button */}
            <button
              onClick={() => clearLogs({})}
              disabled={isClearing || logs.length === 0}
              className="flex items-center gap-1.5 px-3 py-1 text-sm text-muted-foreground hover:text-destructive transition-colors disabled:opacity-50 disabled:cursor-not-allowed"
            >
              {isClearing ? (
                <IconLoader2 className="size-4 animate-spin" />
              ) : (
                <IconTrash className="size-4" />
              )}
              清除
            </button>
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

        {/* Log list */}
        {!isLoading && !error && <LogList logs={logs} />}

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
