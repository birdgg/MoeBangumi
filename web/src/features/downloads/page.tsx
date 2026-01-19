import {
  IconLoader2,
  IconAlertCircle,
  IconDownload,
  IconCloudOff,
} from "@tabler/icons-react";
import { Button } from "@/components/ui/button";
import { DownloadsTable } from "./components";
import { useListTorrents, useDeleteTorrents } from "./hooks/use-downloads";

export function DownloadsPage() {
  const { data: tasks, isLoading, error, refetch } = useListTorrents();
  const deleteMutation = useDeleteTorrents();

  const isEmpty = !tasks || tasks.length === 0;

  // Check if error is "downloader not configured" type
  const isNotConfigured =
    error?.message?.includes("not configured") ||
    error?.message?.includes("NotConfigured");

  return (
    <div className="min-h-full bg-linear-to-br from-chart-1/5 via-background to-chart-3/5 dark:from-zinc-950 dark:via-background dark:to-chart-3/10">
      {/* Decorative background elements */}
      <div className="pointer-events-none fixed inset-0 overflow-hidden">
        <div className="absolute -left-40 -top-40 size-80 rounded-full bg-chart-1/20 blur-3xl dark:bg-chart-1/10" />
        <div className="absolute -right-40 top-1/3 size-96 rounded-full bg-chart-3/20 blur-3xl dark:bg-chart-3/10" />
        <div className="absolute -bottom-40 left-1/3 size-80 rounded-full bg-chart-5/20 blur-3xl dark:bg-chart-5/10" />
      </div>

      {/* Content */}
      <div className="relative px-6 py-6 md:px-8">
        {/* Header */}
        <div className="mb-6 flex items-center justify-between">
          <h1 className="text-lg font-medium text-foreground">下载管理</h1>

          <Button
            variant="ghost"
            size="sm"
            onClick={() => refetch()}
            disabled={isLoading}
          >
            {isLoading ? (
              <IconLoader2 className="size-4 animate-spin" />
            ) : (
              <span>刷新</span>
            )}
          </Button>
        </div>

        {/* Loading state */}
        {isLoading && (
          <div className="flex items-center justify-center py-16">
            <IconLoader2 className="size-6 animate-spin text-muted-foreground" />
          </div>
        )}

        {/* Not configured error state */}
        {!isLoading && isNotConfigured && (
          <div className="py-16">
            <div className="flex flex-col items-center justify-center text-center">
              <div className="mb-4 flex size-20 items-center justify-center rounded-full bg-muted/50">
                <IconCloudOff className="size-10 text-muted-foreground" />
              </div>
              <h3 className="mb-2 text-lg font-semibold text-foreground">
                下载器未配置
              </h3>
              <p className="mb-6 max-w-sm text-sm text-muted-foreground">
                请先在设置中配置下载器连接信息
              </p>
              <Button
                variant="outline"
                onClick={() => (window.location.href = "/settings")}
              >
                前往设置
              </Button>
            </div>
          </div>
        )}

        {/* Generic error state */}
        {!isLoading && error && !isNotConfigured && (
          <div className="py-16">
            <div className="flex flex-col items-center justify-center text-center">
              <div className="mb-4 flex size-20 items-center justify-center rounded-full bg-destructive/10">
                <IconAlertCircle className="size-10 text-destructive" />
              </div>
              <h3 className="mb-2 text-lg font-semibold text-foreground">
                加载失败
              </h3>
              <p className="mb-6 max-w-sm text-sm text-muted-foreground">
                无法获取下载任务列表，请稍后重试
              </p>
              <Button variant="outline" onClick={() => refetch()}>
                重试
              </Button>
            </div>
          </div>
        )}

        {/* Empty state */}
        {!isLoading && !error && isEmpty && (
          <div className="py-16">
            <div className="flex flex-col items-center justify-center text-center">
              <div className="mb-4 flex size-20 items-center justify-center rounded-full bg-linear-to-br from-chart-1/20 to-chart-3/20 dark:from-chart-1/30 dark:to-chart-3/30">
                <IconDownload className="size-10 text-chart-1 dark:text-chart-3" />
              </div>
              <h3 className="mb-2 text-lg font-semibold text-foreground">
                暂无下载任务
              </h3>
              <p className="mb-6 max-w-sm text-sm text-muted-foreground">
                当前没有正在进行的下载任务
              </p>
            </div>
          </div>
        )}

        {/* Downloads table */}
        {!isLoading && !error && !isEmpty && tasks && (
          <DownloadsTable
            tasks={tasks}
            onDelete={(hashes) =>
              deleteMutation.mutate({ body: { hashes, delete_files: true } })
            }
            isDeleting={deleteMutation.isPending}
          />
        )}
      </div>
    </div>
  );
}
