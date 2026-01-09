import { Button } from "@/components/ui/button";
import { cn } from "@/lib/utils";
import {
  IconRefresh,
  IconExternalLink,
  IconSparkles,
  IconInfoCircle,
  IconCircleCheck,
  IconDownload,
} from "@tabler/icons-react";
import { useQuery, useMutation, useQueryClient } from "@tanstack/react-query";
import {
  getVersionOptions,
  getVersionQueryKey,
  checkUpdateMutation,
  performUpdateMutation,
} from "@/lib/api";
import { formatDistanceToNow } from "date-fns";
import { zhCN } from "date-fns/locale";
import { toast } from "sonner";

export function SystemSection() {
  const queryClient = useQueryClient();
  const { data: versionInfo, isLoading } = useQuery(getVersionOptions());

  const { mutate: checkUpdate, isPending: isChecking } = useMutation({
    ...checkUpdateMutation(),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: getVersionQueryKey() });
    },
  });

  const { mutate: performUpdate, isPending: isUpdating } = useMutation({
    ...performUpdateMutation(),
    onSuccess: (data) => {
      if (data.success) {
        toast.success("更新已开始", {
          description: "应用将在更新完成后自动重启",
        });
      } else {
        toast.error("更新失败", { description: data.message });
      }
      queryClient.invalidateQueries({ queryKey: getVersionQueryKey() });
    },
    onError: (error) => {
      const message = error instanceof Error ? error.message : "更新失败";
      toast.error("更新失败", { description: message });
    },
  });

  const handleCheckUpdate = () => {
    checkUpdate({});
  };

  const handlePerformUpdate = () => {
    performUpdate({});
  };

  if (isLoading) {
    return (
      <section className="space-y-5">
        <div className="h-48 animate-pulse rounded-lg bg-muted/50" />
      </section>
    );
  }

  const isUpToDate =
    versionInfo?.latest &&
    !versionInfo.update_available &&
    versionInfo.status === "idle";

  return (
    <section className="space-y-5">
      {/* Version Info Card */}
      <div className="rounded-lg border border-border/50 bg-muted/20 p-4">
        <div className="flex items-center gap-2 pb-3">
          <IconInfoCircle className="size-4 text-chart-1" />
          <span className="text-sm font-medium">版本信息</span>
        </div>

        <div className="space-y-3">
          {/* Current Version */}
          <div className="flex items-center justify-between">
            <span className="text-sm text-muted-foreground">当前版本</span>
            <span className="font-mono text-sm">{versionInfo?.current}</span>
          </div>

          {/* Latest Version */}
          {versionInfo?.latest && (
            <div className="flex items-center justify-between">
              <span className="text-sm text-muted-foreground">最新版本</span>
              <span className="font-mono text-sm">{versionInfo.latest}</span>
            </div>
          )}

          {/* Up to Date Status */}
          {isUpToDate && (
            <div className="flex items-center gap-2 rounded-md bg-green-500/10 p-2">
              <IconCircleCheck className="size-4 text-green-600" />
              <span className="text-sm text-green-600">已是最新版本</span>
            </div>
          )}

          {/* Update Available */}
          {versionInfo?.update_available && (
            <div className="flex items-center gap-2 rounded-md bg-chart-1/10 p-2">
              <IconSparkles className="size-4 text-chart-1" />
              <span className="text-sm text-chart-1">有新版本可用!</span>
              {versionInfo.release_url && (
                <a
                  href={versionInfo.release_url}
                  target="_blank"
                  rel="noopener noreferrer"
                  className="ml-auto flex items-center gap-1 text-xs text-chart-1 hover:underline"
                >
                  查看发布页 <IconExternalLink className="size-3" />
                </a>
              )}
            </div>
          )}

          {/* Changelog */}
          {versionInfo?.changelog && versionInfo.update_available && (
            <div className="rounded-md border bg-card p-3">
              <h4 className="mb-2 text-xs font-medium text-muted-foreground">
                更新日志
              </h4>
              <div className="max-h-48 overflow-y-auto text-xs">
                <pre className="whitespace-pre-wrap font-sans text-foreground/80">
                  {versionInfo.changelog}
                </pre>
              </div>
            </div>
          )}

          {/* Last Checked Time */}
          {versionInfo?.last_checked && (
            <div className="text-xs text-muted-foreground">
              上次检查:{" "}
              {formatDistanceToNow(new Date(versionInfo.last_checked), {
                addSuffix: true,
                locale: zhCN,
              })}
            </div>
          )}
        </div>

        {/* Action Buttons */}
        <div className="mt-4 flex gap-2 border-t border-border/50 pt-3">
          <Button
            type="button"
            variant="outline"
            size="sm"
            onClick={handleCheckUpdate}
            disabled={isChecking || isUpdating || versionInfo?.status === "checking"}
            className="gap-2"
          >
            <IconRefresh
              className={cn("size-4", isChecking && "animate-spin")}
            />
            {isChecking ? "检测中..." : "检测更新"}
          </Button>

          {versionInfo?.update_available && (
            <Button
              type="button"
              size="sm"
              onClick={handlePerformUpdate}
              disabled={isUpdating || versionInfo?.status === "downloading"}
              className="gap-2 bg-chart-1 text-white hover:bg-chart-1/90"
            >
              <IconDownload
                className={cn("size-4", isUpdating && "animate-bounce")}
              />
              {isUpdating ? "更新中..." : "立即更新"}
            </Button>
          )}
        </div>
      </div>

      {/* Info note */}
      <p className="text-xs text-muted-foreground">
        点击检测更新按钮可检查是否有新版本。
      </p>
    </section>
  );
}
