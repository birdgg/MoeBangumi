import { useState } from "react";
import {
  IconPlayerPause,
  IconPlayerPlay,
  IconTrash,
  IconLoader2,
} from "@tabler/icons-react";
import { cn } from "@/lib/utils";
import { Button } from "@/components/ui/button";
import {
  AlertDialog,
  AlertDialogAction,
  AlertDialogCancel,
  AlertDialogContent,
  AlertDialogDescription,
  AlertDialogFooter,
  AlertDialogHeader,
  AlertDialogTitle,
} from "@/components/ui/alert-dialog";
import { Tooltip, TooltipContent, TooltipTrigger } from "@/components/ui/tooltip";
import type { ExtendedTorrentInfo } from "../hooks/use-downloads";

interface DownloadsTableProps {
  torrents: ExtendedTorrentInfo[];
  onPause: (hashes: string[]) => void;
  onResume: (hashes: string[]) => void;
  onDelete: (hashes: string[]) => void;
  isPausing?: boolean;
  isResuming?: boolean;
  isDeleting?: boolean;
}

// Format bytes to human readable
function formatBytes(bytes: number): string {
  if (bytes === 0) return "0 B";
  const k = 1024;
  const sizes = ["B", "KB", "MB", "GB", "TB"];
  const i = Math.floor(Math.log(bytes) / Math.log(k));
  return `${parseFloat((bytes / Math.pow(k, i)).toFixed(1))} ${sizes[i]}`;
}

// Format ETA
function formatEta(seconds: number): string {
  if (seconds < 0 || seconds === 8640000) return "∞";
  if (seconds === 0) return "-";

  const hours = Math.floor(seconds / 3600);
  const minutes = Math.floor((seconds % 3600) / 60);
  const secs = seconds % 60;

  if (hours > 0) return `${hours}h ${minutes}m`;
  if (minutes > 0) return `${minutes}m ${secs}s`;
  return `${secs}s`;
}

// Get status display info
function getStatusInfo(state: string): { label: string; color: string } {
  const stateMap: Record<string, { label: string; color: string }> = {
    downloading: { label: "下载中", color: "text-chart-1" },
    uploading: { label: "做种", color: "text-chart-3" },
    pausedDL: { label: "已暂停", color: "text-muted-foreground" },
    pausedUP: { label: "已暂停", color: "text-muted-foreground" },
    stoppedDL: { label: "已停止", color: "text-muted-foreground" },
    stoppedUP: { label: "已停止", color: "text-muted-foreground" },
    stalledDL: { label: "等待中", color: "text-chart-5" },
    stalledUP: { label: "做种", color: "text-chart-3" },
    checkingDL: { label: "检查中", color: "text-chart-2" },
    checkingUP: { label: "检查中", color: "text-chart-2" },
    checkingResumeData: { label: "检查中", color: "text-chart-2" },
    queuedDL: { label: "排队中", color: "text-muted-foreground" },
    queuedUP: { label: "排队中", color: "text-muted-foreground" },
    forcedDL: { label: "强制下载", color: "text-chart-1" },
    forcedUP: { label: "强制上传", color: "text-chart-3" },
    missingFiles: { label: "文件丢失", color: "text-destructive" },
    error: { label: "错误", color: "text-destructive" },
    allocating: { label: "分配空间", color: "text-chart-2" },
    metaDL: { label: "获取元数据", color: "text-chart-2" },
    moving: { label: "移动中", color: "text-chart-2" },
  };

  return stateMap[state] || { label: state, color: "text-muted-foreground" };
}

// Check if torrent is paused or stopped
function isPaused(state: string): boolean {
  return state === "pausedDL" || state === "pausedUP" || state === "stoppedDL" || state === "stoppedUP";
}

export function DownloadsTable({
  torrents,
  onPause,
  onResume,
  onDelete,
  isPausing,
  isResuming,
  isDeleting,
}: DownloadsTableProps) {
  const [deleteTarget, setDeleteTarget] = useState<string | null>(null);

  const handleDelete = () => {
    if (deleteTarget) {
      onDelete([deleteTarget]);
      setDeleteTarget(null);
    }
  };

  return (
    <>
      <div className="overflow-x-auto rounded-xl border border-border/50 bg-card/50 backdrop-blur-sm">
        <table className="w-full text-sm">
          <thead>
            <tr className="border-b border-border/50 text-left text-muted-foreground">
              <th className="px-4 py-3 font-medium w-96">名称</th>
              <th className="px-4 py-3 font-medium w-24">状态</th>
              <th className="px-4 py-3 font-medium w-40">进度</th>
              <th className="px-4 py-3 font-medium w-32 text-right">大小</th>
              <th className="px-4 py-3 font-medium w-24 text-right">剩余时间</th>
              <th className="px-4 py-3 font-medium w-28 text-right">操作</th>
            </tr>
          </thead>
          <tbody>
            {torrents.map((torrent) => {
              const status = getStatusInfo(torrent.state);
              const paused = isPaused(torrent.state);
              const progress = Math.round(torrent.progress * 100);

              return (
                <tr
                  key={torrent.hash}
                  className="border-b border-border/30 last:border-0 hover:bg-muted/30 transition-colors"
                >
                  {/* Name */}
                  <td className="px-4 py-3">
                    <Tooltip>
                      <TooltipTrigger
                        render={<span className="block truncate text-foreground cursor-default" />}
                      >
                        {torrent.name}
                      </TooltipTrigger>
                      <TooltipContent side="top" className="max-w-md">
                        <p className="break-all">{torrent.name}</p>
                      </TooltipContent>
                    </Tooltip>
                  </td>

                  {/* Status */}
                  <td className="px-4 py-3">
                    <span className={cn("font-medium", status.color)}>
                      {status.label}
                    </span>
                  </td>

                  {/* Progress */}
                  <td className="px-4 py-3">
                    <div className="flex items-center gap-2">
                      <div className="h-2 flex-1 rounded-full bg-muted/50 overflow-hidden">
                        <div
                          className={cn(
                            "h-full rounded-full transition-all",
                            progress === 100
                              ? "bg-chart-3"
                              : "bg-linear-to-r from-chart-1 to-chart-3"
                          )}
                          style={{ width: `${progress}%` }}
                        />
                      </div>
                      <span className="text-xs text-muted-foreground w-10 text-right">
                        {progress}%
                      </span>
                    </div>
                  </td>

                  {/* Size */}
                  <td className="px-4 py-3 text-right text-muted-foreground">
                    {formatBytes(torrent.size)}
                  </td>

                  {/* ETA */}
                  <td className="px-4 py-3 text-right text-muted-foreground">
                    {formatEta(torrent.eta)}
                  </td>

                  {/* Actions */}
                  <td className="px-4 py-3">
                    <div className="flex items-center justify-end gap-1">
                      {paused ? (
                        <Tooltip>
                          <TooltipTrigger
                            render={
                              <Button
                                variant="ghost"
                                size="icon-sm"
                                onClick={() => onResume([torrent.hash])}
                                disabled={isResuming}
                              />
                            }
                          >
                            {isResuming ? (
                              <IconLoader2 className="size-4 animate-spin" />
                            ) : (
                              <IconPlayerPlay className="size-4" />
                            )}
                          </TooltipTrigger>
                          <TooltipContent>继续</TooltipContent>
                        </Tooltip>
                      ) : (
                        <Tooltip>
                          <TooltipTrigger
                            render={
                              <Button
                                variant="ghost"
                                size="icon-sm"
                                onClick={() => onPause([torrent.hash])}
                                disabled={isPausing}
                              />
                            }
                          >
                            {isPausing ? (
                              <IconLoader2 className="size-4 animate-spin" />
                            ) : (
                              <IconPlayerPause className="size-4" />
                            )}
                          </TooltipTrigger>
                          <TooltipContent>暂停</TooltipContent>
                        </Tooltip>
                      )}

                      <Tooltip>
                        <TooltipTrigger
                          render={
                            <Button
                              variant="ghost"
                              size="icon-sm"
                              onClick={() => setDeleteTarget(torrent.hash)}
                              disabled={isDeleting}
                              className="text-destructive hover:text-destructive hover:bg-destructive/10"
                            />
                          }
                        >
                          {isDeleting ? (
                            <IconLoader2 className="size-4 animate-spin" />
                          ) : (
                            <IconTrash className="size-4" />
                          )}
                        </TooltipTrigger>
                        <TooltipContent>删除</TooltipContent>
                      </Tooltip>
                    </div>
                  </td>
                </tr>
              );
            })}
          </tbody>
        </table>
      </div>

      {/* Delete confirmation dialog */}
      <AlertDialog open={!!deleteTarget} onOpenChange={() => setDeleteTarget(null)}>
        <AlertDialogContent>
          <AlertDialogHeader>
            <AlertDialogTitle>确认删除</AlertDialogTitle>
            <AlertDialogDescription>
              确定要删除此下载任务吗？这将同时删除已下载的文件。
            </AlertDialogDescription>
          </AlertDialogHeader>
          <AlertDialogFooter>
            <AlertDialogCancel>取消</AlertDialogCancel>
            <AlertDialogAction
              onClick={handleDelete}
              className="bg-destructive text-destructive-foreground hover:bg-destructive/90"
            >
              删除
            </AlertDialogAction>
          </AlertDialogFooter>
        </AlertDialogContent>
      </AlertDialog>
    </>
  );
}
