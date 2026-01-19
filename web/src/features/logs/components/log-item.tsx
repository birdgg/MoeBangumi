import {
  IconAlertCircle,
  IconAlertTriangle,
  IconInfoCircle,
} from "@tabler/icons-react";
import { cn } from "@/lib/utils";
import type { LogEntry } from "@/lib/api";

const LEVEL_CONFIG = {
  error: {
    icon: IconAlertCircle,
    bgClass: "bg-destructive/10 dark:bg-destructive/20",
    textClass: "text-destructive",
    label: "错误",
  },
  warning: {
    icon: IconAlertTriangle,
    bgClass: "bg-yellow-500/10 dark:bg-yellow-500/20",
    textClass: "text-yellow-600 dark:text-yellow-500",
    label: "警告",
  },
  info: {
    icon: IconInfoCircle,
    bgClass: "bg-blue-500/10 dark:bg-blue-500/20",
    textClass: "text-blue-600 dark:text-blue-500",
    label: "信息",
  },
} as const;

export interface LogItemProps {
  entry: LogEntry;
}

export function LogItem({ entry }: LogItemProps) {
  const level = (entry.level as "error" | "warning" | "info") ?? "info";
  const timestamp = entry.time as string;
  const message = entry.message as string;
  const data = entry.data as Record<string, unknown> | null | undefined;

  const config = LEVEL_CONFIG[level];
  const Icon = config.icon;

  const timeStr = timestamp
    ? new Date(timestamp).toLocaleTimeString("zh-CN")
    : "";

  const hasData = data && Object.keys(data).length > 0;

  return (
    <div
      className={cn(
        "group flex items-start gap-3 rounded-lg border border-transparent p-3",
        "transition-colors hover:border-border hover:bg-muted/30"
      )}
    >
      <div className={cn("mt-0.5 rounded-md p-1.5", config.bgClass)}>
        <Icon className={cn("size-4", config.textClass)} />
      </div>

      <div className="flex-1 space-y-1">
        <div className="flex items-center gap-2 text-xs text-muted-foreground">
          <span className="font-mono">{timeStr}</span>
          <span className={cn("font-medium", config.textClass)}>
            {config.label}
          </span>
        </div>

        <p className="text-sm leading-relaxed text-foreground">{message}</p>

        {hasData && (
          <pre className="mt-2 overflow-x-auto rounded bg-muted/50 p-2 text-xs text-muted-foreground">
            {JSON.stringify(data, null, 2)}
          </pre>
        )}
      </div>
    </div>
  );
}
