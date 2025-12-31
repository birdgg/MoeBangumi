import { cn } from "@/lib/utils";

interface Log {
  id: number;
  created_at: string;
  level: "info" | "warning" | "error";
  message: string;
}

interface LogItemProps {
  log: Log;
}

const levelStyles = {
  error: "text-red-400",
  warning: "text-amber-400",
  info: "text-zinc-500",
};

export function LogItem({ log }: LogItemProps) {
  const time = new Date(log.created_at).toLocaleTimeString("zh-CN", {
    hour: "2-digit",
    minute: "2-digit",
    second: "2-digit",
  });

  const date = new Date(log.created_at).toLocaleDateString("zh-CN", {
    month: "2-digit",
    day: "2-digit",
  });

  return (
    <div className="group font-mono text-sm py-1.5">
      <div className="flex items-start gap-2">
        {/* Timestamp */}
        <span className="shrink-0 text-muted-foreground/60">
          {date} {time}
        </span>

        {/* Level */}
        <span
          className={cn("shrink-0 w-12 uppercase", levelStyles[log.level])}
        >
          {log.level === "warning" ? "warn" : log.level}
        </span>

        {/* Message */}
        <span className="text-foreground/90 break-all whitespace-pre-wrap">{log.message}</span>
      </div>
    </div>
  );
}
