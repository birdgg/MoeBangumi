import { useState } from "react";
import { IconChevronRight } from "@tabler/icons-react";
import { cn } from "@/lib/utils";

interface Event {
  id: number;
  created_at: string;
  level: "info" | "warning" | "error";
  message: string;
  details?: string;
}

interface EventItemProps {
  event: Event;
}

const levelStyles = {
  error: "text-red-400",
  warning: "text-amber-400",
  info: "text-zinc-500",
};

export function EventItem({ event }: EventItemProps) {
  const [expanded, setExpanded] = useState(false);
  const hasDetails = !!event.details;

  const time = new Date(event.created_at).toLocaleTimeString("zh-CN", {
    hour: "2-digit",
    minute: "2-digit",
    second: "2-digit",
  });

  const date = new Date(event.created_at).toLocaleDateString("zh-CN", {
    month: "2-digit",
    day: "2-digit",
  });

  return (
    <div className="group font-mono text-sm">
      <div
        className={cn(
          "flex items-start gap-2 py-1.5 px-2 -mx-2 rounded",
          hasDetails && "cursor-pointer hover:bg-muted/50"
        )}
        onClick={() => hasDetails && setExpanded(!expanded)}
      >
        {/* Expand indicator */}
        <span className="w-4 shrink-0 text-muted-foreground/50">
          {hasDetails && (
            <IconChevronRight
              className={cn(
                "size-4 transition-transform",
                expanded && "rotate-90"
              )}
            />
          )}
        </span>

        {/* Timestamp */}
        <span className="shrink-0 text-muted-foreground/60">
          {date} {time}
        </span>

        {/* Level */}
        <span
          className={cn("shrink-0 w-12 uppercase", levelStyles[event.level])}
        >
          {event.level === "warning" ? "warn" : event.level}
        </span>

        {/* Message */}
        <span className="text-foreground/90 break-all">{event.message}</span>
      </div>

      {/* Details */}
      {expanded && event.details && (
        <pre className="ml-6 pl-4 border-l border-muted text-xs text-muted-foreground overflow-x-auto py-2">
          {(() => {
            try {
              return JSON.stringify(JSON.parse(event.details), null, 2);
            } catch {
              return event.details;
            }
          })()}
        </pre>
      )}
    </div>
  );
}
