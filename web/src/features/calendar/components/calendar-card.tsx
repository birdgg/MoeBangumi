import { cn } from "@/lib/utils";
import type { CalendarSubject } from "@/lib/api";
import { IconCheck, IconRss, IconLoader2 } from "@tabler/icons-react";
import { Button } from "@/components/ui/button";

interface CalendarCardProps {
  subject: CalendarSubject;
  className?: string;
  isSubscribed?: boolean;
  onClick?: () => void;
  onQuickSubscribe?: () => void;
  isSubscribing?: boolean;
}

export function CalendarCard({
  subject,
  className,
  isSubscribed,
  onClick,
  onQuickSubscribe,
  isSubscribing,
}: CalendarCardProps) {
  const displayName = subject.titleChinese || subject.titleJapanese || "";
  const canQuickSubscribe = !isSubscribed && !!subject.mikanId;

  return (
    <div
      className={cn(
        "group relative",
        onClick && "cursor-pointer",
        className
      )}
      onClick={onClick}
    >
      {/* Soft glow effect on hover */}
      <div
        className={cn(
          "absolute -inset-1 rounded-2xl opacity-0 blur-xl transition-all duration-500",
          "bg-linear-to-br from-chart-1/40 via-chart-3/30 to-chart-5/40",
          "group-hover:opacity-100"
        )}
      />

      {/* Card container with cute rounded corners */}
      <div
        className={cn(
          "relative overflow-hidden rounded-2xl",
          "bg-linear-to-br from-white to-chart-1/5 dark:from-zinc-900 dark:to-chart-1/10",
          "border-2 border-chart-1/20 dark:border-chart-1/15",
          "shadow-md shadow-chart-1/10 dark:shadow-chart-1/5",
          "transition-all duration-300 ease-out",
          "group-hover:shadow-lg group-hover:shadow-chart-1/20 dark:group-hover:shadow-chart-1/15",
          "group-hover:-translate-y-1 group-hover:scale-[1.02]",
          "group-hover:border-chart-1/40 dark:group-hover:border-chart-1/30"
        )}
      >
        {/* Poster section */}
        <div className="relative aspect-2/3 overflow-hidden">
          <img
            src={subject.posterUrl || ""}
            alt={displayName}
            className={cn(
              "size-full object-cover transition-all duration-500 ease-out",
              "group-hover:scale-110 group-hover:brightness-105"
            )}
          />

          {/* Gradient overlay */}
          <div className="absolute inset-0 bg-linear-to-t from-black/70 via-transparent to-chart-1/10" />

          {/* Decorative top wave */}
          <div className="absolute inset-x-0 top-0 h-8 bg-linear-to-b from-chart-1/20 to-transparent" />

          {/* Subscribed badge */}
          {isSubscribed && (
            <div className="absolute right-2 top-2">
              {/* Outer glow */}
              <div className="absolute -inset-1.5 rounded-full bg-chart-1/30 blur-md" />

              {/* Badge container */}
              <div className="relative">
                {/* Main badge */}
                <div className={cn(
                  "relative flex items-center gap-1 rounded-full px-2 py-0.5",
                  "bg-linear-to-br from-chart-1 via-chart-1 to-chart-3",
                  "text-[10px] font-bold tracking-wide text-white",
                  "shadow-lg shadow-chart-1/40",
                  "ring-1 ring-white/40 dark:ring-white/20",
                  "backdrop-blur-sm"
                )}>
                  {/* Check icon */}
                  <IconCheck className="size-2.5" strokeWidth={3} />

                  {/* Badge text */}
                  <span className="drop-shadow-sm">已订阅</span>
                </div>

                {/* Bottom highlight */}
                <div className="absolute inset-x-1.5 -bottom-0.5 h-0.5 rounded-full bg-chart-1/20 blur-sm" />
              </div>
            </div>
          )}

          {/* Quick subscribe button - shown on hover when mikanId exists and not subscribed */}
          {canQuickSubscribe && (
            <div className={cn(
              "absolute right-2 top-2",
              "opacity-0 group-hover:opacity-100 transition-opacity duration-200"
            )}>
              <Button
                size="sm"
                className={cn(
                  "h-7 gap-1.5 px-2",
                  "bg-orange-500 hover:bg-orange-600 text-white",
                  "shadow-lg shadow-orange-500/30"
                )}
                onClick={(e) => {
                  e.stopPropagation();
                  onQuickSubscribe?.();
                }}
                disabled={isSubscribing}
              >
                {isSubscribing ? (
                  <IconLoader2 className="size-3.5 animate-spin" />
                ) : (
                  <IconRss className="size-3.5" />
                )}
                <span className="text-xs font-medium">订阅</span>
              </Button>
            </div>
          )}

          {/* Bottom info overlay */}
          <div className="absolute inset-x-0 bottom-0 px-2.5 pb-2.5">
            {/* Title */}
            <h3 className="line-clamp-2 text-sm font-bold text-white drop-shadow-md">
              {displayName}
              {subject.season > 1 && ` 第${subject.season}季`}
            </h3>
          </div>
        </div>
      </div>
    </div>
  );
}

// Skeleton card for loading state
export function CalendarCardSkeleton() {
  return (
    <div className="relative">
      <div
        className={cn(
          "relative overflow-hidden rounded-2xl",
          "bg-linear-to-br from-white to-chart-1/5 dark:from-zinc-900 dark:to-chart-1/10",
          "border-2 border-chart-1/20 dark:border-chart-1/15",
          "shadow-md shadow-chart-1/10 dark:shadow-chart-1/5"
        )}
      >
        {/* Poster skeleton with shimmer */}
        <div className="relative aspect-2/3 overflow-hidden">
          <div className="absolute inset-0 animate-pulse bg-linear-to-r from-chart-1/10 via-chart-1/5 to-chart-1/10 dark:from-zinc-800 dark:via-zinc-700 dark:to-zinc-800" />

          {/* Bottom info skeleton overlay */}
          <div className="absolute inset-x-0 bottom-0 px-2.5 pb-2.5">
            <div className="mb-2 h-4 w-3/4 animate-pulse rounded bg-white/20" />
            <div className="h-2 w-1/2 animate-pulse rounded bg-white/10" />
          </div>
        </div>
      </div>
    </div>
  );
}
