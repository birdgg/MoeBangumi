import { cn } from "@/lib/utils";
import { IconDownload } from "@tabler/icons-react";
import type { Bangumi } from "@/lib/api";
import { SeasonBadge } from "./season-badge";
import { StatusBadge } from "./status-badge";

interface BangumiCardProps {
  bangumi: Bangumi;
  className?: string;
  style?: React.CSSProperties;
  animate?: boolean;
  onClick?: () => void;
}

export function BangumiCard({ bangumi, className, style, animate = true, onClick }: BangumiCardProps) {
  const progress =
    bangumi.current_episode && bangumi.total_episodes > 0
      ? Math.round((bangumi.current_episode / bangumi.total_episodes) * 100)
      : 0;

  return (
    <div
      className={cn(
        "group relative",
        animate && "animate-in fade-in slide-in-from-bottom-3 duration-500 ease-out",
        onClick && "cursor-pointer",
        className
      )}
      style={style}
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
            src={bangumi.poster_url || "/placeholder.png"}
            alt={bangumi.title_original_chinese}
            className={cn(
              "size-full object-cover transition-all duration-500 ease-out",
              "group-hover:scale-110 group-hover:brightness-105"
            )}
          />

          {/* Gradient overlay */}
          <div className="absolute inset-0 bg-linear-to-t from-black/70 via-transparent to-chart-1/10" />

          {/* Decorative top wave */}
          <div className="absolute inset-x-0 top-0 h-8 bg-linear-to-b from-chart-1/20 to-transparent" />

          {/* Top badges row */}
          <div className="absolute inset-x-2 top-2 flex items-center justify-between">
            <StatusBadge finished={bangumi.finished} variant="overlay" />
            <SeasonBadge season={bangumi.season} year={bangumi.year} />
          </div>

          {/* Bottom info overlay */}
          <div className="absolute inset-x-0 bottom-0 px-2.5 pb-2.5">
            {/* Title with kind badge */}
            <div className="flex items-center gap-1.5 mb-1.5">
              <h3 className="line-clamp-1 text-sm font-bold text-white min-w-0 flex-1 drop-shadow-md">
                {bangumi.title_original_chinese}
              </h3>
              {bangumi.kind && (
                <span
                  className={cn(
                    "shrink-0 inline-flex items-center rounded px-1 py-0.5",
                    "text-[9px] font-bold",
                    "bg-white/20 text-white backdrop-blur-sm"
                  )}
                >
                  {bangumi.kind}
                </span>
              )}
            </div>

            {/* Episode progress */}
            <div className="flex items-center gap-2">
              <div className="flex items-center gap-1 text-white/90">
                <IconDownload className="size-3 drop-shadow-sm" strokeWidth={2.5} />
                <span className="text-xs font-bold tabular-nums drop-shadow-sm">
                  {bangumi.current_episode}/{bangumi.total_episodes || "?"}
                </span>
              </div>

              {/* Progress bar */}
              {bangumi.total_episodes > 0 && (
                <div className="flex-1 h-1.5 overflow-hidden rounded-full bg-white/20 backdrop-blur-sm">
                  <div
                    className={cn(
                      "h-full rounded-full transition-all duration-500",
                      progress === 100
                        ? "bg-linear-to-r from-emerald-400 to-teal-400"
                        : "bg-linear-to-r from-chart-1 via-chart-3 to-chart-5"
                    )}
                    style={{ width: `${progress}%` }}
                  />
                </div>
              )}
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}

// Grid container for multiple cards
interface BangumiGridProps {
  children: React.ReactNode;
  className?: string;
}

export function BangumiGrid({ children, className }: BangumiGridProps) {
  return (
    <div
      className={cn(
        "grid gap-4",
        "grid-cols-2 sm:grid-cols-3 md:grid-cols-4 lg:grid-cols-5 xl:grid-cols-6 2xl:grid-cols-7",
        className
      )}
    >
      {children}
    </div>
  );
}

// Skeleton card for loading state
export function BangumiCardSkeleton() {
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
          <div className="absolute inset-0 bg-linear-to-r from-chart-1/10 via-chart-1/5 to-chart-1/10 dark:from-zinc-800 dark:via-zinc-700 dark:to-zinc-800 animate-pulse" />

          {/* Bottom info skeleton overlay */}
          <div className="absolute inset-x-0 bottom-0 px-2.5 pb-2.5">
            <div className="h-4 w-3/4 rounded bg-white/20 animate-pulse mb-2" />
            <div className="h-2 w-1/2 rounded bg-white/10 animate-pulse" />
          </div>
        </div>
      </div>
    </div>
  );
}
