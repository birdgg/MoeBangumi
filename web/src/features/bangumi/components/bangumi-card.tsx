import { cn } from "@/lib/utils";
import type { BangumiWithMetadata } from "@/lib/api";

interface BangumiCardProps {
  bangumi: BangumiWithMetadata;
  className?: string;
  style?: React.CSSProperties;
  animate?: boolean;
  onClick?: () => void;
}

export function BangumiCard({ bangumi, className, style, animate = true, onClick }: BangumiCardProps) {
  const { metadata } = bangumi;

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
      {/* Hover glow */}
      <div
        className={cn(
          "absolute -inset-2 rounded-2xl opacity-0 blur-xl transition-all duration-500",
          "bg-chart-1/30",
          "group-hover:opacity-100"
        )}
      />

      {/* Card */}
      <div
        className={cn(
          "relative overflow-hidden rounded-xl",
          "bg-white dark:bg-zinc-900",
          "ring-1 ring-zinc-200 dark:ring-zinc-800",
          "shadow-sm transition-all duration-300",
          "group-hover:shadow-lg group-hover:-translate-y-0.5",
          "group-hover:ring-chart-1/50"
        )}
      >
        {/* Poster */}
        <div className="relative aspect-2/3 overflow-hidden">
          <img
            src={metadata.poster_url || "/placeholder.png"}
            alt={metadata.title_chinese}
            className="size-full object-cover transition-transform duration-500 group-hover:scale-105"
            loading="lazy"
          />

          {/* Gradient overlay */}
          <div className="absolute inset-0 bg-linear-to-t from-black/80 via-black/20 to-transparent" />

          {/* Bottom info */}
          <div className="absolute inset-x-0 bottom-0 p-3 space-y-2">
            {/* Title */}
            <h3
              className="line-clamp-2 text-sm font-bold text-white leading-snug drop-shadow-sm"
              title={metadata.title_chinese}
            >
              {metadata.title_chinese}
            </h3>

            {/* Season & Episode */}
            <div className="flex items-center gap-2 text-xs">
              {metadata.season > 0 && (
                <span className="glass-badge">第{metadata.season}季</span>
              )}
              <span className="glass-badge">更新至{bangumi.current_episode}集</span>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}

// Grid container
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

// Skeleton
export function BangumiCardSkeleton() {
  return (
    <div className="relative">
      <div
        className={cn(
          "relative overflow-hidden rounded-xl",
          "bg-white dark:bg-zinc-900",
          "ring-1 ring-zinc-200 dark:ring-zinc-800"
        )}
      >
        <div className="relative aspect-2/3 overflow-hidden">
          <div className="absolute inset-0 bg-zinc-200 dark:bg-zinc-800 animate-pulse" />
          <div className="absolute inset-x-0 bottom-0 p-3 space-y-2">
            <div className="h-4 w-3/4 rounded bg-white/20 animate-pulse" />
            <div className="flex gap-2">
              <div className="h-5 w-8 rounded bg-white/10 animate-pulse" />
              <div className="h-5 w-12 rounded bg-white/10 animate-pulse" />
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
