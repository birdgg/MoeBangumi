import { cn } from "@/lib/utils";
import type { CalendarSubject } from "@/lib/api";

interface CalendarCardProps {
  subject: CalendarSubject;
  className?: string;
  onClick?: () => void;
}

export function CalendarCard({
  subject,
  className,
  onClick,
}: CalendarCardProps) {
  const displayName = subject.name_cn || subject.name;

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
            src={subject.images.large || subject.images.common}
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

          {/* Bottom info overlay */}
          <div className="absolute inset-x-0 bottom-0 px-2.5 pb-2.5">
            {/* Title */}
            <h3 className="line-clamp-2 text-sm font-bold text-white drop-shadow-md">
              {displayName}
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
