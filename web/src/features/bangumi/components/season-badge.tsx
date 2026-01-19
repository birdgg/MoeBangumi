import { cn } from "@/lib/utils";

interface SeasonBadgeProps {
  /** Season number of the show (e.g., 1, 2, 3) */
  season: number;
  variant?: "default" | "overlay" | "minimal";
  className?: string;
}

export function SeasonBadge({ season, variant = "default", className }: SeasonBadgeProps) {
  if (season <= 0) return null;

  const text = `第${season}季`;

  if (variant === "minimal") {
    return (
      <span className={cn("text-white/60 text-[11px]", className)}>
        {text}
      </span>
    );
  }

  if (variant === "overlay") {
    return (
      <span
        className={cn(
          "inline-flex items-center font-medium",
          "px-2.5 py-1 rounded-full text-[10px]",
          "bg-chart-1/20 backdrop-blur-xl backdrop-saturate-150 text-chart-1",
          "dark:bg-chart-1/30 dark:text-chart-1",
          "border border-chart-1/20",
          "shadow-[0_2px_8px_rgba(0,0,0,0.1)]",
          className
        )}
      >
        {text}
      </span>
    );
  }

  // Default variant
  return (
    <span
      className={cn(
        "inline-flex items-center rounded-lg px-2 py-1",
        "text-[10px] font-medium",
        "bg-chart-1/10 text-chart-1",
        "dark:bg-chart-1/20 dark:text-chart-1",
        "border border-chart-1/20",
        className
      )}
    >
      {text}
    </span>
  );
}
