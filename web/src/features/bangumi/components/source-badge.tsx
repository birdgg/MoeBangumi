import { cn } from "@/lib/utils";
import { IconWorld, IconDisc } from "@tabler/icons-react";
import type { SourceType } from "@/lib/api";

interface SourceBadgeProps {
  source: SourceType | null | undefined;
  variant?: "default" | "overlay" | "minimal";
  className?: string;
}

export function SourceBadge({ source, variant = "default", className }: SourceBadgeProps) {
  if (!source) return null;

  const isWeb = source === "webrip";
  const label = isWeb ? "WEB" : "BD";
  const Icon = isWeb ? IconWorld : IconDisc;

  return (
    <span
      className={cn(
        "inline-flex items-center gap-1 font-medium",
        variant === "default" && [
          "px-2 py-0.5 rounded-full text-xs",
          "bg-zinc-100 text-zinc-600",
          "dark:bg-zinc-800 dark:text-zinc-400",
        ],
        variant === "overlay" && [
          "px-2.5 py-1 rounded-full text-[10px]",
          "bg-white/20 backdrop-blur-xl backdrop-saturate-150 text-white",
          "border border-white/10",
          "shadow-[0_2px_8px_rgba(0,0,0,0.1)]",
          "transition-transform duration-300 hover:scale-105",
        ],
        variant === "minimal" && "text-white/60 text-[11px]",
        className
      )}
    >
      {variant !== "minimal" && <Icon className="size-3" />}
      {label}
    </span>
  );
}
