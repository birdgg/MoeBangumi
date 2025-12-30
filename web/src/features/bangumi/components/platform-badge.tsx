import { cn } from "@/lib/utils";
import { IconDeviceTv, IconMovie, IconVideoPlus } from "@tabler/icons-react";
import type { Platform } from "@/lib/api";

interface PlatformBadgeProps {
  platform: Platform | string | null | undefined;
  variant?: "default" | "overlay" | "minimal";
  className?: string;
}

const PLATFORM_CONFIG = {
  tv: { icon: IconDeviceTv, label: "TV" },
  movie: { icon: IconMovie, label: "Movie" },
  ova: { icon: IconVideoPlus, label: "OVA" },
} as const;

// Map external platform strings to our Platform enum
// BGM.tv uses "TV", "剧场版", "OVA" etc.
function normalizePlatform(platform: string | null | undefined): Platform | null {
  if (!platform) return null;
  const lower = platform.toLowerCase();
  if (lower === "tv" || lower === "web") return "tv";
  if (lower === "movie" || lower === "剧场版" || lower === "劇場版") return "movie";
  if (lower === "ova" || lower === "oad") return "ova";
  return null;
}

function getPlatformIcon(platform: Platform | null) {
  if (!platform) return null;
  const Icon = PLATFORM_CONFIG[platform].icon;
  return <Icon className="size-3" />;
}

function getPlatformLabel(platform: Platform | null): string | null {
  if (!platform) return null;
  return PLATFORM_CONFIG[platform].label;
}

export function PlatformBadge({ platform: rawPlatform, variant = "default", className }: PlatformBadgeProps) {
  const platform = normalizePlatform(rawPlatform);
  const label = getPlatformLabel(platform);
  if (!label) return null;

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
      {variant !== "minimal" && getPlatformIcon(platform)}
      {label}
    </span>
  );
}
