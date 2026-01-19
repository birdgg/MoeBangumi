import { cn } from "@/lib/utils";

interface YearBadgeProps {
  season: number;
  year: number;
  variant?: "default" | "minimal";
  className?: string;
}

// Helper function to format season
function formatSeason(season: number, year: number): { text: string; emoji: string } {
  const seasonMap: Record<number, { name: string; emoji: string }> = {
    1: { name: "冬", emoji: "❄️" },
    2: { name: "春", emoji: "🌸" },
    3: { name: "夏", emoji: "🌻" },
    4: { name: "秋", emoji: "🍂" },
  };
  const s = seasonMap[season] || { name: "", emoji: "✨" };
  return { text: `${year}${s.name}`, emoji: s.emoji };
}

export function YearBadge({ season, year, variant = "default", className }: YearBadgeProps) {
  const formatted = formatSeason(season, year);

  if (variant === "minimal") {
    return (
      <span className={cn("text-white/60 text-[11px]", className)}>
        {formatted.text}
      </span>
    );
  }

  return (
    <span
      className={cn(
        "inline-flex items-center gap-1 rounded-lg px-2 py-1",
        "text-[10px] font-medium text-white",
        "bg-white/15 backdrop-blur-xl",
        "border border-white/20",
        className
      )}
    >
      <span>{formatted.emoji}</span>
      <span>{formatted.text}</span>
    </span>
  );
}
