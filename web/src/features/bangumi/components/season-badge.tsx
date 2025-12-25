import { cn } from "@/lib/utils";

interface SeasonBadgeProps {
  season: number;
  year: number;
  className?: string;
}

// Helper function to format season with cute emoji
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

export function SeasonBadge({ season, year, className }: SeasonBadgeProps) {
  const formatted = formatSeason(season, year);

  return (
    <span
      className={cn(
        "inline-flex items-center gap-0.5 rounded-full px-2 py-0.5",
        "text-[10px] font-bold",
        "bg-white/80 text-chart-2 backdrop-blur-md shadow-sm",
        "dark:bg-zinc-900/80 dark:text-chart-1",
        className
      )}
    >
      <span>{formatted.emoji}</span>
      <span>{formatted.text}</span>
    </span>
  );
}
