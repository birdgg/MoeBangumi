import * as React from "react";
import { cn } from "@/lib/utils";
import {
  IconCalendar,
  IconCheck,
  IconClock,
  IconSparkles,
} from "@tabler/icons-react";

// Local display type for bangumi cards (separate from API type)
export interface BangumiData {
  id: number;
  chineseName: string;
  japaneseName: string;
  season: string;
  episodes: number;
  currentEpisode?: number;
  poster: string;
  airDate: string;
  isComplete: boolean;
}

interface BangumiCardProps {
  bangumi: BangumiData;
  className?: string;
  style?: React.CSSProperties;
}

export function BangumiCard({ bangumi, className, style }: BangumiCardProps) {
  const [isHovered, setIsHovered] = React.useState(false);

  const progress = bangumi.currentEpisode
    ? Math.round((bangumi.currentEpisode / bangumi.episodes) * 100)
    : 0;

  return (
    <div
      className={cn(
        "group relative",
        "animate-in fade-in slide-in-from-bottom-4 duration-500 ease-out",
        className
      )}
      style={style}
      onMouseEnter={() => setIsHovered(true)}
      onMouseLeave={() => setIsHovered(false)}
    >
      {/* Glow effect on hover */}
      <div
        className={cn(
          "absolute -inset-1 rounded-3xl opacity-0 blur-xl transition-opacity duration-500",
          "bg-linear-to-br from-chart-1/40 via-chart-3/40 to-chart-5/40",
          isHovered && "opacity-100"
        )}
      />

      {/* Card container */}
      <div
        className={cn(
          "relative overflow-hidden rounded-2xl",
          "bg-linear-to-br from-white/90 to-white/70 dark:from-zinc-900/90 dark:to-zinc-800/70",
          "border border-chart-1/30 dark:border-chart-3/20",
          "shadow-lg shadow-chart-1/20 dark:shadow-chart-3/30",
          "backdrop-blur-sm",
          "transition-all duration-500 ease-out",
          "hover:shadow-xl hover:shadow-chart-1/30 dark:hover:shadow-chart-3/40",
          "hover:-translate-y-1 hover:scale-[1.02]"
        )}
      >
        {/* Decorative sparkles */}
        <div className="absolute -right-2 -top-2 text-chart-1/60 dark:text-chart-3/40 transition-transform duration-500 group-hover:rotate-12 group-hover:scale-110">
          <IconSparkles className="size-8" />
        </div>

        {/* Poster section */}
        <div className="relative aspect-3/4 overflow-hidden">
          <img
            src={bangumi.poster}
            alt={bangumi.chineseName}
            className={cn(
              "size-full object-cover transition-all duration-700 ease-out",
              "group-hover:scale-110 group-hover:brightness-110"
            )}
          />

          {/* Gradient overlay */}
          <div className="absolute inset-0 bg-linear-to-t from-black/70 via-black/20 to-transparent" />

          {/* Cute wave decoration at bottom */}
          <svg
            className="absolute -bottom-1 left-0 right-0 h-8 text-white dark:text-zinc-900"
            viewBox="0 0 100 20"
            preserveAspectRatio="none"
          >
            <path
              d="M0 20 Q 25 0, 50 10 Q 75 20, 100 5 L 100 20 Z"
              fill="currentColor"
              className="opacity-90"
            />
          </svg>

          {/* Status badge */}
          <div className="absolute left-3 top-3">
            <div
              className={cn(
                "flex items-center gap-1.5 rounded-full px-3 py-1.5",
                "text-xs font-medium",
                "backdrop-blur-md transition-all duration-300",
                bangumi.isComplete
                  ? "bg-emerald-400/90 text-emerald-950 shadow-lg shadow-emerald-400/30"
                  : "bg-amber-400/90 text-amber-950 shadow-lg shadow-amber-400/30"
              )}
            >
              {bangumi.isComplete ? (
                <>
                  <IconCheck className="size-3.5" strokeWidth={2.5} />
                  <span>完结</span>
                </>
              ) : (
                <>
                  <IconClock className="size-3.5" strokeWidth={2.5} />
                  <span>连载中</span>
                </>
              )}
            </div>
          </div>

          {/* Episode info on poster */}
          <div className="absolute bottom-4 left-3 right-3">
            <div className="flex items-center justify-between text-white">
              <span className="text-sm font-medium drop-shadow-lg">
                {bangumi.currentEpisode
                  ? `第 ${bangumi.currentEpisode} 集`
                  : "未开始"}
              </span>
              <span className="text-sm font-medium drop-shadow-lg">
                共 {bangumi.episodes} 集
              </span>
            </div>
            {/* Progress bar */}
            {bangumi.currentEpisode && (
              <div className="mt-2 h-1.5 overflow-hidden rounded-full bg-white/30 backdrop-blur-sm">
                <div
                  className="h-full rounded-full bg-linear-to-r from-chart-1 to-chart-3 transition-all duration-500"
                  style={{ width: `${progress}%` }}
                />
              </div>
            )}
          </div>
        </div>

        {/* Info section */}
        <div className="relative space-y-3 p-4">
          {/* Titles */}
          <div className="space-y-1">
            <h3 className="line-clamp-1 text-base font-bold text-zinc-800 dark:text-zinc-100 transition-colors group-hover:text-chart-1 dark:group-hover:text-chart-1">
              {bangumi.chineseName}
            </h3>
            <p className="line-clamp-1 text-xs text-zinc-500 dark:text-zinc-400">
              {bangumi.japaneseName}
            </p>
          </div>

          {/* Meta info */}
          <div className="flex flex-wrap items-center gap-2">
            {/* Season tag */}
            <span
              className={cn(
                "inline-flex items-center rounded-full px-2.5 py-1",
                "text-xs font-medium",
                "bg-chart-1/15 text-chart-2",
                "dark:bg-chart-1/20 dark:text-chart-1",
                "border border-chart-1/30 dark:border-chart-1/20"
              )}
            >
              {bangumi.season}
            </span>

            {/* Air date */}
            <span
              className={cn(
                "inline-flex items-center gap-1 rounded-full px-2.5 py-1",
                "text-xs font-medium",
                "bg-chart-4/15 text-chart-5",
                "dark:bg-chart-4/20 dark:text-chart-4",
                "border border-chart-4/30 dark:border-chart-4/20"
              )}
            >
              <IconCalendar className="size-3" />
              {bangumi.airDate}
            </span>
          </div>

          {/* Decorative dots */}
          <div className="absolute -bottom-1 right-4 flex gap-1">
            <div className="size-1.5 rounded-full bg-chart-1/60 dark:bg-chart-1/40" />
            <div className="size-1.5 rounded-full bg-chart-3/60 dark:bg-chart-3/40" />
            <div className="size-1.5 rounded-full bg-chart-5/60 dark:bg-chart-5/40" />
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
        "grid gap-6",
        "grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 xl:grid-cols-4 2xl:grid-cols-5",
        className
      )}
    >
      {children}
    </div>
  );
}

// Demo data for preview
export const demoBangumiList: BangumiData[] = [
  {
    id: 1,
    chineseName: "葬送的芙莉莲",
    japaneseName: "葬送のフリーレン",
    season: "2024年冬",
    episodes: 28,
    currentEpisode: 28,
    poster: "https://lain.bgm.tv/pic/cover/l/5e/5e/425998_gQsv9.jpg",
    airDate: "2023-09-29",
    isComplete: true,
  },
  {
    id: 2,
    chineseName: "我独自升级",
    japaneseName: "俺だけレベルアップな件",
    season: "2024年冬",
    episodes: 12,
    currentEpisode: 8,
    poster: "https://lain.bgm.tv/pic/cover/l/44/1b/454686_P7jC4.jpg",
    airDate: "2024-01-06",
    isComplete: false,
  },
  {
    id: 3,
    chineseName: "药屋少女的呢喃",
    japaneseName: "薬屋のひとりごと",
    season: "2024年冬",
    episodes: 24,
    currentEpisode: 24,
    poster: "https://lain.bgm.tv/pic/cover/l/8c/e0/400602_w49R4.jpg",
    airDate: "2023-10-21",
    isComplete: true,
  },
  {
    id: 4,
    chineseName: "迷宫饭",
    japaneseName: "ダンジョン飯",
    season: "2024年冬",
    episodes: 24,
    currentEpisode: 15,
    poster: "https://lain.bgm.tv/pic/cover/l/4c/c6/302286_cIVpD.jpg",
    airDate: "2024-01-04",
    isComplete: false,
  },
  {
    id: 5,
    chineseName: "咒术回战 涩谷事变",
    japaneseName: "呪術廻戦 渋谷事変",
    season: "2023年秋",
    episodes: 23,
    currentEpisode: 23,
    poster: "https://lain.bgm.tv/pic/cover/l/4e/3d/371546_lkM58.jpg",
    airDate: "2023-07-06",
    isComplete: true,
  },
  {
    id: 6,
    chineseName: "间谍过家家",
    japaneseName: "SPY×FAMILY",
    season: "2023年秋",
    episodes: 12,
    poster: "https://lain.bgm.tv/pic/cover/l/23/33/378862_sjLVx.jpg",
    airDate: "2023-10-07",
    isComplete: true,
  },
];
