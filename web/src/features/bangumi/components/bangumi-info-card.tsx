import { cn } from "@/lib/utils";
import { IconDeviceTv } from "@tabler/icons-react";
import { SeasonBadge } from "./season-badge";
import { StatusBadge } from "./status-badge";

interface BangumiInfoCardProps {
  posterUrl?: string | null;
  titleChinese: string;
  titleJapanese?: string | null;
  /** Broadcast year */
  year?: number | null;
  /** Broadcast season (1=winter, 2=spring, 3=summer, 4=fall) */
  broadcastSeason?: number | null;
  totalEpisodes?: number | null;
  /** Season number of the show (e.g., Season 2) */
  seasonNumber?: number | null;
  platform?: string | null;
  isFinished?: boolean;
  className?: string;
}

export function BangumiInfoCard({
  posterUrl,
  titleChinese,
  titleJapanese,
  year,
  broadcastSeason,
  totalEpisodes,
  seasonNumber,
  platform,
  isFinished,
  className,
}: BangumiInfoCardProps) {
  const hasTags = (year && broadcastSeason) || totalEpisodes || seasonNumber || platform || isFinished !== undefined;

  return (
    <div
      className={cn(
        "relative overflow-hidden rounded-xl border border-chart-3/20 dark:border-chart-1/20",
        "bg-linear-to-br from-chart-3/5 via-transparent to-chart-1/5 dark:from-chart-3/10 dark:to-chart-1/10",
        className
      )}
    >
      {/* Background blur effect from poster */}
      {posterUrl && (
        <div
          className="absolute inset-0 opacity-20 dark:opacity-30 blur-2xl scale-150"
          style={{
            backgroundImage: `url(${posterUrl})`,
            backgroundSize: "cover",
            backgroundPosition: "center",
          }}
        />
      )}

      <div className={cn("relative flex gap-4 p-4")}>
        {/* Poster */}
        <div className="shrink-0">
          {posterUrl ? (
            <img
              src={posterUrl}
              alt={titleChinese}
              className={cn(
                "object-cover rounded-lg shadow-lg",
                "w-20 h-28 ring-1 ring-white/10"
              )}
            />
          ) : (
            <div className="w-20 h-28 rounded-lg bg-chart-3/20 dark:bg-chart-1/20 flex items-center justify-center">
              <IconDeviceTv className="size-8 text-chart-3/50 dark:text-chart-1/50" />
            </div>
          )}
        </div>

        {/* Info */}
        <div className="flex-1 min-w-0 flex flex-col justify-between py-0.5">
          <div className="space-y-1">
            <h3 className="font-semibold text-foreground truncate">
              {titleChinese}
            </h3>
            {titleJapanese && (
              <p className="text-xs text-muted-foreground truncate">
                {titleJapanese}
              </p>
            )}
          </div>

          {/* Tags */}
          {hasTags && (
            <div className="flex flex-wrap gap-1.5 mt-2">
              {year && broadcastSeason && (
                <SeasonBadge season={broadcastSeason} year={year} />
              )}
              {platform && (
                <span className="inline-flex items-center gap-1 px-2 py-0.5 rounded-md text-xs bg-chart-2/15 dark:bg-chart-2/20 text-chart-2 dark:text-chart-2">
                  {platform}
                </span>
              )}
              {isFinished !== undefined && (
                <StatusBadge finished={isFinished} />
              )}
            </div>
          )}
        </div>
      </div>
    </div>
  );
}
