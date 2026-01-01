import { cn } from "@/lib/utils";
import { IconDeviceTv } from "@tabler/icons-react";
import { YearBadge } from "../year-badge";
import { PlatformBadge } from "../platform-badge";

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
  /** BGM.tv ID for linking to https://bgm.tv/subject/{id} */
  bgmtvId?: number | null;
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
  bgmtvId,
  className,
}: BangumiInfoCardProps) {
  const hasTags = (year && broadcastSeason) || totalEpisodes || seasonNumber || platform;

  const Wrapper = bgmtvId ? "a" : "div";
  const wrapperProps = bgmtvId
    ? {
      href: `https://bgm.tv/subject/${bgmtvId}`,
      target: "_blank",
      rel: "noopener noreferrer",
    }
    : {};

  return (
    <Wrapper
      {...wrapperProps}
      className={cn(
        "relative overflow-hidden rounded-xl",
        "border border-white/20 dark:border-white/10",
        "bg-white/60 dark:bg-white/5 backdrop-blur-xl",
        "shadow-sm shadow-chart-1/5",
        bgmtvId && "block cursor-pointer transition-all hover:border-chart-1/30 hover:bg-white/70 dark:hover:bg-white/10 hover:shadow-md hover:shadow-chart-1/10",
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
                <YearBadge season={broadcastSeason} year={year} />
              )}
              <PlatformBadge platform={platform} className="rounded-md" />
            </div>
          )}
        </div>
      </div>
    </Wrapper>
  );
}
