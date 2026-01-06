import { useState, useEffect } from "react";
import { useQuery } from "@tanstack/react-query";
import { IconSparkles, IconX, IconConfetti } from "@tabler/icons-react";
import { getVersionOptions } from "@/lib/api/client/@tanstack/react-query.gen";

const LAST_VERSION_KEY = "moe-bangumi-last-version";

export function UpdateBanner() {
  const [dismissed, setDismissed] = useState(false);
  const [justUpdated, setJustUpdated] = useState(false);
  const [isVisible, setIsVisible] = useState(true);

  const { data: versionInfo } = useQuery({
    ...getVersionOptions(),
    refetchInterval: 60 * 1000,
  });

  useEffect(() => {
    if (!versionInfo?.current) return;

    const lastVersion = localStorage.getItem(LAST_VERSION_KEY);
    if (lastVersion && lastVersion !== versionInfo.current) {
      setJustUpdated(true);
      // Delay visibility for smooth entrance
      setTimeout(() => setIsVisible(true), 100);
    }
    localStorage.setItem(LAST_VERSION_KEY, versionInfo.current);
  }, [versionInfo]);

  const handleDismiss = () => {
    setIsVisible(false);
    setTimeout(() => setDismissed(true), 300);
  };

  if (dismissed || !justUpdated) {
    return null;
  }

  return (
    <div className="fixed inset-x-0 top-0 z-50 flex justify-center pointer-events-none px-4 pt-4">
      <div
        className={`
          pointer-events-auto
          relative overflow-hidden
          flex items-center gap-3
          px-5 py-3
          rounded-2xl
          border border-chart-1/20
          bg-linear-to-r from-chart-1/10 via-chart-2/10 to-chart-1/10
          backdrop-blur-xl
          shadow-[0_8px_32px_-8px] shadow-chart-1/25
          transition-all duration-300 ease-out
          ${isVisible
            ? "opacity-100 translate-y-0 scale-100"
            : "opacity-0 -translate-y-4 scale-95"
          }
        `}
      >
        {/* Animated gradient border glow */}
        <div className="absolute inset-0 rounded-2xl bg-linear-to-r from-chart-1/20 via-chart-3/20 to-chart-1/20 blur-xl -z-10 animate-pulse" />

        {/* Sparkle decorations */}
        <span className="absolute -top-1 -left-1 text-chart-1/60 animate-bounce text-xs">✦</span>
        <span className="absolute -bottom-1 -right-1 text-chart-2/60 animate-bounce text-xs" style={{ animationDelay: "0.2s" }}>✦</span>

        {/* Icon with glow */}
        <div className="relative">
          <IconConfetti className="size-5 text-chart-1 drop-shadow-[0_0_8px_var(--chart-1)]" />
          <IconSparkles className="absolute -top-1 -right-1 size-3 text-chart-2 animate-ping" />
        </div>

        {/* Content */}
        <div className="flex items-center gap-2">
          <span className="text-sm font-medium bg-linear-to-r from-chart-1 to-chart-2 bg-clip-text text-transparent">
            已更新到
          </span>
          <a
            href={`https://github.com/birdgg/moe-bangumi/releases/tag/v${versionInfo?.current}`}
            target="_blank"
            rel="noopener noreferrer"
            className="px-2 py-0.5 rounded-full bg-chart-1/15 text-chart-1 text-sm font-bold tabular-nums hover:bg-chart-1/25 transition-colors cursor-pointer"
          >
            v{versionInfo?.current}
          </a>
        </div>

        {/* Close button */}
        <button
          onClick={handleDismiss}
          className="
            ml-1 p-1.5 rounded-full
            text-muted-foreground/60
            hover:text-chart-1 hover:bg-chart-1/10
            transition-all duration-200
            hover:rotate-90
          "
          aria-label="关闭"
        >
          <IconX className="size-4" />
        </button>
      </div>
    </div>
  );
}
