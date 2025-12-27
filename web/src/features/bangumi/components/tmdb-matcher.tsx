import * as React from "react";
import { cn } from "@/lib/utils";
import { useSearchTmdb } from "../hooks/use-bangumi";
import { type TvShow } from "@/lib/api";
import { IconLoader2, IconMovie, IconCheck, IconChevronDown, IconExternalLink } from "@tabler/icons-react";
import { useDebouncedValue } from "@tanstack/react-pacer";
import { Tooltip, TooltipTrigger, TooltipContent } from "@/components/ui/tooltip";

interface TmdbMatcherProps {
  value: TvShow | null;
  onChange: (show: TvShow | null) => void;
  /** Search keyword (controlled from outside, e.g., from title_chinese input) */
  keyword: string;
  className?: string;
  /** Pre-filled TMDB ID (disables initial auto-search until dropdown is opened) */
  initialTmdbId?: number;
  /** Pre-filled TMDB title to display when initialTmdbId is provided */
  initialTmdbTitle?: string;
}

const TMDB_IMAGE_BASE = "https://image.tmdb.org/t/p/w92";

export function TmdbMatcher({
  value,
  onChange,
  keyword,
  className,
  initialTmdbId,
  initialTmdbTitle,
}: TmdbMatcherProps) {
  const [open, setOpen] = React.useState(false);
  const [debouncedKeyword] = useDebouncedValue(keyword, { wait: 400 });
  const hasSearchedRef = React.useRef(false);
  const triggerRef = React.useRef<HTMLButtonElement>(null);
  // Track if user has ever opened the dropdown (to enable search when initialTmdbId is provided)
  const hasOpenedRef = React.useRef(false);

  // Disable search until user opens dropdown when initialTmdbId is provided
  const shouldSearch = !initialTmdbId || hasOpenedRef.current;
  const searchKeyword = shouldSearch ? debouncedKeyword : "";

  const { data, isLoading, isFetching } = useSearchTmdb(searchKeyword);
  const results = data ?? [];
  const isSearching = shouldSearch && (isLoading || isFetching);

  // Compute display value: use value if set, otherwise fallback to initial props
  const displayValue = value ?? (initialTmdbId ? { id: initialTmdbId, name: initialTmdbTitle ?? `TMDB #${initialTmdbId}` } as TvShow : null);

  // Auto-select first result when search completes (only once per search term)
  // Skip auto-select when initialTmdbId is provided and user hasn't opened dropdown yet
  React.useEffect(() => {
    if (
      shouldSearch &&
      !isSearching &&
      results.length > 0 &&
      debouncedKeyword &&
      !hasSearchedRef.current
    ) {
      hasSearchedRef.current = true;
      onChange(results[0]);
    }
  }, [shouldSearch, isSearching, results, debouncedKeyword, onChange]);

  // Reset search flag when keyword changes
  React.useEffect(() => {
    hasSearchedRef.current = false;
  }, [debouncedKeyword]);

  const handleSelect = (show: TvShow) => {
    onChange(show);
    setOpen(false);
  };

  const handleToggleOpen = () => {
    if (!open) {
      hasOpenedRef.current = true;
    }
    setOpen(!open);
  };

  return (
    <div className={cn("space-y-2", className)}>
      {/* Trigger area */}
      <div className="flex items-center gap-1.5">
        <button
          ref={triggerRef}
          type="button"
          onClick={handleToggleOpen}
          className={cn(
            "flex flex-1 items-center justify-between gap-2 rounded-lg border px-3 py-2 text-sm",
            "border-input bg-transparent",
            "hover:bg-accent hover:text-accent-foreground",
            "focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-ring",
            "disabled:cursor-not-allowed disabled:opacity-50"
          )}
        >
          <span className={cn("truncate flex items-center gap-2", !displayValue && "text-muted-foreground")}>
            {displayValue ? (
              <>
                <span className="truncate">{displayValue.name}</span>
                <span className="font-mono text-xs text-chart-3 dark:text-chart-1 shrink-0">#{displayValue.id}</span>
              </>
            ) : (
              "选择 TMDB 匹配..."
            )}
          </span>
          <div className="flex items-center gap-1">
            {isSearching && (
              <IconLoader2 className="size-4 animate-spin text-chart-3 dark:text-chart-1" />
            )}
            <IconChevronDown className="size-4 text-muted-foreground" />
          </div>
        </button>

        {/* External link to TMDB */}
        {displayValue && (
          <Tooltip>
            <TooltipTrigger
              render={
                <a
                  href={`https://www.themoviedb.org/tv/${displayValue.id}`}
                  target="_blank"
                  rel="noopener noreferrer"
                  className={cn(
                    "flex items-center justify-center size-9 shrink-0 rounded-lg border",
                    "border-input bg-transparent",
                    "hover:bg-chart-1/10 hover:border-chart-1/50 hover:text-chart-1",
                    "focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-ring",
                    "transition-colors duration-200"
                  )}
                />
              }
            >
              <IconExternalLink className="size-4" />
            </TooltipTrigger>
            <TooltipContent side="top">在 TMDB 中查看</TooltipContent>
          </Tooltip>
        )}
      </div>

      {/* Dropdown */}
      {open && (
        <>
          {/* Backdrop */}
          <div
            className="fixed inset-0 z-40"
            onClick={() => setOpen(false)}
          />
          {/* Dropdown content */}
          <div className="relative z-50">
            <div className="absolute top-0 left-0 right-0 max-h-64 overflow-y-auto rounded-lg border bg-popover p-1 shadow-lg animate-in fade-in-0 zoom-in-95 slide-in-from-top-2 duration-200">
              {results.length === 0 ? (
                <div className="py-6 text-center text-sm text-muted-foreground">
                  {isSearching ? (
                    <span className="flex items-center justify-center gap-2">
                      <IconLoader2 className="size-4 animate-spin" />
                      搜索中...
                    </span>
                  ) : keyword ? (
                    "未找到结果"
                  ) : (
                    "请输入中文标题以搜索"
                  )}
                </div>
              ) : (
                results.map((show: TvShow) => (
                  <button
                    key={show.id}
                    type="button"
                    onClick={() => handleSelect(show)}
                    className={cn(
                      "flex w-full items-center gap-3 rounded-md px-2 py-2 text-left",
                      "hover:bg-accent hover:text-accent-foreground",
                      value?.id === show.id && "bg-accent"
                    )}
                  >
                    {/* Poster thumbnail */}
                    <div className="relative size-10 shrink-0 overflow-hidden rounded-md bg-muted">
                      {show.poster_path ? (
                        <img
                          src={`${TMDB_IMAGE_BASE}${show.poster_path}`}
                          alt={show.name}
                          className="size-full object-cover"
                        />
                      ) : (
                        <div className="flex size-full items-center justify-center">
                          <IconMovie className="size-5 text-muted-foreground" />
                        </div>
                      )}
                    </div>

                    {/* Show info */}
                    <div className="flex min-w-0 flex-1 flex-col">
                      <span className="truncate font-medium">{show.name}</span>
                      <div className="flex items-center gap-2 text-xs text-muted-foreground">
                        <span className="font-mono text-chart-3 dark:text-chart-1">
                          #{show.id}
                        </span>
                        {show.first_air_date && (
                          <>
                            <span className="text-border">|</span>
                            <span>{show.first_air_date.split("-")[0]}</span>
                          </>
                        )}
                        {show.original_name !== show.name && (
                          <>
                            <span className="text-border">|</span>
                            <span className="truncate">{show.original_name}</span>
                          </>
                        )}
                      </div>
                    </div>

                    {/* Check icon for selected */}
                    {value?.id === show.id && (
                      <IconCheck className="size-4 shrink-0 text-chart-3 dark:text-chart-1" />
                    )}
                  </button>
                ))
              )}
            </div>
          </div>
        </>
      )}

    </div>
  );
}
