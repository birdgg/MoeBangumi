import * as React from "react";
import { Dialog as DialogPrimitive } from "@base-ui/react/dialog";
import { useDebouncedValue } from "@tanstack/react-pacer";
import { cn } from "@/lib/utils";
import { useSearchBangumi } from "../hooks/use-bangumi";
import { type Subject } from "@/lib/api";
import {
  IconSearch,
  IconX,
  IconLoader2,
  IconSparkles,
  IconCalendar,
  IconMovie,
  IconDeviceTv,
  IconWorld,
  IconCheck,
} from "@tabler/icons-react";

interface SearchBangumiModalProps {
  open: boolean;
  onOpenChange: (open: boolean) => void;
  onSelect?: (subject: Subject) => void;
}

export function SearchBangumiModal({
  open,
  onOpenChange,
  onSelect,
}: SearchBangumiModalProps) {
  const [searchQuery, setSearchQuery] = React.useState("");
  const [debouncedQuery] = useDebouncedValue(searchQuery, { wait: 300 });
  const [selectedId, setSelectedId] = React.useState<number | null>(null);
  const inputRef = React.useRef<HTMLInputElement>(null);

  // Reset state when modal opens
  React.useEffect(() => {
    if (open) {
      setSearchQuery("");
      setSelectedId(null);
      // Focus input after animation
      setTimeout(() => inputRef.current?.focus(), 100);
    }
  }, [open]);

  const { data: results, isLoading, isError } = useSearchBangumi(debouncedQuery);

  const handleSelect = (subject: Subject) => {
    setSelectedId(subject.id);
    onSelect?.(subject);
    // Close after brief delay to show selection
    setTimeout(() => {
      onOpenChange(false);
    }, 200);
  };

  const getPlatformIcon = (platform: string | null) => {
    switch (platform) {
      case "TV":
        return <IconDeviceTv className="size-3.5" />;
      case "Web":
        return <IconWorld className="size-3.5" />;
      case "剧场版":
        return <IconMovie className="size-3.5" />;
      default:
        return null;
    }
  };

  return (
    <DialogPrimitive.Root open={open} onOpenChange={onOpenChange}>
      <DialogPrimitive.Portal>
        {/* Backdrop with blur */}
        <DialogPrimitive.Backdrop
          className={cn(
            "fixed inset-0 z-50",
            "bg-black/20 dark:bg-black/40 backdrop-blur-sm",
            "data-[state=open]:animate-in data-[state=closed]:animate-out",
            "data-[state=closed]:fade-out-0 data-[state=open]:fade-in-0",
            "duration-200"
          )}
        />

        {/* Modal Content */}
        <DialogPrimitive.Popup
          className={cn(
            "fixed left-1/2 top-[10%] z-50 -translate-x-1/2",
            "w-[calc(100%-2rem)] max-w-2xl",
            "max-h-[80vh] overflow-hidden",
            "rounded-2xl",
            "bg-linear-to-br from-white/95 via-white/90 to-chart-1/10",
            "dark:from-zinc-900/95 dark:via-zinc-900/90 dark:to-chart-3/20",
            "border border-chart-1/30 dark:border-chart-3/30",
            "shadow-2xl shadow-chart-3/20 dark:shadow-chart-3/50",
            "backdrop-blur-xl",
            "data-[state=open]:animate-in data-[state=closed]:animate-out",
            "data-[state=closed]:fade-out-0 data-[state=open]:fade-in-0",
            "data-[state=closed]:zoom-out-95 data-[state=open]:zoom-in-95",
            "data-[state=closed]:slide-out-to-top-4 data-[state=open]:slide-in-from-top-4",
            "duration-300 ease-out",
            "outline-none"
          )}
        >
          {/* Decorative elements */}
          <div className="pointer-events-none absolute -right-20 -top-20 size-40 rounded-full bg-linear-to-br from-chart-1/30 to-chart-3/30 blur-3xl dark:from-chart-1/20 dark:to-chart-3/20" />
          <div className="pointer-events-none absolute -left-20 bottom-0 size-40 rounded-full bg-linear-to-br from-chart-5/30 to-chart-3/30 blur-3xl dark:from-chart-5/20 dark:to-chart-3/20" />

          {/* Header */}
          <div className="relative border-b border-chart-1/30 dark:border-chart-3/20 p-4">
            <div className="flex items-center gap-3 mb-4">
              <div className="flex size-10 items-center justify-center rounded-xl bg-linear-to-br from-chart-1 to-chart-3 text-white shadow-lg shadow-chart-3/30">
                <IconSparkles className="size-5" />
              </div>
              <div>
                <DialogPrimitive.Title className="text-lg font-bold bg-linear-to-r from-chart-1 via-chart-3 to-chart-5 bg-clip-text text-transparent">
                  搜索番剧
                </DialogPrimitive.Title>
                <DialogPrimitive.Description className="text-xs text-muted-foreground">
                  从 BGM.tv 搜索并添加你喜欢的番剧
                </DialogPrimitive.Description>
              </div>
              <DialogPrimitive.Close
                className={cn(
                  "ml-auto flex size-8 items-center justify-center rounded-lg",
                  "text-muted-foreground hover:text-foreground",
                  "hover:bg-chart-1/20 dark:hover:bg-chart-3/30",
                  "transition-colors duration-200",
                  "outline-none focus-visible:ring-2 focus-visible:ring-chart-1 dark:focus-visible:ring-chart-3"
                )}
              >
                <IconX className="size-4" />
              </DialogPrimitive.Close>
            </div>

            {/* Search Input */}
            <div className="relative">
              <IconSearch className="absolute left-3 top-1/2 size-4 -translate-y-1/2 text-chart-1/60 dark:text-chart-3/60" />
              {isLoading && (
                <IconLoader2 className="absolute right-3 top-1/2 size-4 -translate-y-1/2 text-chart-1 dark:text-chart-3 animate-spin" />
              )}
              <input
                ref={inputRef}
                type="text"
                value={searchQuery}
                onChange={(e) => setSearchQuery(e.target.value)}
                placeholder="输入番剧名称开始搜索..."
                className={cn(
                  "w-full h-11 pl-10 pr-10 rounded-xl",
                  "bg-white/70 dark:bg-zinc-800/70",
                  "border border-chart-1/30 dark:border-chart-3/30",
                  "text-sm placeholder:text-muted-foreground",
                  "outline-none",
                  "focus:border-chart-1 dark:focus:border-chart-3",
                  "focus:ring-2 focus:ring-chart-1/20 dark:focus:ring-chart-3/20",
                  "transition-all duration-200"
                )}
              />
            </div>
          </div>

          {/* Results */}
          <div className="relative max-h-[calc(80vh-180px)] overflow-y-auto p-4">
            {/* Empty state */}
            {!debouncedQuery && (
              <div className="flex flex-col items-center justify-center py-12 text-center">
                <div className="mb-4 flex size-16 items-center justify-center rounded-2xl bg-linear-to-br from-chart-1/20 to-chart-3/20 dark:from-chart-1/30 dark:to-chart-3/30">
                  <IconSearch className="size-8 text-chart-1 dark:text-chart-3" />
                </div>
                <p className="text-sm text-muted-foreground">
                  输入关键词搜索你想添加的番剧
                </p>
              </div>
            )}

            {/* Loading state */}
            {isLoading && debouncedQuery && (
              <div className="flex flex-col items-center justify-center py-12">
                <IconLoader2 className="size-8 text-chart-1 dark:text-chart-3 animate-spin mb-3" />
                <p className="text-sm text-muted-foreground">搜索中...</p>
              </div>
            )}

            {/* Error state */}
            {isError && (
              <div className="flex flex-col items-center justify-center py-12 text-center">
                <div className="mb-4 flex size-16 items-center justify-center rounded-2xl bg-red-100 dark:bg-red-900/30">
                  <IconX className="size-8 text-red-500" />
                </div>
                <p className="text-sm text-muted-foreground">
                  搜索失败，请稍后重试
                </p>
              </div>
            )}

            {/* No results */}
            {!isLoading && !isError && debouncedQuery && results?.length === 0 && (
              <div className="flex flex-col items-center justify-center py-12 text-center">
                <div className="mb-4 flex size-16 items-center justify-center rounded-2xl bg-linear-to-br from-chart-1/20 to-chart-3/20 dark:from-chart-1/30 dark:to-chart-3/30">
                  <IconSparkles className="size-8 text-chart-1 dark:text-chart-3" />
                </div>
                <p className="text-sm text-muted-foreground">
                  没有找到相关番剧
                </p>
              </div>
            )}

            {/* Results grid */}
            {!isLoading && !isError && results && results.length > 0 && (
              <div className="grid gap-3">
                {results.map((subject, index) => (
                  <button
                    key={subject.id}
                    onClick={() => handleSelect(subject)}
                    className={cn(
                      "group relative flex gap-4 p-3 rounded-xl text-left",
                      "bg-white/50 dark:bg-zinc-800/50",
                      "border border-chart-1/20 dark:border-chart-3/20",
                      "hover:bg-chart-1/10 dark:hover:bg-chart-3/20",
                      "hover:border-chart-1/50 dark:hover:border-chart-3/50",
                      "hover:shadow-lg hover:shadow-chart-1/20 dark:hover:shadow-chart-3/30",
                      "transition-all duration-200",
                      "outline-none focus-visible:ring-2 focus-visible:ring-chart-1 dark:focus-visible:ring-chart-3",
                      "animate-in fade-in slide-in-from-bottom-2",
                      selectedId === subject.id && "ring-2 ring-chart-1 dark:ring-chart-3 bg-chart-1/10 dark:bg-chart-3/20"
                    )}
                    style={{ animationDelay: `${index * 50}ms`, animationFillMode: "both" }}
                  >
                    {/* Poster */}
                    <div className="relative size-20 shrink-0 overflow-hidden rounded-lg bg-linear-to-br from-chart-1/20 to-chart-3/20 dark:from-chart-1/30 dark:to-chart-3/30">
                      {subject.image ? (
                        <img
                          src={subject.image}
                          alt={subject.name_cn || subject.name}
                          className="size-full object-cover transition-transform duration-300 group-hover:scale-110"
                        />
                      ) : (
                        <div className="flex size-full items-center justify-center">
                          <IconSparkles className="size-6 text-chart-1/60 dark:text-chart-3/60" />
                        </div>
                      )}
                      {/* Selection indicator */}
                      {selectedId === subject.id && (
                        <div className="absolute inset-0 flex items-center justify-center bg-chart-1/80 dark:bg-chart-3/80">
                          <IconCheck className="size-6 text-white" />
                        </div>
                      )}
                    </div>

                    {/* Info */}
                    <div className="flex-1 min-w-0">
                      <h4 className="font-semibold text-sm text-foreground line-clamp-1 group-hover:text-chart-1 dark:group-hover:text-chart-1 transition-colors">
                        {subject.name_cn || subject.name}
                      </h4>
                      {subject.name_cn && (
                        <p className="text-xs text-muted-foreground line-clamp-1 mt-0.5">
                          {subject.name}
                        </p>
                      )}
                      <div className="flex flex-wrap items-center gap-2 mt-2">
                        {/* Date */}
                        {subject.date && (
                          <span className="inline-flex items-center gap-1 text-xs text-muted-foreground">
                            <IconCalendar className="size-3" />
                            {subject.date}
                          </span>
                        )}
                        {/* Platform */}
                        {subject.platform && (
                          <span
                            className={cn(
                              "inline-flex items-center gap-1 px-2 py-0.5 rounded-full text-xs font-medium",
                              "bg-chart-1/15 text-chart-2",
                              "dark:bg-chart-1/20 dark:text-chart-1"
                            )}
                          >
                            {getPlatformIcon(subject.platform)}
                            {subject.platform}
                          </span>
                        )}
                        {/* Episodes */}
                        {subject.eps > 0 && (
                          <span className="text-xs text-muted-foreground">
                            共 {subject.eps} 集
                          </span>
                        )}
                      </div>
                    </div>

                    {/* Hover arrow */}
                    <div className="flex items-center pr-1 opacity-0 group-hover:opacity-100 transition-opacity">
                      <svg
                        className="size-5 text-chart-1 dark:text-chart-3"
                        fill="none"
                        viewBox="0 0 24 24"
                        stroke="currentColor"
                      >
                        <path
                          strokeLinecap="round"
                          strokeLinejoin="round"
                          strokeWidth={2}
                          d="M9 5l7 7-7 7"
                        />
                      </svg>
                    </div>
                  </button>
                ))}
              </div>
            )}
          </div>
        </DialogPrimitive.Popup>
      </DialogPrimitive.Portal>
    </DialogPrimitive.Root>
  );
}
