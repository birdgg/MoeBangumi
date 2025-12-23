import * as React from "react";
import { Dialog as DialogPrimitive } from "@base-ui/react/dialog";
import { cn } from "@/lib/utils";
import { type SearchResult, type Subgroup } from "@/lib/api";
import { useSearchMikan, useMikanRss } from "../hooks/use-bangumi";
import { Input } from "@/components/ui/input";
import {
  IconX,
  IconRss,
  IconLoader2,
  IconSearch,
  IconChevronLeft,
  IconUsers,
  IconCheck,
} from "@tabler/icons-react";
import { useDebouncedValue } from "@tanstack/react-pacer";

interface MikanRssModalProps {
  open: boolean;
  onOpenChange: (open: boolean) => void;
  onSelect: (rssUrl: string) => void;
  initialKeyword?: string;
}

export function MikanRssModal({
  open,
  onOpenChange,
  onSelect,
  initialKeyword = "",
}: MikanRssModalProps) {
  const [inputValue, setInputValue] = React.useState(initialKeyword);
  const [debouncedKeyword] = useDebouncedValue(inputValue, { wait: 400 });
  const [selectedBangumi, setSelectedBangumi] = React.useState<SearchResult | null>(null);

  const { data: searchResults, isLoading: isSearching, isFetching: isSearchFetching } = useSearchMikan(debouncedKeyword);
  const { data: bangumiDetail, isLoading: isLoadingDetail, isFetching: isFetchingDetail } = useMikanRss(selectedBangumi?.id ?? "");

  const isSearchingAny = isSearching || isSearchFetching;
  const isLoadingDetailAny = isLoadingDetail || isFetchingDetail;

  // Reset state when modal opens
  React.useEffect(() => {
    if (open) {
      setInputValue(initialKeyword);
      setSelectedBangumi(null);
    }
  }, [open, initialKeyword]);

  const handleSelectSubgroup = (subgroup: Subgroup) => {
    onSelect(subgroup.rss_url);
    onOpenChange(false);
  };

  const handleBack = () => {
    setSelectedBangumi(null);
  };

  return (
    <DialogPrimitive.Root open={open} onOpenChange={onOpenChange}>
      <DialogPrimitive.Portal>
        {/* Backdrop */}
        <DialogPrimitive.Backdrop
          className={cn(
            "fixed inset-0 z-[60]",
            "bg-black/20 dark:bg-black/40 backdrop-blur-sm",
            "data-[state=open]:animate-in data-[state=closed]:animate-out",
            "data-[state=closed]:fade-out-0 data-[state=open]:fade-in-0",
            "duration-200"
          )}
        />

        {/* Modal */}
        <DialogPrimitive.Popup
          className={cn(
            "fixed left-1/2 top-1/2 z-[60] -translate-x-1/2 -translate-y-1/2",
            "w-[calc(100%-2rem)] max-w-lg",
            "max-h-[80vh] overflow-hidden",
            "rounded-2xl",
            "bg-linear-to-br from-white/95 via-white/90 to-chart-1/10",
            "dark:from-zinc-900/95 dark:via-zinc-900/90 dark:to-chart-3/20",
            "border border-chart-1/30 dark:border-chart-3/30",
            "shadow-2xl shadow-chart-1/20 dark:shadow-chart-3/50",
            "backdrop-blur-xl",
            "data-[state=open]:animate-in data-[state=closed]:animate-out",
            "data-[state=closed]:fade-out-0 data-[state=open]:fade-in-0",
            "data-[state=closed]:zoom-out-95 data-[state=open]:zoom-in-95",
            "duration-300 ease-out",
            "outline-none"
          )}
        >
          {/* Decorative elements */}
          <div className="pointer-events-none absolute -right-20 -top-20 size-40 rounded-full bg-linear-to-br from-chart-1/30 to-chart-3/30 blur-3xl dark:from-chart-1/20 dark:to-chart-3/20" />
          <div className="pointer-events-none absolute -left-20 bottom-0 size-40 rounded-full bg-linear-to-br from-chart-5/30 to-chart-1/30 blur-3xl dark:from-chart-5/20 dark:to-chart-1/20" />

          {/* Header */}
          <div className="relative border-b border-chart-1/30 dark:border-chart-3/20 p-4">
            <div className="flex items-center gap-3">
              {selectedBangumi ? (
                <button
                  type="button"
                  onClick={handleBack}
                  className={cn(
                    "flex size-10 items-center justify-center rounded-xl",
                    "bg-linear-to-br from-chart-1 to-chart-3 text-white",
                    "shadow-lg shadow-chart-3/30",
                    "hover:opacity-90 transition-opacity"
                  )}
                >
                  <IconChevronLeft className="size-5" />
                </button>
              ) : (
                <div className="flex size-10 items-center justify-center rounded-xl bg-linear-to-br from-chart-1 to-chart-3 text-white shadow-lg shadow-chart-3/30">
                  <IconRss className="size-5" />
                </div>
              )}
              <div className="flex-1">
                <DialogPrimitive.Title className="text-lg font-bold bg-linear-to-r from-chart-1 via-chart-3 to-chart-5 bg-clip-text text-transparent">
                  {selectedBangumi ? selectedBangumi.name : "Mikan RSS 搜索"}
                </DialogPrimitive.Title>
                <DialogPrimitive.Description className="text-xs text-muted-foreground">
                  {selectedBangumi ? "选择字幕组获取 RSS 订阅" : "搜索番剧并选择字幕组 RSS"}
                </DialogPrimitive.Description>
              </div>
              <DialogPrimitive.Close
                className={cn(
                  "flex size-8 items-center justify-center rounded-lg",
                  "text-muted-foreground hover:text-foreground",
                  "hover:bg-chart-1/20 dark:hover:bg-chart-3/30",
                  "transition-colors duration-200",
                  "outline-none focus-visible:ring-2 focus-visible:ring-chart-1 dark:focus-visible:ring-chart-3"
                )}
              >
                <IconX className="size-4" />
              </DialogPrimitive.Close>
            </div>
          </div>

          {/* Content */}
          <div className="relative flex flex-col max-h-[calc(80vh-80px)]">
            {!selectedBangumi ? (
              <>
                {/* Search Input */}
                <div className="p-4 border-b border-chart-1/20 dark:border-chart-3/20">
                  <div className="relative">
                    <IconSearch className="absolute left-3 top-1/2 -translate-y-1/2 size-4 text-muted-foreground" />
                    <Input
                      value={inputValue}
                      onChange={(e) => setInputValue(e.target.value)}
                      placeholder="输入番剧名称搜索..."
                      className="pl-10"
                    />
                    {isSearchingAny && (
                      <div className="absolute right-3 top-1/2 -translate-y-1/2">
                        <IconLoader2 className="size-4 animate-spin text-chart-1 dark:text-chart-3" />
                      </div>
                    )}
                  </div>
                </div>

                {/* Search Results */}
                <div className="flex-1 overflow-y-auto p-4 space-y-2 [&::-webkit-scrollbar]:hidden [-ms-overflow-style:none] [scrollbar-width:none]">
                  {isSearchingAny && !searchResults ? (
                    <div className="flex items-center justify-center py-8">
                      <IconLoader2 className="size-6 animate-spin text-chart-1 dark:text-chart-3" />
                    </div>
                  ) : searchResults && searchResults.length > 0 ? (
                    searchResults.map((result) => (
                      <button
                        key={result.id}
                        type="button"
                        onClick={() => setSelectedBangumi(result)}
                        className={cn(
                          "w-full text-left p-3 rounded-xl",
                          "bg-linear-to-r from-chart-1/5 to-chart-3/5",
                          "dark:from-chart-1/10 dark:to-chart-3/10",
                          "border border-transparent",
                          "hover:border-chart-1/30 dark:hover:border-chart-3/30",
                          "hover:from-chart-1/10 hover:to-chart-3/10",
                          "dark:hover:from-chart-1/20 dark:hover:to-chart-3/20",
                          "transition-all duration-200"
                        )}
                      >
                        <div className="font-medium truncate">{result.name}</div>
                        <div className="text-xs text-muted-foreground mt-1">
                          ID: {result.id}
                        </div>
                      </button>
                    ))
                  ) : debouncedKeyword ? (
                    <div className="text-center py-8 text-muted-foreground">
                      未找到结果
                    </div>
                  ) : (
                    <div className="text-center py-8 text-muted-foreground">
                      输入关键词开始搜索
                    </div>
                  )}
                </div>
              </>
            ) : (
              /* Subgroup List */
              <div className="flex-1 overflow-y-auto p-4 space-y-2 [&::-webkit-scrollbar]:hidden [-ms-overflow-style:none] [scrollbar-width:none]">
                {isLoadingDetailAny ? (
                  <div className="flex items-center justify-center py-8">
                    <IconLoader2 className="size-6 animate-spin text-chart-1 dark:text-chart-3" />
                  </div>
                ) : bangumiDetail && bangumiDetail.subgroups.length > 0 ? (
                  bangumiDetail.subgroups.map((subgroup) => (
                    <button
                      key={subgroup.id}
                      type="button"
                      onClick={() => handleSelectSubgroup(subgroup)}
                      className={cn(
                        "w-full text-left p-4 rounded-xl",
                        "bg-linear-to-r from-chart-1/5 to-chart-3/5",
                        "dark:from-chart-1/10 dark:to-chart-3/10",
                        "border border-transparent",
                        "hover:border-chart-1/30 dark:hover:border-chart-3/30",
                        "hover:from-chart-1/10 hover:to-chart-3/10",
                        "dark:hover:from-chart-1/20 dark:hover:to-chart-3/20",
                        "transition-all duration-200",
                        "group"
                      )}
                    >
                      <div className="flex items-start gap-3">
                        <div className="flex size-10 shrink-0 items-center justify-center rounded-lg bg-chart-1/20 dark:bg-chart-3/30">
                          <IconUsers className="size-5 text-chart-1 dark:text-chart-3" />
                        </div>
                        <div className="flex-1 min-w-0">
                          <div className="font-medium truncate">{subgroup.name || "未知字幕组"}</div>
                          <div className="text-xs text-muted-foreground mt-1">
                            {subgroup.episodes.length} 个资源
                          </div>
                        </div>
                        <div className="flex size-8 shrink-0 items-center justify-center rounded-lg opacity-0 group-hover:opacity-100 bg-chart-1/20 dark:bg-chart-3/30 transition-opacity">
                          <IconCheck className="size-4 text-chart-1 dark:text-chart-3" />
                        </div>
                      </div>
                    </button>
                  ))
                ) : (
                  <div className="text-center py-8 text-muted-foreground">
                    暂无字幕组资源
                  </div>
                )}
              </div>
            )}
          </div>
        </DialogPrimitive.Popup>
      </DialogPrimitive.Portal>
    </DialogPrimitive.Root>
  );
}
