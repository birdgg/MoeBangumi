import * as React from "react";
import { Dialog as DialogPrimitive } from "@base-ui/react/dialog";
import { cn } from "@/lib/utils";
import { type SearchResult } from "@/lib/api";
import { useSearchMikan, useMikanRss } from "../hooks/use-bangumi";
import { Input } from "@/components/ui/input";
import { Button } from "@/components/ui/button";
import {
  IconX,
  IconRss,
  IconLoader2,
  IconSearch,
  IconChevronLeft,
  IconChevronDown,
  IconUsers,
  IconCheck,
} from "@tabler/icons-react";
import { useDebouncedValue } from "@tanstack/react-pacer";

interface RssSelectionEntry {
  url: string;
  group: string | null;
  filters: string[];
  include_filters: string[];
}

interface MikanRssModalProps {
  open: boolean;
  onOpenChange: (open: boolean) => void;
  onSelect: (entries: RssSelectionEntry[]) => void;
  initialKeyword?: string;
  mikanId?: string;
}

export function MikanRssModal({
  open,
  onOpenChange,
  onSelect,
  initialKeyword = "",
  mikanId,
}: MikanRssModalProps) {
  // Direct mode: when mikanId is provided, skip search and show subgroups directly
  const isDirectMode = !!mikanId;
  const [inputValue, setInputValue] = React.useState(initialKeyword);
  const [debouncedKeyword] = useDebouncedValue(inputValue, { wait: 400 });
  const [selectedBangumi, setSelectedBangumi] = React.useState<SearchResult | null>(null);
  const [expandedSubgroup, setExpandedSubgroup] = React.useState<string | null>(null);
  const [selectedSubgroups, setSelectedSubgroups] = React.useState<Set<string>>(new Set());
  const hasAutoSelected = React.useRef(false);

  const { data: searchResults, isLoading: isSearching, isFetching: isSearchFetching } = useSearchMikan(isDirectMode ? "" : debouncedKeyword);
  const { data: bangumiDetail, isLoading: isLoadingDetail, isFetching: isFetchingDetail } = useMikanRss(isDirectMode ? mikanId : (selectedBangumi?.id ?? ""));

  const isSearchingAny = isSearching || isSearchFetching;
  const isLoadingDetailAny = isLoadingDetail || isFetchingDetail;


  // Auto-select when there's only one search result (only once per modal open)
  React.useEffect(() => {
    if (searchResults && searchResults.length === 1 && !selectedBangumi && !hasAutoSelected.current) {
      setSelectedBangumi(searchResults[0]);
      hasAutoSelected.current = true;
    }
  }, [searchResults, selectedBangumi]);

  // Reset state when modal opens
  React.useEffect(() => {
    if (open) {
      setInputValue(initialKeyword);
      setSelectedBangumi(null);
      setExpandedSubgroup(null);
      setSelectedSubgroups(new Set());
      hasAutoSelected.current = false;
    }
  }, [open, initialKeyword]);

  const toggleSubgroup = (subgroupId: string) => {
    setSelectedSubgroups((prev) => {
      const next = new Set(prev);
      if (next.has(subgroupId)) {
        next.delete(subgroupId);
      } else {
        next.add(subgroupId);
      }
      return next;
    });
  };

  const handleConfirm = () => {
    if (!bangumiDetail) return;
    const selectedEntries = bangumiDetail.subgroups
      .filter((sg) => selectedSubgroups.has(sg.id))
      .map((sg) => ({
        url: sg.rss_url,
        group: sg.name || null,
        filters: [],
        include_filters: [],
      }));
    onSelect(selectedEntries);
    onOpenChange(false);
  };

  const handleBack = () => {
    setSelectedBangumi(null);
    setSelectedSubgroups(new Set());
  };

  return (
    <DialogPrimitive.Root open={open} onOpenChange={onOpenChange}>
      <DialogPrimitive.Portal>
        {/* Backdrop */}
        <DialogPrimitive.Backdrop
          className={cn(
            "fixed inset-0 z-60",
            "bg-black/20 dark:bg-black/40 backdrop-blur-sm",
            "data-[state=open]:animate-in data-[state=closed]:animate-out",
            "data-[state=closed]:fade-out-0 data-[state=open]:fade-in-0",
            "duration-200"
          )}
        />

        {/* Modal */}
        <DialogPrimitive.Popup
          className={cn(
            "fixed left-1/2 top-1/2 z-60 -translate-x-1/2 -translate-y-1/2",
            "w-[calc(100%-2rem)] max-w-2xl",
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
            "data-[state=closed]:slide-out-to-right-8 data-[state=open]:slide-in-from-right-8",
            "duration-300 ease-out",
            "outline-none"
          )}
        >
          {/* Decorative elements - Top right glow */}
          <div className="pointer-events-none absolute -right-20 -top-20 size-40 rounded-full bg-linear-to-br from-chart-1/30 to-chart-3/30 blur-3xl dark:from-chart-1/20 dark:to-chart-3/20" />

          {/* Bottom backlight - subtle glow */}
          <div className="pointer-events-none absolute -bottom-20 left-1/2 -translate-x-1/2 w-full h-20 -z-10">
            <div className="absolute inset-0 bg-linear-to-t from-chart-1/25 via-chart-3/10 to-transparent blur-2xl dark:from-chart-1/20 dark:via-chart-3/10" />
            <div className="absolute inset-x-[25%] inset-y-0 bg-linear-to-t from-chart-3/30 to-transparent blur-xl dark:from-chart-3/25 animate-pulse animation-duration-[4s]" />
          </div>

          {/* Header */}
          <div className="relative border-b border-chart-1/30 dark:border-chart-3/20 p-4">
            <div className="flex items-center gap-3">
              {selectedBangumi && !isDirectMode ? (
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
              <div className="flex-1 min-w-0">
                <DialogPrimitive.Title className="text-lg font-bold bg-linear-to-r from-chart-1 via-chart-3 to-chart-5 bg-clip-text text-transparent truncate">
                  {isDirectMode ? initialKeyword || "选择字幕组" : (selectedBangumi ? selectedBangumi.name : "Mikan RSS 搜索")}
                </DialogPrimitive.Title>
                <DialogPrimitive.Description className="text-xs text-muted-foreground">
                  {isDirectMode || selectedBangumi ? "选择字幕组" : "搜索番剧并选择字幕组 RSS"}
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
            {!isDirectMode && !selectedBangumi ? (
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
              <>
                <div className="flex-1 overflow-y-auto p-4 space-y-2 [&::-webkit-scrollbar]:hidden [-ms-overflow-style:none] [scrollbar-width:none]">
                  {isLoadingDetailAny ? (
                    <div className="flex items-center justify-center py-8">
                      <IconLoader2 className="size-6 animate-spin text-chart-1 dark:text-chart-3" />
                    </div>
                  ) : bangumiDetail && bangumiDetail.subgroups.length > 0 ? (
                    bangumiDetail.subgroups.map((subgroup) => {
                      const isExpanded = expandedSubgroup === subgroup.id;
                      return (
                        <div
                          key={subgroup.id}
                          className={cn(
                            "rounded-xl overflow-hidden",
                            "bg-linear-to-r from-chart-1/5 to-chart-3/5",
                            "dark:from-chart-1/10 dark:to-chart-3/10",
                            "border",
                            isExpanded
                              ? "border-chart-1/30 dark:border-chart-3/30"
                              : "border-transparent hover:border-chart-1/30 dark:hover:border-chart-3/30",
                            "transition-all duration-200"
                          )}
                        >
                          {/* Subgroup Header */}
                          <div className="flex items-center gap-3 p-4">
                            <button
                              type="button"
                              onClick={() => setExpandedSubgroup(isExpanded ? null : subgroup.id)}
                              className="flex items-center gap-3 flex-1 min-w-0 text-left"
                            >
                              <div className="flex size-10 shrink-0 items-center justify-center rounded-lg bg-chart-1/20 dark:bg-chart-3/30">
                                <IconUsers className="size-5 text-chart-1 dark:text-chart-3" />
                              </div>
                              <div className="flex-1 min-w-0">
                                <div className="font-medium truncate">{subgroup.name || "未知字幕组"}</div>
                                <div className="text-xs text-muted-foreground mt-1">
                                  {subgroup.episodes.length} 个资源
                                </div>
                              </div>
                              <IconChevronDown
                                className={cn(
                                  "size-5 text-muted-foreground transition-transform duration-200",
                                  isExpanded && "rotate-180"
                                )}
                              />
                            </button>
                            <button
                              type="button"
                              onClick={() => toggleSubgroup(subgroup.id)}
                              className={cn(
                                "flex size-8 shrink-0 items-center justify-center rounded-lg",
                                "transition-all border-2",
                                selectedSubgroups.has(subgroup.id)
                                  ? "bg-chart-1 dark:bg-chart-3 text-white border-transparent"
                                  : "border-muted-foreground/30 text-muted-foreground/50 hover:border-chart-1/50 hover:text-chart-1 dark:hover:border-chart-3/50 dark:hover:text-chart-3"
                              )}
                              title={selectedSubgroups.has(subgroup.id) ? "取消选择" : "选择此字幕组"}
                            >
                              <IconCheck className="size-4" />
                            </button>
                          </div>

                          {/* Episodes List */}
                          {isExpanded && subgroup.episodes.length > 0 && (
                            <div className="border-t border-chart-1/20 dark:border-chart-3/20 px-4 py-2 space-y-1.5 max-h-64 overflow-y-auto [&::-webkit-scrollbar]:hidden [-ms-overflow-style:none] [scrollbar-width:none]">
                              {subgroup.episodes.map((episode, index) => (
                                <div
                                  key={index}
                                  className={cn(
                                    "px-2 py-1.5 rounded-lg text-xs",
                                    "bg-white/50 dark:bg-zinc-800/50",
                                    "border border-chart-1/10 dark:border-chart-3/10",
                                    "text-muted-foreground line-clamp-2"
                                  )}
                                >
                                  {episode.name}
                                </div>
                              ))}
                            </div>
                          )}
                        </div>
                      );
                    })
                  ) : (
                    <div className="text-center py-8 text-muted-foreground">
                      暂无字幕组资源
                    </div>
                  )}
                </div>


                {/* Footer with confirm button */}
                <div className="border-t border-chart-1/30 dark:border-chart-3/20 p-4 bg-linear-to-br from-white/95 via-white/90 to-chart-1/10 dark:from-zinc-900/95 dark:via-zinc-900/90 dark:to-chart-3/20">
                  <Button
                    onClick={handleConfirm}
                    disabled={selectedSubgroups.size === 0}
                    className={cn(
                      "w-full gap-2 bg-linear-to-r from-chart-1 to-chart-3 text-white",
                      "shadow-lg shadow-chart-3/30",
                      "hover:opacity-90",
                      "disabled:opacity-50 disabled:cursor-not-allowed"
                    )}
                  >
                    <IconCheck className="size-4" />
                    {selectedSubgroups.size > 0
                      ? `确认选择 (${selectedSubgroups.size} 个字幕组)`
                      : "请选择字幕组"}
                  </Button>
                </div>
              </>
            )}
          </div>
        </DialogPrimitive.Popup>
      </DialogPrimitive.Portal>
    </DialogPrimitive.Root>
  );
}
