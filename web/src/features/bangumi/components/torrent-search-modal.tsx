import * as React from "react";
import { Dialog as DialogPrimitive } from "@base-ui/react/dialog";
import { useQuery } from "@tanstack/react-query";
import { useDebouncedValue } from "@tanstack/react-pacer";
import { AnimatePresence, motion } from "framer-motion";
import { cn } from "@/lib/utils";
import {
  searchTorrentsOptions,
  type TorrentSearchResult,
  type TorrentSource,
} from "@/lib/api";
import {
  IconX,
  IconSearch,
  IconDownload,
  IconLoader2,
  IconAlertCircle,
} from "@tabler/icons-react";

interface TorrentSearchModalProps {
  open: boolean;
  onOpenChange: (open: boolean) => void;
  onSelect: (torrentUrl: string) => void;
  initialKeyword?: string;
}

// Default tag added when modal opens
const DEFAULT_TAG = "bdrip";

// Predefined quick tags
const QUICK_TAGS = [DEFAULT_TAG, "VCB-Studio"] as const;

// Available sources
const SOURCES: { value: TorrentSource; label: string }[] = [
  { value: "nyaa", label: "Nyaa" },
  { value: "mikan", label: "Mikan" },
];

// Source tab component
function SourceTabs({
  value,
  onChange,
}: {
  value: TorrentSource;
  onChange: (source: TorrentSource) => void;
}) {
  return (
    <div className="flex items-center gap-1 p-1 rounded-lg bg-muted/50 shrink-0">
      {SOURCES.map((source) => (
        <button
          key={source.value}
          type="button"
          onClick={() => onChange(source.value)}
          className={cn(
            "relative px-4 py-1.5 rounded-md text-sm font-medium transition-all duration-200",
            value === source.value
              ? "text-foreground"
              : "text-muted-foreground hover:text-foreground/80"
          )}
        >
          {value === source.value && (
            <motion.div
              layoutId="source-tab-indicator"
              className={cn(
                "absolute inset-0 rounded-md",
                "bg-white dark:bg-zinc-800",
                "border border-chart-1/20 dark:border-chart-3/20",
                "shadow-sm"
              )}
              transition={{ type: "spring", damping: 25, stiffness: 300 }}
            />
          )}
          <span className="relative z-10">{source.label}</span>
        </button>
      ))}
    </div>
  );
}

// Input tag chip component (removable)
function InputTag({
  label,
  onRemove,
}: {
  label: string;
  onRemove: () => void;
}) {
  return (
    <span
      className={cn(
        "inline-flex items-center gap-1 pl-2.5 pr-1.5 py-1 rounded-full text-xs font-medium",
        "bg-chart-1/20 dark:bg-chart-3/20 text-chart-1 dark:text-chart-3",
        "border border-chart-1/30 dark:border-chart-3/30"
      )}
    >
      {label}
      <button
        type="button"
        onClick={(e) => {
          e.stopPropagation();
          onRemove();
        }}
        className="flex items-center justify-center size-4 rounded-full hover:bg-chart-1/30 dark:hover:bg-chart-3/30 transition-colors"
      >
        <IconX className="size-3" />
      </button>
    </span>
  );
}

// Quick tag button component
function QuickTagButton({ tag, onClick }: { tag: string; onClick: () => void }) {
  return (
    <button
      type="button"
      onClick={onClick}
      className={cn(
        "inline-flex items-center px-2.5 py-1 rounded-full text-xs font-medium",
        "border transition-all duration-200",
        "bg-white/50 dark:bg-zinc-800/50 text-muted-foreground",
        "border-chart-1/30 dark:border-chart-3/30",
        "hover:border-chart-1/50 dark:hover:border-chart-3/50",
        "hover:bg-chart-1/10 dark:hover:bg-chart-3/10"
      )}
    >
      {tag}
    </button>
  );
}

// Source badge component
function SourceBadge({ source }: { source: "mikan" | "nyaa" }) {
  const config = {
    mikan: {
      label: "Mikan",
      className: "bg-chart-1/20 text-chart-1 dark:bg-chart-1/30",
    },
    nyaa: {
      label: "Nyaa",
      className: "bg-chart-3/20 text-chart-3 dark:bg-chart-3/30",
    },
  };

  const { label, className } = config[source];

  return (
    <span
      className={cn(
        "shrink-0 rounded-full px-2 py-0.5 text-xs font-medium",
        className
      )}
    >
      {label}
    </span>
  );
}

// Torrent item component
function TorrentItem({
  item,
  onSelect,
  index,
}: {
  item: TorrentSearchResult;
  onSelect: (torrentUrl: string) => void;
  index: number;
}) {
  return (
    <motion.button
      initial={{ opacity: 0, y: 10 }}
      animate={{ opacity: 1, y: 0 }}
      transition={{ delay: index * 0.03, duration: 0.2 }}
      onClick={() => onSelect(item.torrent_url)}
      className={cn(
        "w-full text-left p-3 rounded-xl",
        "bg-white/50 dark:bg-zinc-800/50",
        "border border-chart-1/20 dark:border-chart-3/20",
        "hover:bg-chart-1/10 dark:hover:bg-chart-3/10",
        "hover:border-chart-1/40 dark:hover:border-chart-3/40",
        "hover:shadow-md hover:shadow-chart-1/10 dark:hover:shadow-chart-3/10",
        "transition-all duration-200",
        "group cursor-pointer"
      )}
    >
      <div className="flex items-start gap-3">
        <div className="flex-1 min-w-0">
          <p className="text-sm font-medium text-foreground line-clamp-2 group-hover:text-chart-1 dark:group-hover:text-chart-3 transition-colors">
            {item.title}
          </p>
          <div className="flex items-center gap-2 mt-1.5">
            <SourceBadge source={item.source} />
            <span className="text-xs text-muted-foreground/70 font-mono truncate">
              {item.info_hash.slice(0, 8)}...
            </span>
          </div>
        </div>
        <div className="shrink-0 flex items-center justify-center size-8 rounded-lg bg-chart-1/10 dark:bg-chart-3/10 group-hover:bg-chart-1 dark:group-hover:bg-chart-3 transition-colors">
          <IconDownload className="size-4 text-chart-1 dark:text-chart-3 group-hover:text-white transition-colors" />
        </div>
      </div>
    </motion.button>
  );
}

export function TorrentSearchModal({
  open,
  onOpenChange,
  onSelect,
  initialKeyword = "",
}: TorrentSearchModalProps) {
  const [tags, setTags] = React.useState<string[]>([]);
  const [inputValue, setInputValue] = React.useState("");
  const [source, setSource] = React.useState<TorrentSource>("nyaa");
  const inputRef = React.useRef<HTMLInputElement>(null);
  const containerRef = React.useRef<HTMLDivElement>(null);

  // Build full search keyword from all tags
  const fullKeyword = React.useMemo(() => {
    return tags.join(" ");
  }, [tags]);

  // Debounce keyword for API call
  const [debouncedKeyword] = useDebouncedValue(fullKeyword, { wait: 500 });

  // Search query
  const {
    data: results,
    isLoading,
    isError,
    error,
  } = useQuery({
    ...searchTorrentsOptions({
      query: { keyword: debouncedKeyword, source },
    }),
    enabled: debouncedKeyword.length > 0,
  });

  // Initialize tags when modal opens
  React.useEffect(() => {
    if (open) {
      const initialTags = initialKeyword.trim()
        ? [initialKeyword.trim(), DEFAULT_TAG]
        : [DEFAULT_TAG];
      setTags(initialTags);
      setInputValue("");
      setSource("nyaa"); // Reset to default source
      setTimeout(() => inputRef.current?.focus(), 100);
    }
  }, [open, initialKeyword]);

  // Add a tag
  const addTag = (tag: string) => {
    const trimmed = tag.trim();
    if (trimmed && !tags.includes(trimmed)) {
      setTags((prev) => [...prev, trimmed]);
    }
  };

  // Remove a tag by index
  const removeTag = (index: number) => {
    setTags((prev) => prev.filter((_, i) => i !== index));
  };

  // Handle input keydown
  const handleKeyDown = (e: React.KeyboardEvent<HTMLInputElement>) => {
    if (e.key === "Enter") {
      e.preventDefault();
      if (inputValue.trim()) {
        addTag(inputValue);
        setInputValue("");
      } else if (results && results.length > 0) {
        onSelect(results[0].torrent_url);
        onOpenChange(false);
      }
    } else if (e.key === "Backspace" && !inputValue && tags.length > 0) {
      // Remove last tag when backspace on empty input
      removeTag(tags.length - 1);
    }
  };

  // Handle select
  const handleSelect = (torrentUrl: string) => {
    onSelect(torrentUrl);
    onOpenChange(false);
  };

  // Focus input when clicking container
  const handleContainerClick = () => {
    inputRef.current?.focus();
  };

  return (
    <DialogPrimitive.Root open={open} onOpenChange={onOpenChange}>
      <AnimatePresence>
        {open && (
          <DialogPrimitive.Portal keepMounted>
            {/* Backdrop */}
            <DialogPrimitive.Backdrop
              render={
                <motion.div
                  initial={{ opacity: 0 }}
                  animate={{ opacity: 1 }}
                  exit={{ opacity: 0 }}
                  transition={{ duration: 0.2 }}
                  className={cn(
                    "fixed inset-0 z-[60]",
                    "bg-black/20 dark:bg-black/40 backdrop-blur-sm"
                  )}
                />
              }
            />

            {/* Modal */}
            <DialogPrimitive.Popup
              render={
                <motion.div
                  initial={{ opacity: 0, scale: 0.95, x: "calc(-50% + 32px)", y: "-50%" }}
                  animate={{ opacity: 1, scale: 1, x: "-50%", y: "-50%" }}
                  exit={{ opacity: 0, scale: 0.95, x: "calc(-50% + 32px)", y: "-50%" }}
                  transition={{
                    type: "spring",
                    damping: 25,
                    stiffness: 300,
                  }}
                  className={cn(
                    "fixed left-1/2 top-1/2 z-[60]",
                    "w-[calc(100%-2rem)] max-w-xl",
                    "max-h-[80vh] overflow-hidden flex flex-col",
                    "rounded-2xl",
                    "bg-linear-to-br from-white/95 via-white/90 to-chart-1/10",
                    "dark:from-zinc-900/95 dark:via-zinc-900/90 dark:to-chart-3/20",
                    "border border-chart-1/30 dark:border-chart-3/30",
                    "shadow-2xl shadow-chart-1/20 dark:shadow-chart-3/50",
                    "backdrop-blur-xl",
                    "outline-none"
                  )}
                />
              }
            >
              {/* Decorative glow */}
              <motion.div
                initial={{ opacity: 0, scale: 0.8 }}
                animate={{ opacity: 1, scale: 1 }}
                transition={{ delay: 0.1, duration: 0.3 }}
                className="pointer-events-none absolute -right-16 -top-16 size-32 rounded-full bg-linear-to-br from-chart-1/30 to-chart-3/30 blur-3xl dark:from-chart-1/20 dark:to-chart-3/20"
              />

              {/* Header */}
              <div className="relative border-b border-chart-1/30 dark:border-chart-3/20 px-4 py-3 shrink-0">
                <div className="flex items-center gap-2.5">
                  <motion.div
                    initial={{ scale: 0, rotate: -180 }}
                    animate={{ scale: 1, rotate: 0 }}
                    transition={{ type: "spring", damping: 15, stiffness: 300, delay: 0.1 }}
                    className="flex size-8 items-center justify-center rounded-lg bg-linear-to-br from-chart-1 to-chart-3 text-white shadow-md shadow-chart-1/30"
                  >
                    <IconSearch className="size-4" />
                  </motion.div>
                  <DialogPrimitive.Title className="flex-1 text-base font-semibold bg-linear-to-r from-chart-1 via-chart-3 to-chart-5 bg-clip-text text-transparent">
                    搜索种子
                  </DialogPrimitive.Title>
                  <DialogPrimitive.Close
                    className={cn(
                      "flex size-7 items-center justify-center rounded-md",
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
              <motion.div
                initial={{ opacity: 0, y: 10 }}
                animate={{ opacity: 1, y: 0 }}
                transition={{ delay: 0.15, duration: 0.2 }}
                className="relative p-4 space-y-3 flex-1 overflow-hidden flex flex-col"
              >
                {/* Tag Input Container */}
                <div
                  ref={containerRef}
                  onClick={handleContainerClick}
                  className={cn(
                    "flex flex-wrap items-center gap-1.5 p-2 min-h-[42px] shrink-0",
                    "rounded-lg border border-input bg-background",
                    "focus-within:ring-2 focus-within:ring-chart-1/50 dark:focus-within:ring-chart-3/50",
                    "cursor-text transition-all"
                  )}
                >
                  <IconSearch className="size-4 text-muted-foreground shrink-0 ml-1" />
                  {tags.map((tag, index) => (
                    <InputTag
                      key={`${tag}-${index}`}
                      label={tag}
                      onRemove={() => removeTag(index)}
                    />
                  ))}
                  <input
                    ref={inputRef}
                    type="text"
                    value={inputValue}
                    onChange={(e) => setInputValue(e.target.value)}
                    onKeyDown={handleKeyDown}
                    placeholder={tags.length === 0 ? "输入关键词，按 Enter 添加..." : "继续添加..."}
                    className={cn(
                      "flex-1 min-w-[120px] bg-transparent text-sm",
                      "placeholder:text-muted-foreground/60",
                      "outline-none border-none"
                    )}
                  />
                </div>

                {/* Source Tabs and Quick Tags */}
                <div className="flex items-center justify-between gap-3 shrink-0">
                  <SourceTabs value={source} onChange={setSource} />
                  <div className="flex items-center gap-2">
                    <span className="text-xs text-muted-foreground">快捷:</span>
                    {QUICK_TAGS.map((tag) => (
                      <QuickTagButton
                        key={tag}
                        tag={tag}
                        onClick={() => addTag(tag)}
                      />
                    ))}
                  </div>
                </div>

                {/* Results area */}
                <div className="flex-1 overflow-y-auto min-h-0">
                  {/* Empty state */}
                  {!debouncedKeyword && (
                    <motion.div
                      initial={{ opacity: 0, scale: 0.9 }}
                      animate={{ opacity: 1, scale: 1 }}
                      transition={{ delay: 0.2, type: "spring", damping: 20 }}
                      className="flex flex-col items-center justify-center py-12 text-muted-foreground"
                    >
                      <div className="flex size-16 items-center justify-center rounded-full bg-chart-1/10 dark:bg-chart-3/10 mb-4">
                        <IconDownload className="size-8 text-chart-1 dark:text-chart-3" />
                      </div>
                      <p className="text-sm font-medium">
                        搜索 {source === "nyaa" ? "Nyaa" : "Mikan"} 种子
                      </p>
                      <p className="text-xs text-muted-foreground/70 mt-1">
                        添加关键词开始搜索
                      </p>
                    </motion.div>
                  )}

                  {/* Loading state */}
                  {debouncedKeyword && isLoading && (
                    <motion.div
                      initial={{ opacity: 0 }}
                      animate={{ opacity: 1 }}
                      className="flex flex-col items-center justify-center py-12"
                    >
                      <IconLoader2 className="size-8 text-chart-1 dark:text-chart-3 animate-spin" />
                      <p className="text-sm text-muted-foreground mt-3">正在搜索...</p>
                    </motion.div>
                  )}

                  {/* Error state */}
                  {debouncedKeyword && isError && (
                    <motion.div
                      initial={{ opacity: 0, scale: 0.9 }}
                      animate={{ opacity: 1, scale: 1 }}
                      className="flex flex-col items-center justify-center py-12 text-muted-foreground"
                    >
                      <div className="flex size-16 items-center justify-center rounded-full bg-red-500/10 mb-4">
                        <IconAlertCircle className="size-8 text-red-500" />
                      </div>
                      <p className="text-sm font-medium text-red-500">搜索失败</p>
                      <p className="text-xs text-muted-foreground/70 mt-1">
                        {error?.message || "请稍后再试"}
                      </p>
                    </motion.div>
                  )}

                  {/* No results */}
                  {debouncedKeyword && !isLoading && !isError && results?.length === 0 && (
                    <motion.div
                      initial={{ opacity: 0, scale: 0.9 }}
                      animate={{ opacity: 1, scale: 1 }}
                      className="flex flex-col items-center justify-center py-12 text-muted-foreground"
                    >
                      <div className="flex size-16 items-center justify-center rounded-full bg-chart-1/10 dark:bg-chart-3/10 mb-4">
                        <IconSearch className="size-8 text-chart-1 dark:text-chart-3" />
                      </div>
                      <p className="text-sm font-medium">未找到结果</p>
                      <p className="text-xs text-muted-foreground/70 mt-1">
                        尝试使用不同的关键词
                      </p>
                    </motion.div>
                  )}

                  {/* Results list */}
                  {debouncedKeyword && !isLoading && !isError && results && results.length > 0 && (
                    <div className="space-y-2">
                      {results.map((item, index) => (
                        <TorrentItem
                          key={`${item.source}-${item.info_hash}`}
                          item={item}
                          onSelect={handleSelect}
                          index={index}
                        />
                      ))}
                    </div>
                  )}
                </div>

                {/* Results count */}
                {debouncedKeyword && !isLoading && !isError && results && results.length > 0 && (
                  <motion.div
                    initial={{ opacity: 0 }}
                    animate={{ opacity: 1 }}
                    className="shrink-0 pt-2 border-t border-chart-1/20 dark:border-chart-3/20"
                  >
                    <p className="text-xs text-muted-foreground text-center">
                      找到 <span className="font-medium text-chart-1 dark:text-chart-3">{results.length}</span> 个结果
                    </p>
                  </motion.div>
                )}
              </motion.div>
            </DialogPrimitive.Popup>
          </DialogPrimitive.Portal>
        )}
      </AnimatePresence>
    </DialogPrimitive.Root>
  );
}
