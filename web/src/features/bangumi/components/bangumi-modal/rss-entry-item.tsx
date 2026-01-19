import * as React from "react";
import { cn } from "@/lib/utils";
import { Input } from "@/components/ui/input";
import { Button } from "@/components/ui/button";
import { IconX, IconTrash, IconUsers } from "@tabler/icons-react";
import type { RssFormEntry } from "./types";

interface RssEntryItemProps {
  entry: RssFormEntry;
  onUpdate: (updatedEntry: RssFormEntry) => void;
  onRemove: () => void;
}

export function RssEntryItem({
  entry,
  onUpdate,
  onRemove,
}: RssEntryItemProps) {
  const handleUrlChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    onUpdate({ ...entry, url: e.target.value });
  };

  const handleAddFilter = (filter: string) => {
    if (filter && !entry.filters.includes(filter)) {
      onUpdate({ ...entry, filters: [...entry.filters, filter] });
    }
  };

  const handleRemoveFilter = (filterIndex: number) => {
    onUpdate({
      ...entry,
      filters: entry.filters.filter((_, fi) => fi !== filterIndex),
    });
  };

  const handleAddIncludeFilter = (filter: string) => {
    if (filter && !entry.include_filters.includes(filter)) {
      onUpdate({ ...entry, include_filters: [...entry.include_filters, filter] });
    }
  };

  const handleRemoveIncludeFilter = (filterIndex: number) => {
    onUpdate({
      ...entry,
      include_filters: entry.include_filters.filter((_, fi) => fi !== filterIndex),
    });
  };

  return (
    <div
      className={cn(
        "space-y-2 p-3 rounded-lg border bg-chart-3/5 dark:bg-chart-1/5",
        "border-chart-3/20 dark:border-chart-1/20"
      )}
    >
      {/* Header: Badges */}
      <div className="flex items-center gap-2 flex-wrap">
        {/* Subtitle Group Badge (read-only) */}
        {entry.subtitle_group && (
          <span className="shrink-0 inline-flex items-center gap-1 px-2 py-0.5 rounded-md text-xs bg-chart-3/20 dark:bg-chart-1/20 text-chart-3 dark:text-chart-1 font-medium">
            <IconUsers className="size-3" />
            {entry.subtitle_group}
          </span>
        )}
      </div>

      {/* URL and Actions */}
      <div className="flex gap-2 items-center">
        <Input
          value={entry.url}
          onChange={handleUrlChange}
          placeholder="RSS 订阅地址"
          className="flex-1"
        />
        {/* Delete Button */}
        <Button
          type="button"
          variant="outline"
          size="icon"
          onClick={onRemove}
          className="shrink-0 border-destructive/30 hover:bg-destructive/10 hover:text-destructive"
        >
          <IconTrash className="size-4" />
        </Button>
      </div>

      {/* Include Filter Tags (green theme) */}
      <div className="flex flex-wrap gap-1.5 items-center">
        <span className="text-xs text-muted-foreground shrink-0">包含:</span>
        {entry.include_filters.map((filter, filterIndex) => (
          <span
            key={filterIndex}
            className="inline-flex items-center gap-1 pl-2 pr-1.5 py-0.5 rounded-full text-xs font-medium bg-green-500/20 dark:bg-green-500/30 text-green-600 dark:text-green-400 border border-green-500/40 dark:border-green-500/50"
          >
            <code>{filter}</code>
            <button
              type="button"
              onClick={() => handleRemoveIncludeFilter(filterIndex)}
              className="flex items-center justify-center size-4 rounded-full hover:bg-green-500/30 transition-colors"
            >
              <IconX className="size-3" />
            </button>
          </span>
        ))}
        <Input
          placeholder="输入正则包含..."
          className="h-6 w-32 text-xs px-2"
          onKeyDown={(e) => {
            if (e.key === "Enter") {
              e.preventDefault();
              const input = e.currentTarget;
              const value = input.value.trim();
              if (value) {
                handleAddIncludeFilter(value);
                input.value = "";
              }
            }
          }}
        />
      </div>

      {/* Exclude Filter Tags (red theme) */}
      <div className="flex flex-wrap gap-1.5 items-center">
        <span className="text-xs text-muted-foreground shrink-0">排除:</span>
        {entry.filters.map((filter, filterIndex) => (
          <span
            key={filterIndex}
            className="inline-flex items-center gap-1 pl-2 pr-1.5 py-0.5 rounded-full text-xs font-medium bg-red-500/20 dark:bg-red-500/30 text-red-600 dark:text-red-400 border border-red-500/40 dark:border-red-500/50"
          >
            <code>{filter}</code>
            <button
              type="button"
              onClick={() => handleRemoveFilter(filterIndex)}
              className="flex items-center justify-center size-4 rounded-full hover:bg-red-500/30 transition-colors"
            >
              <IconX className="size-3" />
            </button>
          </span>
        ))}
        <Input
          placeholder="输入正则排除..."
          className="h-6 w-32 text-xs px-2"
          onKeyDown={(e) => {
            if (e.key === "Enter") {
              e.preventDefault();
              const input = e.currentTarget;
              const value = input.value.trim();
              if (value) {
                handleAddFilter(value);
                input.value = "";
              }
            }
          }}
        />
      </div>
    </div>
  );
}
