import * as React from "react";
import { cn } from "@/lib/utils";
import { Input } from "@/components/ui/input";
import { Button } from "@/components/ui/button";
import { IconPlus, IconTrash, IconGripVertical } from "@tabler/icons-react";
import { type SettingsFormInstance } from "../hooks";
import { Reorder, useDragControls } from "framer-motion";

export interface PrioritySectionProps {
  form: SettingsFormInstance;
}

interface PriorityListProps {
  title: string;
  description: string;
  icon: string;
  items: string[];
  placeholder: string;
  suggestions?: string[];
  onAdd: (item: string) => void;
  onRemove: (index: number) => void;
  onReorder: (items: string[]) => void;
}

interface ReorderItemProps {
  item: string;
  index: number;
  onRemove: () => void;
}

function ReorderItem({ item, index, onRemove }: ReorderItemProps) {
  const controls = useDragControls();

  return (
    <Reorder.Item
      value={item}
      dragListener={false}
      dragControls={controls}
      className="group flex items-center gap-2 rounded-lg border border-border/50 bg-muted/30 px-2 py-1.5 data-[dragging]:z-50 data-[dragging]:border-chart-1/50 data-[dragging]:bg-muted/80 data-[dragging]:shadow-lg"
    >
      {/* Rank badge */}
      <span
        className={cn(
          "flex size-5 shrink-0 items-center justify-center rounded text-xs font-medium",
          index === 0
            ? "bg-chart-1/20 text-chart-1"
            : index === 1
              ? "bg-chart-3/20 text-chart-3"
              : index === 2
                ? "bg-chart-5/20 text-chart-5"
                : "bg-muted text-muted-foreground"
        )}
      >
        {index + 1}
      </span>

      {/* Drag handle */}
      <button
        type="button"
        className="cursor-grab touch-none text-muted-foreground/50 hover:text-muted-foreground active:cursor-grabbing"
        onPointerDown={(e) => controls.start(e)}
      >
        <IconGripVertical className="size-4 shrink-0" />
      </button>

      {/* Item text */}
      <span className="flex-1 truncate text-sm">{item}</span>

      {/* Delete button */}
      <Button
        type="button"
        variant="ghost"
        size="icon-sm"
        onClick={onRemove}
        className="size-6 shrink-0 text-muted-foreground opacity-0 transition-opacity hover:bg-destructive/10 hover:text-destructive group-hover:opacity-100"
      >
        <IconTrash className="size-3.5" />
      </Button>
    </Reorder.Item>
  );
}

function PriorityList({
  title,
  description,
  icon,
  items,
  placeholder,
  suggestions,
  onAdd,
  onRemove,
  onReorder,
}: PriorityListProps) {
  const [newItem, setNewItem] = React.useState("");
  const [localItems, setLocalItems] = React.useState(items);

  // Sync local state with props when items change externally
  React.useEffect(() => {
    setLocalItems(items);
  }, [items]);

  const handleReorder = (newItems: string[]) => {
    setLocalItems(newItems); // Update local state immediately for smooth animation
    onReorder(newItems); // Update form state
  };

  const handleAdd = () => {
    const trimmed = newItem.trim();
    if (!trimmed || items.includes(trimmed)) return;
    onAdd(trimmed);
    setNewItem("");
  };

  const handleKeyDown = (e: React.KeyboardEvent) => {
    if (e.key === "Enter") {
      e.preventDefault();
      handleAdd();
    }
  };

  const handleSuggestionClick = (suggestion: string) => {
    if (!items.includes(suggestion)) {
      onAdd(suggestion);
    }
  };

  return (
    <div className="space-y-3">
      {/* Header */}
      <div className="flex items-start gap-2">
        <span className="text-base">{icon}</span>
        <div>
          <h4 className="text-sm font-medium text-foreground">{title}</h4>
          <p className="text-xs text-muted-foreground">{description}</p>
        </div>
      </div>

      {/* Add Input */}
      <div className="flex gap-2">
        <Input
          type="text"
          placeholder={placeholder}
          value={newItem}
          onChange={(e) => setNewItem(e.target.value)}
          onKeyDown={handleKeyDown}
          className="flex-1 text-sm"
        />
        <Button
          type="button"
          size="sm"
          onClick={handleAdd}
          disabled={!newItem.trim() || items.includes(newItem.trim())}
          className="gap-1"
        >
          <IconPlus className="size-3.5" />
          添加
        </Button>
      </div>

      {/* Suggestions */}
      {suggestions && suggestions.length > 0 && (
        <div className="flex flex-wrap gap-1.5">
          {suggestions
            .filter((s) => !items.includes(s))
            .map((suggestion) => (
              <button
                key={suggestion}
                type="button"
                onClick={() => handleSuggestionClick(suggestion)}
                className="rounded-full border border-chart-1/30 bg-chart-1/5 px-2.5 py-0.5 text-xs text-chart-1 transition-colors hover:bg-chart-1/15"
              >
                + {suggestion}
              </button>
            ))}
        </div>
      )}

      {/* Item List */}
      {localItems.length > 0 && (
        <Reorder.Group
          axis="y"
          values={localItems}
          onReorder={handleReorder}
          className="space-y-1"
        >
          {localItems.map((item, index) => (
            <ReorderItem
              key={item}
              item={item}
              index={index}
              onRemove={() => onRemove(index)}
            />
          ))}
        </Reorder.Group>
      )}
    </div>
  );
}

export function PrioritySection({ form }: PrioritySectionProps) {
  const subtitleGroups = form.state.values.priority.subtitle_groups;
  const subtitleLanguages = form.state.values.priority.subtitle_languages;
  const resolutions = form.state.values.priority.resolutions;

  // Helper to create handlers for each list
  const createHandlers = (
    fieldName:
      | "priority.subtitle_groups"
      | "priority.subtitle_languages"
      | "priority.resolutions"
  ) => {
    const items =
      fieldName === "priority.subtitle_groups"
        ? subtitleGroups
        : fieldName === "priority.subtitle_languages"
          ? subtitleLanguages
          : resolutions;

    return {
      onAdd: (item: string) => {
        form.setFieldValue(fieldName, [...items, item]);
      },
      onRemove: (index: number) => {
        form.setFieldValue(
          fieldName,
          items.filter((_, i) => i !== index)
        );
      },
      onReorder: (newItems: string[]) => {
        form.setFieldValue(fieldName, newItems);
      },
    };
  };

  return (
    <section className="space-y-6">
      {/* Header */}
      <div className="rounded-xl bg-gradient-to-br from-chart-1/5 to-chart-3/5 p-4 border border-chart-1/20">
        <p className="text-sm text-muted-foreground">
          配置资源选择的优先级顺序。当多个资源可用时，系统会自动选择优先级最高的资源下载。
          如果发现更高优先级的资源，会自动替换（洗版）已下载的低优先级资源。
        </p>
        <p className="mt-2 text-xs text-muted-foreground/80">
          优先级固定顺序：字幕组 &gt; 字幕语种 &gt; 分辨率。未配置的属性视为最低优先级。
        </p>
      </div>

      {/* Subtitle Groups */}
      <PriorityList
        title="字幕组优先级"
        description="越靠前优先级越高"
        icon="♡"
        items={subtitleGroups}
        placeholder="输入字幕组名称..."
        suggestions={["ANi", "喵萌奶茶屋", "桜都字幕组", "LoliHouse", "NC-Raws"]}
        {...createHandlers("priority.subtitle_groups")}
      />

      {/* Subtitle Languages */}
      <PriorityList
        title="字幕语种优先级"
        description="越靠前优先级越高"
        icon="✨"
        items={subtitleLanguages}
        placeholder="输入字幕语种..."
        suggestions={["简日", "简繁日", "简体", "繁日", "繁体"]}
        {...createHandlers("priority.subtitle_languages")}
      />

      {/* Resolutions */}
      <PriorityList
        title="分辨率优先级"
        description="越靠前优先级越高"
        icon="★"
        items={resolutions}
        placeholder="输入分辨率..."
        suggestions={["2160P", "1080P", "720P", "480P"]}
        {...createHandlers("priority.resolutions")}
      />
    </section>
  );
}
