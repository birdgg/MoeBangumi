import * as React from "react";
import { cn } from "@/lib/utils";
import { Input } from "@/components/ui/input";
import { Button } from "@/components/ui/button";
import {
  IconPlus,
  IconTrash,
  IconGripVertical,
} from "@tabler/icons-react";
import { type SettingsFormInstance } from "../hooks";
import { Reorder, useDragControls } from "framer-motion";
import {
  type SubtitlePattern,
  subtitlePatternLabel,
  patternKey,
} from "../schema";

export interface PrioritySectionProps {
  form: SettingsFormInstance;
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
      className="group flex items-center gap-2 rounded-lg border border-border/50 bg-muted/30 px-2 py-1.5 data-dragging:z-50 data-dragging:border-chart-1/50 data-dragging:bg-muted/80 data-dragging:shadow-lg"
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

interface LanguagePatternItemProps {
  pattern: SubtitlePattern;
  index: number;
}

function LanguagePatternItem({ pattern, index }: LanguagePatternItemProps) {
  const controls = useDragControls();

  return (
    <Reorder.Item
      value={patternKey(pattern)}
      dragListener={false}
      dragControls={controls}
      className="flex items-center gap-2 rounded-lg border border-border/50 bg-muted/30 px-2 py-1.5 data-dragging:z-50 data-dragging:border-chart-1/50 data-dragging:bg-muted/80 data-dragging:shadow-lg"
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

      {/* Pattern label */}
      <span className="flex-1 text-sm">{subtitlePatternLabel(pattern)}</span>
    </Reorder.Item>
  );
}

export function PrioritySection({ form }: PrioritySectionProps) {
  const [newGroup, setNewGroup] = React.useState("");

  const handleAddGroup = () => {
    const trimmed = newGroup.trim();
    if (!trimmed) return;
    const currentGroups = form.getFieldValue("priority.groups") as string[];
    if (currentGroups.includes(trimmed)) return;
    form.setFieldValue("priority.groups", [...currentGroups, trimmed]);
    setNewGroup("");
  };

  const handleRemoveGroup = (item: string) => {
    const currentGroups = form.getFieldValue("priority.groups") as string[];
    form.setFieldValue(
      "priority.groups",
      currentGroups.filter((g) => g !== item)
    );
  };

  const handleReorderGroups = (newItems: string[]) => {
    form.setFieldValue("priority.groups", newItems);
  };

  const handleKeyDown = (e: React.KeyboardEvent) => {
    if (e.key === "Enter") {
      e.preventDefault();
      handleAddGroup();
    }
  };

  const handleReorderLanguages = (newKeys: string[], currentPatterns: SubtitlePattern[]) => {
    // Convert keys back to patterns maintaining order
    const keyToPattern = new Map(currentPatterns.map(p => [patternKey(p), p]));
    const newPatterns = newKeys.map(key => keyToPattern.get(key)!).filter(Boolean);
    form.setFieldValue("priority.languages", newPatterns);
  };

  return (
    <section className="space-y-6">
      {/* Header */}
      <div className="rounded-xl bg-linear-to-br from-chart-1/5 to-chart-3/5 p-4 border border-chart-1/20">
        <p className="text-sm text-muted-foreground">
          配置资源选择的优先级顺序。当多个资源可用时，系统会自动选择优先级最高的资源下载。
        </p>
      </div>

      {/* Subscribe to priority values for reactivity */}
      <form.Subscribe
        selector={(state) => ({
          groups: state.values.priority.groups,
          languages: state.values.priority.languages,
        })}
      >
        {({ groups, languages }) => (
          <>
            {/* Subtitle Groups */}
            <div className="space-y-3">
              <div className="flex items-start gap-2">
                <span className="text-base">♡</span>
                <div>
                  <h4 className="text-sm font-medium text-foreground">字幕组优先级</h4>
                  <p className="text-xs text-muted-foreground">越靠前优先级越高</p>
                </div>
              </div>

              {/* Add Input */}
              <div className="flex gap-2">
                <Input
                  type="text"
                  placeholder="输入字幕组名称..."
                  value={newGroup}
                  onChange={(e) => setNewGroup(e.target.value)}
                  onKeyDown={handleKeyDown}
                  className="flex-1 text-sm"
                />
                <Button
                  type="button"
                  size="sm"
                  onClick={handleAddGroup}
                  disabled={!newGroup.trim() || groups.includes(newGroup.trim())}
                  className="gap-1"
                >
                  <IconPlus className="size-3.5" />
                  添加
                </Button>
              </div>

              {/* Groups List */}
              {groups.length > 0 && (
                <Reorder.Group
                  axis="y"
                  values={groups}
                  onReorder={handleReorderGroups}
                  className="space-y-1"
                >
                  {groups.map((item, index) => (
                    <ReorderItem
                      key={item}
                      item={item}
                      index={index}
                      onRemove={() => handleRemoveGroup(item)}
                    />
                  ))}
                </Reorder.Group>
              )}
            </div>

            {/* Subtitle Language Patterns */}
            <div className="space-y-3">
              <div className="flex items-start gap-2">
                <span className="text-base">✨</span>
                <div>
                  <h4 className="text-sm font-medium text-foreground">字幕语言优先级</h4>
                  <p className="text-xs text-muted-foreground">越靠前优先级越高，拖拽调整顺序</p>
                </div>
              </div>

              {/* Languages List - Reorderable */}
              {languages.length > 0 && (
                <Reorder.Group
                  axis="y"
                  values={languages.map(patternKey)}
                  onReorder={(newKeys) => handleReorderLanguages(newKeys, languages)}
                  className="space-y-1"
                >
                  {languages.map((pattern, index) => (
                    <LanguagePatternItem
                      key={patternKey(pattern)}
                      pattern={pattern}
                      index={index}
                    />
                  ))}
                </Reorder.Group>
              )}
            </div>
          </>
        )}
      </form.Subscribe>
    </section>
  );
}
