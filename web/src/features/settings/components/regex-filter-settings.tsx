import * as React from "react";
import { cn } from "@/lib/utils";
import { Input } from "@/components/ui/input";
import { Button } from "@/components/ui/button";
import { Label } from "@/components/ui/label";
import {
  IconFilter,
  IconPlus,
  IconTrash,
  IconRegex,
  IconDeviceFloppy,
  IconLoader2,
} from "@tabler/icons-react";
import { useMutation, useQueryClient } from "@tanstack/react-query";
import {
  updateSettingsMutation,
  getSettingsQueryKey,
  type FilterSettings,
} from "@/lib/api";

function SettingsCard({
  children,
  className,
}: {
  children: React.ReactNode;
  className?: string;
}) {
  return (
    <div
      className={cn(
        "relative overflow-hidden rounded-2xl border border-border/50 bg-card/80 p-6 backdrop-blur-sm",
        "ring-1 ring-foreground/5",
        "before:absolute before:inset-0 before:-z-10 before:bg-linear-to-br before:from-chart-1/5 before:via-transparent before:to-chart-3/5",
        className
      )}
    >
      {children}
    </div>
  );
}

interface RegexPattern {
  id: string;
  pattern: string;
}

interface RegexFilterSettingsProps {
  settings?: FilterSettings;
}

export function RegexFilterSettings({ settings }: RegexFilterSettingsProps) {
  const queryClient = useQueryClient();
  const [patterns, setPatterns] = React.useState<RegexPattern[]>([]);
  const [newPattern, setNewPattern] = React.useState("");
  const [error, setError] = React.useState<string | null>(null);

  // 保存 mutation
  const { mutate: saveSettings, isPending: isSaving } = useMutation({
    ...updateSettingsMutation(),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: getSettingsQueryKey() });
    },
  });

  // 当设置加载完成后，用 API 数据初始化
  React.useEffect(() => {
    if (settings?.global_rss_filters) {
      setPatterns(
        settings.global_rss_filters.map((pattern, index) => ({
          id: index.toString(),
          pattern,
        }))
      );
    }
  }, [settings]);

  // 检测是否有未保存的修改
  const isDirty = React.useMemo(() => {
    const currentPatterns = patterns.map((p) => p.pattern);
    const originalPatterns = settings?.global_rss_filters ?? [];
    if (currentPatterns.length !== originalPatterns.length) return true;
    return currentPatterns.some((p, i) => p !== originalPatterns[i]);
  }, [patterns, settings]);

  // 保存设置
  const handleSave = () => {
    saveSettings({
      body: {
        filter: {
          global_rss_filters: patterns.map((p) => p.pattern),
        },
      },
    });
  };

  const handleAddPattern = () => {
    if (!newPattern.trim()) return;

    // Validate regex
    try {
      new RegExp(newPattern);
    } catch {
      setError("无效的正则表达式");
      return;
    }

    setPatterns([...patterns, { id: Date.now().toString(), pattern: newPattern }]);
    setNewPattern("");
    setError(null);
  };

  const handleRemovePattern = (id: string) => {
    setPatterns(patterns.filter((p) => p.id !== id));
  };

  const handleKeyDown = (e: React.KeyboardEvent) => {
    if (e.key === "Enter") {
      e.preventDefault();
      handleAddPattern();
    }
  };

  return (
    <SettingsCard>
      <div className="space-y-5">
        {/* Add new pattern */}
        <div className="space-y-2">
          <Label className="text-sm font-medium">添加过滤规则</Label>
          <div className="flex gap-2">
            <div className="relative flex-1">
              <div className="pointer-events-none absolute left-2.5 top-1/2 -translate-y-1/2 text-muted-foreground">
                <IconRegex className="size-4" />
              </div>
              <Input
                type="text"
                placeholder="输入正则表达式..."
                value={newPattern}
                onChange={(e) => {
                  setNewPattern(e.target.value);
                  setError(null);
                }}
                onKeyDown={handleKeyDown}
                className={cn(
                  "pl-9 font-mono text-sm transition-all duration-200",
                  "focus:ring-2 focus:ring-chart-3/20",
                  error && "border-destructive focus:ring-destructive/20"
                )}
              />
            </div>
            <Button
              onClick={handleAddPattern}
              disabled={!newPattern.trim()}
              className="gap-1.5 bg-chart-3 text-white hover:bg-chart-3/90"
            >
              <IconPlus className="size-4" />
              添加
            </Button>
          </div>
          {error && (
            <p className="text-sm text-destructive">{error}</p>
          )}
        </div>

        {/* Pattern list */}
        <div className="space-y-2">
          <Label className="text-sm font-medium">
            已添加规则
            <span className="ml-2 text-xs font-normal text-muted-foreground">
              ({patterns.length} 条)
            </span>
          </Label>

          {patterns.length === 0 ? (
            <div className="flex flex-col items-center justify-center rounded-xl border border-dashed border-border/50 bg-muted/30 py-8">
              <IconFilter className="mb-2 size-8 text-muted-foreground/50" />
              <p className="text-sm text-muted-foreground">暂无过滤规则</p>
            </div>
          ) : (
            <div className="space-y-2">
              {patterns.map((pattern, index) => (
                <div
                  key={pattern.id}
                  className={cn(
                    "group flex items-center justify-between gap-3 rounded-lg border border-border/50 bg-muted/30 px-3 py-2.5",
                    "transition-all duration-200 hover:border-chart-3/30 hover:bg-muted/50"
                  )}
                  style={{
                    animationDelay: `${index * 50}ms`,
                  }}
                >
                  <div className="flex min-w-0 flex-1 items-center gap-3">
                    <span className="flex size-6 shrink-0 items-center justify-center rounded-md bg-chart-3/10 text-xs font-medium text-chart-3">
                      {index + 1}
                    </span>
                    <code className="truncate font-mono text-sm text-foreground/80">
                      {pattern.pattern}
                    </code>
                  </div>
                  <Button
                    variant="ghost"
                    size="icon-sm"
                    onClick={() => handleRemovePattern(pattern.id)}
                    className="shrink-0 opacity-0 transition-opacity group-hover:opacity-100 hover:bg-destructive/10 hover:text-destructive"
                  >
                    <IconTrash className="size-4" />
                  </Button>
                </div>
              ))}
            </div>
          )}
        </div>

        {/* Save button */}
        <div className="flex items-center justify-end border-t border-border/50 pt-5">
          <Button
            onClick={handleSave}
            disabled={!isDirty || isSaving}
            className={cn(
              "gap-2",
              "bg-linear-to-r from-chart-3 to-chart-5 text-white",
              "shadow-lg shadow-chart-3/20 transition-all duration-300",
              "hover:shadow-xl hover:shadow-chart-3/30",
              "disabled:opacity-50"
            )}
          >
            {isSaving ? (
              <IconLoader2 className="size-4 animate-spin" />
            ) : (
              <IconDeviceFloppy className="size-4" />
            )}
            {isSaving ? "保存中..." : "保存"}
          </Button>
        </div>
      </div>
    </SettingsCard>
  );
}
