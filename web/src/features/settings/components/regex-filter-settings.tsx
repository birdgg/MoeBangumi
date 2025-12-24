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
} from "@tabler/icons-react";

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

export function RegexFilterSettings() {
  const [patterns, setPatterns] = React.useState<RegexPattern[]>([
    { id: "1", pattern: "\\[繁體\\]" },
    { id: "2", pattern: "\\[简体\\]" },
  ]);
  const [newPattern, setNewPattern] = React.useState("");
  const [error, setError] = React.useState<string | null>(null);

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
      <div className="mb-6 flex items-center gap-3">
        <div className="flex size-10 items-center justify-center rounded-xl bg-linear-to-br from-chart-3/20 to-chart-5/20">
          <IconFilter className="size-5 text-chart-3" />
        </div>
        <div>
          <h3 className="font-semibold text-foreground">正则过滤规则</h3>
          <p className="text-sm text-muted-foreground">
            匹配的资源标题将被自动过滤
          </p>
        </div>
      </div>

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
      </div>
    </SettingsCard>
  );
}
