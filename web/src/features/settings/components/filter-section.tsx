import * as React from "react";
import { cn } from "@/lib/utils";
import { Input } from "@/components/ui/input";
import { Button } from "@/components/ui/button";
import { Label } from "@/components/ui/label";
import {
  IconPlus,
  IconTrash,
  IconRegex,
} from "@tabler/icons-react";
import { getErrorMessage, type SettingsFormInstance } from "../hooks";

export interface FilterSectionProps {
  form: SettingsFormInstance;
}

export function FilterSection({ form }: FilterSectionProps) {
  // Temporary UI state for the input field
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

    // Add to form state
    const currentPatterns = form.state.values.filter.global_rss_filters;
    form.setFieldValue("filter.global_rss_filters", [...currentPatterns, newPattern]);

    setNewPattern("");
    setError(null);
  };

  const handleRemovePattern = (index: number) => {
    const currentPatterns = form.state.values.filter.global_rss_filters;
    form.setFieldValue(
      "filter.global_rss_filters",
      currentPatterns.filter((_, i) => i !== index)
    );
  };

  const handleKeyDown = (e: React.KeyboardEvent) => {
    if (e.key === "Enter") {
      e.preventDefault();
      handleAddPattern();
    }
  };

  return (
    <section className="space-y-5">
      <div>
        <h3 className="text-sm font-medium text-foreground">全局过滤规则</h3>
        <p className="text-xs text-muted-foreground mt-0.5">
          使用正则表达式过滤 RSS 订阅内容
        </p>
      </div>

      <div className="space-y-4">
        {/* Add Pattern Input */}
        <div className="space-y-1.5">
          <Label className="text-sm text-muted-foreground">添加规则</Label>
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
                  "pl-9 font-mono text-sm",
                  error && "border-destructive"
                )}
              />
            </div>
            <Button
              type="button"
              size="sm"
              onClick={handleAddPattern}
              disabled={!newPattern.trim()}
              className="gap-1"
            >
              <IconPlus className="size-3.5" />
              添加
            </Button>
          </div>
          {error && <p className="text-xs text-destructive">{error}</p>}
        </div>

        {/* Pattern List */}
        <form.Field name="filter.global_rss_filters">
          {(field) => {
            const fieldError = getErrorMessage(field.state.meta.errors[0]);
            return (
              <>
                {field.state.value.length > 0 && (
                  <div className="space-y-1.5">
                    <Label className="text-sm text-muted-foreground">
                      已添加 ({field.state.value.length})
                    </Label>
                    <div className="space-y-1">
                      {field.state.value.map((pattern, index) => (
                        <div
                          key={index}
                          className="group flex items-center gap-2 rounded-md border border-border/50 bg-muted/30 px-2.5 py-1.5"
                        >
                          <span className="flex size-5 shrink-0 items-center justify-center rounded bg-chart-3/10 text-xs text-chart-3">
                            {index + 1}
                          </span>
                          <code className="flex-1 truncate font-mono text-sm text-foreground/80">
                            {pattern}
                          </code>
                          <Button
                            type="button"
                            variant="ghost"
                            size="icon-sm"
                            onClick={() => handleRemovePattern(index)}
                            className="size-6 shrink-0 text-muted-foreground hover:bg-destructive/10 hover:text-destructive"
                          >
                            <IconTrash className="size-3.5" />
                          </Button>
                        </div>
                      ))}
                    </div>
                  </div>
                )}
                {fieldError && (
                  <p className="text-xs text-destructive">{fieldError}</p>
                )}
              </>
            );
          }}
        </form.Field>
      </div>
    </section>
  );
}
