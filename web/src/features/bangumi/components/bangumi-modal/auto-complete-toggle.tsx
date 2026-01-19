import { cn } from "@/lib/utils";
import { Field, FieldLabel } from "@/components/ui/field";
import { IconCircleCheck } from "@tabler/icons-react";

interface AutoCompleteToggleProps {
  id: string;
  value: boolean;
  onChange: (value: boolean) => void;
}

export function AutoCompleteToggle({
  id,
  value,
  onChange,
}: AutoCompleteToggleProps) {
  return (
    <Field orientation="horizontal">
      <FieldLabel htmlFor={id} className="flex-1 cursor-pointer">
        <div className="flex items-center gap-2">
          <IconCircleCheck className="size-4 text-chart-3 dark:text-chart-1" />
          自动完成
        </div>
        <p className="text-xs text-muted-foreground font-normal mt-0.5">
          启用后下载全部剧集，禁用后仅下载最新一集
        </p>
      </FieldLabel>
      <button
        id={id}
        type="button"
        role="switch"
        aria-checked={value}
        onClick={() => onChange(!value)}
        className={cn(
          "relative inline-flex h-6 w-11 shrink-0 cursor-pointer items-center rounded-full transition-colors duration-200",
          "focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-chart-3 dark:focus-visible:ring-chart-1 focus-visible:ring-offset-2",
          value
            ? "bg-linear-to-r from-chart-3 to-chart-1"
            : "bg-muted"
        )}
      >
        <span
          className={cn(
            "pointer-events-none block size-5 rounded-full bg-white shadow-lg ring-0 transition-transform duration-200",
            value ? "translate-x-5" : "translate-x-0.5"
          )}
        />
      </button>
    </Field>
  );
}
