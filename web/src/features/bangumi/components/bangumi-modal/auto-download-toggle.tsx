import { cn } from "@/lib/utils";
import { Field, FieldLabel } from "@/components/ui/field";
import { IconDownload } from "@tabler/icons-react";

interface AutoDownloadToggleProps {
  id: string;
  value: boolean;
  onChange: (value: boolean) => void;
}

export function AutoDownloadToggle({
  id,
  value,
  onChange,
}: AutoDownloadToggleProps) {
  return (
    <Field orientation="horizontal">
      <FieldLabel htmlFor={id} className="flex-1 cursor-pointer">
        <div className="flex items-center gap-2">
          <IconDownload className="size-4 text-chart-3 dark:text-chart-1" />
          自动下载
        </div>
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
