import { Switch as SwitchPrimitive } from "@base-ui/react/switch";
import { cn } from "@/lib/utils";

interface SwitchProps {
  checked?: boolean;
  onCheckedChange?: (checked: boolean) => void;
  disabled?: boolean;
  className?: string;
}

function Switch({
  checked,
  onCheckedChange,
  disabled,
  className,
}: SwitchProps) {
  return (
    <SwitchPrimitive.Root
      checked={checked}
      onCheckedChange={onCheckedChange}
      disabled={disabled}
      className={cn(
        "group relative inline-flex h-5 w-9 shrink-0 cursor-pointer items-center rounded-full",
        "border-2 border-transparent shadow-sm transition-all duration-300 ease-out",
        "focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-chart-1/50 focus-visible:ring-offset-2 focus-visible:ring-offset-background",
        "disabled:cursor-not-allowed disabled:opacity-50",
        "bg-muted data-checked:bg-linear-to-r data-checked:from-chart-1 data-checked:to-chart-3/80",
        "hover:shadow-md data-checked:shadow-chart-1/30",
        className
      )}
    >
      <SwitchPrimitive.Thumb
        className={cn(
          "pointer-events-none block size-4 rounded-full bg-white ring-0",
          "shadow-md transition-all duration-300 ease-out",
          "translate-x-0 data-checked:translate-x-4",
          "data-checked:shadow-lg data-checked:shadow-chart-1/20"
        )}
      />
    </SwitchPrimitive.Root>
  );
}

export { Switch };
