import { cn } from "@/lib/utils";
import { IconCircleCheck, IconPlayerPlay } from "@tabler/icons-react";

interface StatusBadgeProps {
  finished: boolean;
  /** "default" for info cards, "overlay" for image overlays */
  variant?: "default" | "overlay";
  className?: string;
}

export function StatusBadge({ finished, variant = "default", className }: StatusBadgeProps) {
  const Icon = finished ? IconCircleCheck : IconPlayerPlay;
  const label = finished ? "完结" : "放送中";

  const content = (
    <>
      <Icon className="size-3" />
      <span>{label}</span>
    </>
  );

  if (variant === "overlay") {
    return (
      <div
        className={cn(
          "flex items-center gap-1 rounded-lg px-2 py-1",
          "text-[10px] font-medium",
          "backdrop-blur-xl",
          "border",
          finished
            ? "bg-emerald-500/20 text-emerald-100 border-emerald-400/30"
            : "bg-amber-500/20 text-amber-100 border-amber-400/30",
          className
        )}
      >
        {content}
      </div>
    );
  }

  return (
    <span
      className={cn(
        "inline-flex items-center gap-1 px-2 py-0.5 rounded-md text-xs",
        finished
          ? "bg-emerald-500/15 dark:bg-emerald-500/20 text-emerald-600 dark:text-emerald-400"
          : "bg-amber-500/15 dark:bg-amber-500/20 text-amber-600 dark:text-amber-400",
        className
      )}
    >
      {content}
    </span>
  );
}
