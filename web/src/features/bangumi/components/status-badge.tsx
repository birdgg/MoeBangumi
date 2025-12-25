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
          "flex items-center gap-1 rounded-full px-2 py-0.5",
          "text-[10px] font-bold",
          "backdrop-blur-md transition-all duration-300",
          "shadow-sm",
          finished
            ? "bg-emerald-400/90 text-emerald-950"
            : "bg-amber-300/90 text-amber-900",
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
