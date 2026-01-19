import * as React from "react";
import { Dialog as DialogPrimitive } from "@base-ui/react/dialog";
import { motion } from "framer-motion";
import { cn } from "@/lib/utils";

interface AnimatedModalProps {
  open: boolean;
  onOpenChange: (open: boolean) => void;
  /** When true, modal shifts left and shrinks (for nested modals) */
  shifted?: boolean;
  children: React.ReactNode;
  className?: string;
}

export function AnimatedModal({
  open,
  onOpenChange,
  shifted = false,
  children,
  className,
}: AnimatedModalProps) {
  return (
    <DialogPrimitive.Root open={open} onOpenChange={onOpenChange}>
      <DialogPrimitive.Portal>
        {/* Backdrop */}
        <DialogPrimitive.Backdrop
          className={cn(
            "fixed inset-0 z-50",
            "bg-black/20 dark:bg-black/40 backdrop-blur-sm",
            "data-[state=open]:animate-in data-[state=closed]:animate-out",
            "data-[state=closed]:fade-out-0 data-[state=open]:fade-in-0",
            "duration-200"
          )}
        />

        {/* Modal */}
        <DialogPrimitive.Popup
          render={
            <motion.div
              initial={{ scale: 0.9, y: 20, opacity: 0 }}
              animate={{
                scale: shifted ? 0.95 : 1,
                y: 0,
                left: shifted ? "25%" : "50%",
                opacity: shifted ? 0.8 : 1,
              }}
              transition={{
                type: "spring",
                damping: 25,
                stiffness: 400,
              }}
              className={cn(
                "fixed left-1/2 top-1/2 z-50 -translate-y-1/2 -translate-x-1/2",
                "w-[calc(100%-2rem)] max-w-2xl",
                "max-h-[90vh] overflow-hidden",
                "rounded-2xl",
                "bg-linear-to-br from-white/95 via-white/90 to-chart-3/10",
                "dark:from-zinc-900/95 dark:via-zinc-900/90 dark:to-chart-1/20",
                "border border-chart-3/30 dark:border-chart-1/30",
                "shadow-2xl shadow-chart-3/20 dark:shadow-chart-1/50",
                "backdrop-blur-xl",
                "outline-none",
                className
              )}
            />
          }
        >
          {children}
        </DialogPrimitive.Popup>
      </DialogPrimitive.Portal>
    </DialogPrimitive.Root>
  );
}

// Re-export dialog primitives for convenience
export const AnimatedModalClose = DialogPrimitive.Close;
export const AnimatedModalTitle = DialogPrimitive.Title;
