import { Toaster as Sonner, type ToasterProps } from "sonner";

function Toaster({ ...props }: ToasterProps) {
  return (
    <Sonner
      toastOptions={{
        unstyled: true,
        classNames: {
          toast:
            "flex items-center gap-3 w-[356px] p-4 rounded-xl border shadow-lg bg-popover text-popover-foreground border-border",
          title: "font-semibold text-sm",
          description: "text-sm text-muted-foreground mt-1",
          success:
            "flex items-center gap-3 w-[356px] p-4 rounded-xl border-2 shadow-lg shadow-chart-1/10 bg-popover text-popover-foreground border-chart-1",
          error:
            "flex items-center gap-3 w-[356px] p-4 rounded-xl border-2 shadow-lg shadow-destructive/10 bg-popover text-popover-foreground border-destructive",
        },
      }}
      {...props}
    />
  );
}

export { Toaster };
