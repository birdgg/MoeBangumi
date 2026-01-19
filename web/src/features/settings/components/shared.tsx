import { cn } from "@/lib/utils";
import { Label } from "@/components/ui/label";

interface SectionHeaderProps {
  icon: React.ReactNode;
  title: string;
  description?: string;
  accent?: "chart-1" | "chart-3";
}

export function SectionHeader({
  icon,
  title,
  accent = "chart-1",
}: SectionHeaderProps) {
  return (
    <div className="flex items-center gap-3 pb-2">
      <div
        className={cn(
          "flex size-8 items-center justify-center rounded-lg",
          accent === "chart-1" && "bg-chart-1/10 text-chart-1",
          accent === "chart-3" && "bg-chart-3/10 text-chart-3"
        )}
      >
        {icon}
      </div>
      <div>
        <h2 className="text-sm font-semibold text-foreground">{title}</h2>
      </div>
    </div>
  );
}

interface FormFieldProps {
  label: string;
  children: React.ReactNode;
}

export function FormField({ label, children }: FormFieldProps) {
  return (
    <div className="space-y-1.5">
      <Label className="text-sm text-muted-foreground">{label}</Label>
      {children}
    </div>
  );
}
