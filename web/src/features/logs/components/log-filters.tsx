import { IconCalendar, IconFilter } from "@tabler/icons-react";
import { format, parse } from "date-fns";
import { zhCN } from "date-fns/locale";
import { Button } from "@/components/ui/button";
import { Calendar } from "@/components/ui/calendar";
import {
  Popover,
  PopoverContent,
  PopoverTrigger,
} from "@/components/ui/popover";
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
} from "@/components/ui/select";
import type { LogLevel } from "../hooks/use-log-filters";

const LEVEL_OPTIONS = [
  { value: "all", label: "全部" },
  { value: "error", label: "错误" },
  { value: "warning", label: "警告" },
  { value: "info", label: "信息" },
] as const;

export interface LogFiltersProps {
  date: string;
  level: LogLevel;
  onDateChange: (date: string) => void;
  onLevelChange: (level: LogLevel) => void;
  disabled?: boolean;
}

export function LogFilters({
  date,
  level,
  onDateChange,
  onLevelChange,
  disabled,
}: LogFiltersProps) {
  const selectedDate = parse(date, "yyyy-MM-dd", new Date());

  const handleDateSelect = (day: Date | undefined) => {
    if (day) {
      onDateChange(format(day, "yyyy-MM-dd"));
    }
  };

  return (
    <div className="flex items-center gap-3">
      <Popover>
        <PopoverTrigger
          render={
            <Button variant="outline" size="sm" disabled={disabled}>
              <IconCalendar className="size-4 text-muted-foreground" />
              <span>{format(selectedDate, "yyyy年M月d日", { locale: zhCN })}</span>
            </Button>
          }
        />
        <PopoverContent className="w-auto p-0" align="end">
          <Calendar
            mode="single"
            selected={selectedDate}
            onSelect={handleDateSelect}
            locale={zhCN}
            disabled={{ after: new Date() }}
          />
        </PopoverContent>
      </Popover>

      <Select
        value={level ?? "all"}
        onValueChange={(value) =>
          onLevelChange(value === "all" ? undefined : (value as LogLevel))
        }
        disabled={disabled}
      >
        <SelectTrigger size="sm" className="w-24">
          <IconFilter className="size-4 text-muted-foreground" />
          <span>
            {LEVEL_OPTIONS.find((o) => o.value === (level ?? "all"))?.label}
          </span>
        </SelectTrigger>
        <SelectContent>
          {LEVEL_OPTIONS.map((option) => (
            <SelectItem key={option.value} value={option.value}>
              {option.label}
            </SelectItem>
          ))}
        </SelectContent>
      </Select>
    </div>
  );
}
