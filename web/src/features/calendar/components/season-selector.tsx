import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
} from "@/components/ui/select";
import type { SeasonOption, Season } from "../hooks";
import { SEASON_LABELS } from "../hooks";

export interface SeasonSelectorProps {
  year: number;
  season: Season;
  options: SeasonOption[];
  selectedValue: string;
  onValueChange: (value: string | null) => void;
  disabled?: boolean;
  className?: string;
}

export function SeasonSelector({
  year,
  season,
  options,
  selectedValue,
  onValueChange,
  disabled,
  className,
}: SeasonSelectorProps) {
  return (
    <Select
      value={selectedValue}
      onValueChange={onValueChange}
      disabled={disabled}
    >
      <SelectTrigger size="sm" className={className}>
        <span>
          {year} {SEASON_LABELS[season]}
        </span>
      </SelectTrigger>
      <SelectContent>
        {options.map((option) => (
          <SelectItem key={option.value} value={option.value}>
            {option.label}
          </SelectItem>
        ))}
      </SelectContent>
    </Select>
  );
}
