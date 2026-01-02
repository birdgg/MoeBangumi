import { useState, useMemo, useCallback } from "react";
import type { Season } from "@/lib/api";

// Season order and labels
const SEASONS: Season[] = ["winter", "spring", "summer", "fall"];

// Year range
const MIN_YEAR = 2012;

// LocalStorage key for persisting season selection
const CALENDAR_SEASON_KEY = "moe-calendar-season";

// Season option type
export interface SeasonOption {
  value: string; // "2025-winter"
  label: string; // "2025 冬"
  year: number;
  season: Season;
}

export const SEASON_LABELS: Record<Season, string> = {
  winter: "冬",
  spring: "春",
  summer: "夏",
  fall: "秋",
};

// Get current season based on month
function getCurrentSeason(): Season {
  const month = new Date().getMonth() + 1;
  if (month >= 1 && month <= 3) return "winter";
  if (month >= 4 && month <= 6) return "spring";
  if (month >= 7 && month <= 9) return "summer";
  return "fall";
}

// Get stored season selection from localStorage
function getStoredSeasonSelection(): { year: number; season: Season } | null {
  if (typeof window === "undefined") return null;
  const stored = localStorage.getItem(CALENDAR_SEASON_KEY);
  if (!stored) return null;
  try {
    const parsed = JSON.parse(stored);
    const currentYear = new Date().getFullYear();
    if (
      typeof parsed.year === "number" &&
      parsed.year >= MIN_YEAR &&
      parsed.year <= currentYear &&
      SEASONS.includes(parsed.season)
    ) {
      // Validate that stored selection is not in the future
      if (parsed.year === currentYear) {
        const currentSeasonIndex = SEASONS.indexOf(getCurrentSeason());
        const storedSeasonIndex = SEASONS.indexOf(parsed.season);
        if (storedSeasonIndex > currentSeasonIndex) {
          return null; // Stored season is in the future, ignore it
        }
      }
      return parsed;
    }
  } catch {
    // Invalid JSON, ignore
  }
  return null;
}

// Save season selection to localStorage
function setStoredSeasonSelection(year: number, season: Season): void {
  if (typeof window === "undefined") return;
  localStorage.setItem(CALENDAR_SEASON_KEY, JSON.stringify({ year, season }));
}

// Generate all season options (newest first, excluding future seasons)
function getSeasonOptions(): SeasonOption[] {
  const options: SeasonOption[] = [];
  const currentYear = new Date().getFullYear();
  const currentSeason = getCurrentSeason();
  const currentSeasonIndex = SEASONS.indexOf(currentSeason);

  for (let year = currentYear; year >= MIN_YEAR; year--) {
    // For current year, only show seasons up to current
    // For past years, show all seasons (reversed: fall -> winter)
    const seasonsToShow =
      year === currentYear
        ? SEASONS.slice(0, currentSeasonIndex + 1).reverse()
        : [...SEASONS].reverse();

    for (const season of seasonsToShow) {
      options.push({
        value: `${year}-${season}`,
        label: `${year} ${SEASON_LABELS[season]}`,
        year,
        season,
      });
    }
  }
  return options;
}

export interface UseSeasonSelectionReturn {
  year: number;
  season: Season;
  options: SeasonOption[];
  selectedValue: string;
  updateSelection: (year: number, season: Season) => void;
  handleValueChange: (value: string | null) => void;
}

export function useSeasonSelection(): UseSeasonSelectionReturn {
  const [year, setYear] = useState(() => {
    const stored = getStoredSeasonSelection();
    return stored?.year ?? new Date().getFullYear();
  });
  const [season, setSeason] = useState<Season>(() => {
    const stored = getStoredSeasonSelection();
    return stored?.season ?? getCurrentSeason();
  });

  const options = useMemo(() => getSeasonOptions(), []);
  const selectedValue = `${year}-${season}`;

  const updateSelection = useCallback((newYear: number, newSeason: Season) => {
    setYear(newYear);
    setSeason(newSeason);
    setStoredSeasonSelection(newYear, newSeason);
  }, []);

  const handleValueChange = useCallback(
    (value: string | null) => {
      if (!value) return;
      const option = options.find((o) => o.value === value);
      if (option) {
        updateSelection(option.year, option.season);
      }
    },
    [options, updateSelection]
  );

  return {
    year,
    season,
    options,
    selectedValue,
    updateSelection,
    handleValueChange,
  };
}
