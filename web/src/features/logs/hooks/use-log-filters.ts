import { useState, useEffect } from "react";
import { formatInTimeZone } from "date-fns-tz";
import type { LogLevel as ApiLogLevel } from "@/lib/api";

const STORAGE_KEY_DATE = "logs-filter-date";
const STORAGE_KEY_LEVEL = "logs-filter-level";

export type LogLevel = ApiLogLevel | undefined;

export function getUtcDateString(): string {
  return formatInTimeZone(new Date(), "UTC", "yyyy-MM-dd");
}

export function useLogFilters() {
  const [date, setDate] = useState<string>(() => {
    if (typeof window === "undefined") return getUtcDateString();
    const stored = localStorage.getItem(STORAGE_KEY_DATE);
    return stored ?? getUtcDateString();
  });

  const [level, setLevel] = useState<LogLevel>(() => {
    if (typeof window === "undefined") return undefined;
    const stored = localStorage.getItem(STORAGE_KEY_LEVEL);
    return stored ? (stored as LogLevel) : undefined;
  });

  useEffect(() => {
    localStorage.setItem(STORAGE_KEY_DATE, date);
  }, [date]);

  useEffect(() => {
    if (level) {
      localStorage.setItem(STORAGE_KEY_LEVEL, level);
    } else {
      localStorage.removeItem(STORAGE_KEY_LEVEL);
    }
  }, [level]);

  return {
    date,
    level,
    setDate,
    setLevel,
  };
}
