import { useState, useMemo } from "react";
import { cn } from "@/lib/utils";
import {
  useCalendar,
  useRefreshCalendar,
  useSeasonSelection,
} from "./hooks";
import { CalendarCard, CalendarCardSkeleton, SeasonSelector } from "./components";
import { BangumiModal } from "@/features/bangumi/components";
import type { CalendarSubject } from "@/lib/api";
import {
  IconAlertCircle,
  IconCalendarWeek,
  IconRefresh,
} from "@tabler/icons-react";
import { Button } from "@/components/ui/button";
import { calendarSubjectToModalData } from "@/lib/converters";

// Get today's weekday (1-7, Monday-Sunday)
function getTodayWeekday(): number {
  const day = new Date().getDay();
  // Convert Sunday (0) to 7, keep others as is
  return day === 0 ? 7 : day;
}

// Weekday labels for display
const WEEKDAY_LABELS: Record<number, string> = {
  1: "周一",
  2: "周二",
  3: "周三",
  4: "周四",
  5: "周五",
  6: "周六",
  7: "周日",
};

export function SchedulePage() {
  // Season selection with localStorage persistence
  const {
    year,
    season,
    options,
    selectedValue,
    handleValueChange,
  } = useSeasonSelection();

  const calendarParams = { year, season };
  const { data: calendar, isLoading, error } = useCalendar(calendarParams);
  const refreshMutation = useRefreshCalendar(calendarParams);

  const [selectedSubject, setSelectedSubject] =
    useState<CalendarSubject | null>(null);
  const [modalOpen, setModalOpen] = useState(false);

  const isRefreshing = refreshMutation.isPending;

  // Sort calendar by weekday, starting from today
  const todayWeekday = getTodayWeekday();
  const sortedCalendar = useMemo(() => {
    if (!calendar) return [];

    // Sort so today comes first, then tomorrow, etc.
    return [...calendar].sort((a, b) => {
      const aDay = a.weekday.id;
      const bDay = b.weekday.id;
      const aOffset = (aDay - todayWeekday + 7) % 7;
      const bOffset = (bDay - todayWeekday + 7) % 7;
      return aOffset - bOffset;
    });
  }, [calendar, todayWeekday]);

  const handleCardClick = (subject: CalendarSubject) => {
    setSelectedSubject(subject);
    setModalOpen(true);
  };

  const handleModalClose = (open: boolean) => {
    if (!open) {
      setModalOpen(false);
      setSelectedSubject(null);
    }
  };

  const isEmpty = !calendar || calendar.length === 0;

  return (
    <div className="min-h-full bg-linear-to-br from-chart-1/5 via-background to-chart-3/5 dark:from-zinc-950 dark:via-background dark:to-chart-3/10">
      {/* Decorative background elements */}
      <div className="pointer-events-none fixed inset-0 overflow-hidden">
        <div className="absolute -left-40 -top-40 size-80 rounded-full bg-chart-1/20 blur-3xl dark:bg-chart-1/10" />
        <div className="absolute -right-40 top-1/3 size-96 rounded-full bg-chart-3/20 blur-3xl dark:bg-chart-3/10" />
        <div className="absolute -bottom-40 left-1/3 size-80 rounded-full bg-chart-5/20 blur-3xl dark:bg-chart-5/10" />
      </div>

      {/* Content */}
      <div className="relative px-6 py-6 md:px-8">
        {/* Header */}
        <div className="mb-6 flex items-center justify-between">
          <h1 className="text-lg font-medium text-foreground">每日放送</h1>

          {/* Season Selector */}
          <div className="flex items-center gap-2">
            <SeasonSelector
              year={year}
              season={season}
              options={options}
              selectedValue={selectedValue}
              onValueChange={handleValueChange}
              disabled={isLoading}
            />

            <Button
              variant="ghost"
              size="sm"
              onClick={() => refreshMutation.mutate({})}
              disabled={isLoading || isRefreshing}
            >
              <IconRefresh
                className={cn("size-4", isRefreshing && "animate-spin")}
              />
              <span className="ml-1">刷新</span>
            </Button>
          </div>
        </div>

        {/* Loading state */}
        {isLoading && (
          <div className="space-y-8">
            {Array.from({ length: 3 }).map((_, i) => (
              <section key={i}>
                <div className="mb-4 h-8 w-24 animate-pulse rounded-lg bg-chart-1/20" />
                <div className="grid grid-cols-2 gap-4 sm:grid-cols-3 md:grid-cols-4 lg:grid-cols-5 xl:grid-cols-6 2xl:grid-cols-7">
                  {Array.from({ length: 6 }).map((_, j) => (
                    <CalendarCardSkeleton key={j} />
                  ))}
                </div>
              </section>
            ))}
          </div>
        )}

        {/* Error state */}
        {error && (
          <div className="py-16">
            <div className="flex flex-col items-center justify-center text-center">
              <div className="mb-4 flex size-20 items-center justify-center rounded-full bg-destructive/10">
                <IconAlertCircle className="size-10 text-destructive" />
              </div>
              <h3 className="mb-2 text-lg font-semibold text-foreground">
                加载失败
              </h3>
              <p className="mb-6 max-w-sm text-sm text-muted-foreground">
                无法获取每日放送数据，请稍后重试
              </p>
              <Button
                variant="outline"
                onClick={() => window.location.reload()}
              >
                重试
              </Button>
            </div>
          </div>
        )}

        {/* Empty state */}
        {!isLoading && !error && isEmpty && (
          <div className="py-16">
            <div className="flex flex-col items-center justify-center text-center">
              <div className="mb-4 flex size-20 items-center justify-center rounded-full bg-linear-to-br from-chart-1/20 to-chart-3/20 dark:from-chart-1/30 dark:to-chart-3/30">
                <IconCalendarWeek className="size-10 text-chart-1 dark:text-chart-3" />
              </div>
              <h3 className="mb-2 text-lg font-semibold text-foreground">
                暂无放送数据
              </h3>
              <p className="max-w-sm text-sm text-muted-foreground">
                目前没有可显示的每日放送数据
              </p>
            </div>
          </div>
        )}

        {/* Calendar grid by weekday */}
        {!isLoading && !error && !isEmpty && (
          <div className="space-y-10">
            {sortedCalendar.map((day) => {
              const isToday = day.weekday.id === todayWeekday;
              return (
                <section key={day.weekday.id}>
                  {/* Weekday header */}
                  <div className="mb-4 flex items-center gap-3">
                    <h2
                      className={cn(
                        "text-xl font-bold",
                        isToday
                          ? "bg-linear-to-r from-chart-1 via-chart-2 to-chart-3 bg-clip-text text-transparent"
                          : "text-foreground"
                      )}
                    >
                      {WEEKDAY_LABELS[day.weekday.id]}
                    </h2>
                    {isToday && (
                      <span className="rounded-full bg-linear-to-r from-chart-1 to-chart-3 px-2.5 py-0.5 text-xs font-medium text-white shadow-sm">
                        今天
                      </span>
                    )}
                    <span className="text-sm text-muted-foreground">
                      {day.items.length} 部
                    </span>
                  </div>

                  {/* Cards grid */}
                  <div className="grid grid-cols-2 gap-4 sm:grid-cols-3 md:grid-cols-4 lg:grid-cols-5 xl:grid-cols-6 2xl:grid-cols-7">
                    {day.items.map((subject) => (
                      <CalendarCard
                        key={subject.bgmtv_id ?? subject.mikan_id ?? subject.title_chinese}
                        subject={subject}
                        onClick={() => handleCardClick(subject)}
                      />
                    ))}
                  </div>
                </section>
              );
            })}
          </div>
        )}
      </div>

      {/* Add Bangumi Modal */}
      {selectedSubject && (
        <BangumiModal
          open={modalOpen}
          onOpenChange={handleModalClose}
          mode="add"
          data={calendarSubjectToModalData(selectedSubject)}
          onSuccess={() => {
            setModalOpen(false);
            setSelectedSubject(null);
          }}
        />
      )}
    </div>
  );
}