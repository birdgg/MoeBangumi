import * as React from "react";
import { Dialog as DialogPrimitive } from "@base-ui/react/dialog";
import { useForm } from "@tanstack/react-form";
import { toast } from "sonner";
import { cn } from "@/lib/utils";
import { parseBgmtvName } from "@/lib/parser";
import { type Subject, type CreateBangumi, type TvShow } from "@/lib/api";
import { useCreateBangumi, useEpisodes } from "../hooks/use-bangumi";
import { Input } from "@/components/ui/input";
import { Button } from "@/components/ui/button";
import {
  Field,
  FieldLabel,
  FieldGroup,
} from "@/components/ui/field";
import {
  IconX,
  IconSparkles,
  IconLoader2,
  IconDeviceTv,
  IconDownload,
  IconFolder,
  IconHash,
  IconMovie,
  IconRss,
  IconSearch,
  IconPlus,
  IconTrash,
  IconCalendar,
  IconStack2,
  IconPlayerPlay,
  IconCircleCheck,
} from "@tabler/icons-react";
import { TmdbMatcher } from "./tmdb-matcher";
import { MikanRssModal } from "./mikan-rss-modal";

interface RssEntry {
  url: string;
  filters: string[];
}

interface AddBangumiModalProps {
  open: boolean;
  onOpenChange: (open: boolean) => void;
  subject: Subject;
  onSuccess?: () => void;
}

export function AddBangumiModal({
  open,
  onOpenChange,
  subject,
  onSuccess,
}: AddBangumiModalProps) {
  const createBangumi = useCreateBangumi();
  const [selectedTmdb, setSelectedTmdb] = React.useState<TvShow | null>(null);
  const [mikanModalOpen, setMikanModalOpen] = React.useState(false);
  const { data: episodes } = useEpisodes(subject?.id ?? 0);

  // Calculate episode offset from episodes data
  const episodeOffset = React.useMemo(() => {
    if (!episodes || episodes.length === 0) return 0;
    return episodes[0].sort
  }, [episodes]);

  // Calculate air_week from air_date (0=Sunday, 1=Monday, ..., 6=Saturday)
  const airWeek = React.useMemo(() => {
    if (!subject?.date) return null;
    const date = new Date(subject.date);
    return isNaN(date.getTime()) ? null : date.getDay();
  }, [subject]);

  // Parse titles and season from subject
  const parsedTitles = React.useMemo(() => {
    if (!subject) return { chinese: "", japanese: "", season: 1 };
    const parsedChinese = parseBgmtvName(subject.name_cn || subject.name || "");
    const parsedJapanese = parseBgmtvName(subject.name || "");
    return {
      chinese: parsedChinese.name,
      japanese: parsedJapanese.name,
      season: parsedChinese.season ?? parsedJapanese.season ?? 1,
    };
  }, [subject]);

  // Check if bangumi has finished airing
  const isFinished = React.useMemo(() => {
    if (!episodes || !subject?.eps) return false;
    return episodes.length >= subject.eps;
  }, [episodes, subject]);

  const form = useForm({
    defaultValues: {
      title_chinese: "",
      title_japanese: "",
      episode_offset: 0,
      auto_download: true,
      save_path: "",
      rss_entries: [] as RssEntry[],
    },
    onSubmit: async ({ value }) => {
      const request: CreateBangumi = {
        title_chinese: value.title_chinese,
        title_japanese: value.title_japanese || null,
        title_original: subject.name_cn,
        year: subject.date ? parseInt(subject.date.split("-")[0]) : new Date().getFullYear(),
        bgmtv_id: subject.id,
        tmdb_id: selectedTmdb?.id ?? null,
        poster_url: subject.image || null,
        air_date: subject.date || null,
        air_week: airWeek,
        total_episodes: subject.eps || 0,
        episode_offset: value.episode_offset,
        auto_download: value.auto_download,
        save_path: value.save_path || null,
        finished: isFinished,
        kind: subject.platform || null,
        season: parsedTitles.season,
        source_type: "webrip",
        rss_entries: value.rss_entries,
      };

      try {
        await createBangumi.mutateAsync({ body: request });
        toast.success("添加成功", {
          description: `「${value.title_chinese}」已添加到追番列表`,
        });
        onSuccess?.();
        resetForm();
        onOpenChange(false);
      } catch (error) {
        const message = error instanceof Error ? error.message : "未知错误";
        toast.error("添加失败", {
          description: message,
        });
      }
    },
  });

  const resetForm = React.useCallback(() => {
    form.reset();
    setSelectedTmdb(null);
    setMikanModalOpen(false);
  }, [form]);

  const handleOpenChange = React.useCallback((newOpen: boolean) => {
    if (!newOpen) {
      resetForm();
    }
    onOpenChange(newOpen);
  }, [resetForm, onOpenChange]);

  // Set form values when modal opens or data changes
  React.useEffect(() => {
    if (open && subject) {
      form.setFieldValue("title_chinese", parsedTitles.chinese);
      form.setFieldValue("title_japanese", parsedTitles.japanese);
      form.setFieldValue("episode_offset", episodeOffset);
    }
  }, [open, subject, parsedTitles, episodeOffset, form]);

  return (
    <DialogPrimitive.Root open={open} onOpenChange={handleOpenChange}>
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
          className={cn(
            "fixed top-1/2 z-50 -translate-y-1/2",
            "w-[calc(100%-2rem)] max-w-2xl",
            "max-h-[90vh] overflow-hidden",
            "rounded-2xl",
            "bg-linear-to-br from-white/95 via-white/90 to-chart-3/10",
            "dark:from-zinc-900/95 dark:via-zinc-900/90 dark:to-chart-1/20",
            "border border-chart-3/30 dark:border-chart-1/30",
            "shadow-2xl shadow-chart-3/20 dark:shadow-chart-1/50",
            "backdrop-blur-xl",
            "data-[state=open]:animate-in data-[state=closed]:animate-out",
            "data-[state=closed]:fade-out-0 data-[state=open]:fade-in-0",
            "data-[state=closed]:zoom-out-95 data-[state=open]:zoom-in-95",
            "duration-300 ease-out",
            "outline-none",
            "transition-all",
            mikanModalOpen
              ? "left-[25%] -translate-x-1/2 scale-95 opacity-80"
              : "left-1/2 -translate-x-1/2"
          )}
        >
          {/* Decorative elements - Top right glow */}
          <div className="pointer-events-none absolute -right-20 -top-20 size-40 rounded-full bg-linear-to-br from-chart-3/30 to-chart-1/30 blur-3xl dark:from-chart-3/20 dark:to-chart-1/20" />

          {/* Bottom backlight - subtle glow */}
          <div className="pointer-events-none absolute -bottom-20 left-1/2 -translate-x-1/2 w-full h-20 -z-10">
            <div className="absolute inset-0 bg-linear-to-t from-chart-1/25 via-chart-3/10 to-transparent blur-2xl dark:from-chart-1/20 dark:via-chart-3/10" />
            <div className="absolute inset-x-[25%] inset-y-0 bg-linear-to-t from-chart-3/30 to-transparent blur-xl dark:from-chart-3/25 animate-pulse animation-duration-[4s]" />
          </div>

          {/* Header */}
          <div className="relative border-b border-chart-3/30 dark:border-chart-1/20 px-4 py-3">
            <div className="flex items-center gap-2.5">
              <div className="flex size-8 items-center justify-center rounded-lg bg-linear-to-br from-chart-3 to-chart-1 text-white shadow-md shadow-chart-1/30">
                <IconSparkles className="size-4" />
              </div>
              <DialogPrimitive.Title className="flex-1 text-base font-semibold bg-linear-to-r from-chart-3 via-chart-1 to-chart-5 bg-clip-text text-transparent">
                添加番剧
              </DialogPrimitive.Title>
              <DialogPrimitive.Description className="sr-only">
                填写番剧信息并添加到追番列表
              </DialogPrimitive.Description>
              <DialogPrimitive.Close
                className={cn(
                  "flex size-7 items-center justify-center rounded-md",
                  "text-muted-foreground hover:text-foreground",
                  "hover:bg-chart-3/20 dark:hover:bg-chart-1/30",
                  "transition-colors duration-200",
                  "outline-none focus-visible:ring-2 focus-visible:ring-chart-3 dark:focus-visible:ring-chart-1"
                )}
              >
                <IconX className="size-4" />
              </DialogPrimitive.Close>
            </div>
          </div>

          {/* Form Content */}
          <form
            onSubmit={(e) => {
              e.preventDefault();
              e.stopPropagation();
              form.handleSubmit();
            }}
            className="relative flex flex-col max-h-[calc(90vh-80px)]"
          >
            <div className="flex-1 overflow-y-auto p-6 space-y-6 [&::-webkit-scrollbar]:hidden [-ms-overflow-style:none] [scrollbar-width:none]">
              {/* Subject Info Card */}
              {subject && (
                <div className="relative overflow-hidden rounded-xl border border-chart-3/20 dark:border-chart-1/20 bg-linear-to-br from-chart-3/5 via-transparent to-chart-1/5 dark:from-chart-3/10 dark:to-chart-1/10">
                  {/* Background blur effect from poster */}
                  {subject.image && (
                    <div
                      className="absolute inset-0 opacity-20 dark:opacity-30 blur-2xl scale-150"
                      style={{
                        backgroundImage: `url(${subject.image})`,
                        backgroundSize: "cover",
                        backgroundPosition: "center",
                      }}
                    />
                  )}

                  <div className="relative flex gap-4 p-4">
                    {/* Poster */}
                    <div className="shrink-0">
                      {subject.image ? (
                        <img
                          src={subject.image}
                          alt={subject.name_cn || subject.name}
                          className="w-20 h-28 object-cover rounded-lg shadow-lg ring-1 ring-white/10"
                        />
                      ) : (
                        <div className="w-20 h-28 rounded-lg bg-chart-3/20 dark:bg-chart-1/20 flex items-center justify-center">
                          <IconDeviceTv className="size-8 text-chart-3/50 dark:text-chart-1/50" />
                        </div>
                      )}
                    </div>

                    {/* Info */}
                    <div className="flex-1 min-w-0 flex flex-col justify-between py-0.5">
                      <div className="space-y-1">
                        <h3 className="font-semibold text-foreground truncate">
                          {subject.name_cn || subject.name}
                        </h3>
                        {subject.name_cn && subject.name !== subject.name_cn && (
                          <p className="text-xs text-muted-foreground truncate">
                            {subject.name}
                          </p>
                        )}
                      </div>

                      {/* Tags */}
                      <div className="flex flex-wrap gap-1.5 mt-2">
                        {subject.date && (
                          <span className="inline-flex items-center gap-1 px-2 py-0.5 rounded-md text-xs bg-chart-3/15 dark:bg-chart-3/20 text-chart-3 dark:text-chart-3">
                            <IconCalendar className="size-3" />
                            {subject.date}
                          </span>
                        )}
                        {subject.eps && subject.eps > 0 && (
                          <span className="inline-flex items-center gap-1 px-2 py-0.5 rounded-md text-xs bg-chart-1/15 dark:bg-chart-1/20 text-chart-1 dark:text-chart-1">
                            <IconStack2 className="size-3" />
                            {subject.eps} 话
                          </span>
                        )}
                        {parsedTitles.season > 1 && (
                          <span className="inline-flex items-center gap-1 px-2 py-0.5 rounded-md text-xs bg-chart-5/15 dark:bg-chart-5/20 text-chart-5 dark:text-chart-5">
                            第 {parsedTitles.season} 季
                          </span>
                        )}
                        {subject.platform && (
                          <span className="inline-flex items-center gap-1 px-2 py-0.5 rounded-md text-xs bg-chart-2/15 dark:bg-chart-2/20 text-chart-2 dark:text-chart-2">
                            {subject.platform}
                          </span>
                        )}
                        <span className={cn(
                          "inline-flex items-center gap-1 px-2 py-0.5 rounded-md text-xs",
                          isFinished
                            ? "bg-emerald-500/15 dark:bg-emerald-500/20 text-emerald-600 dark:text-emerald-400"
                            : "bg-amber-500/15 dark:bg-amber-500/20 text-amber-600 dark:text-amber-400"
                        )}>
                          {isFinished ? (
                            <>
                              <IconCircleCheck className="size-3" />
                              已完结
                            </>
                          ) : (
                            <>
                              <IconPlayerPlay className="size-3" />
                              放送中
                            </>
                          )}
                        </span>
                      </div>
                    </div>
                  </div>
                </div>
              )}

              <FieldGroup>
                {/* TMDB Matcher */}
                <Field>
                  <FieldLabel>
                    <IconMovie className="size-4 text-chart-3 dark:text-chart-1" />
                    TMDB 匹配
                  </FieldLabel>
                  <TmdbMatcher
                    value={selectedTmdb}
                    onChange={setSelectedTmdb}
                    initialKeyword={parseBgmtvName(subject?.name_cn || subject?.name || "").name}
                  />
                </Field>

                {/* Episode Offset */}
                <form.Field name="episode_offset">
                  {(field) => (
                    <Field>
                      <FieldLabel htmlFor={field.name}>
                        <IconHash className="size-4 text-chart-3 dark:text-chart-1" />
                        集数偏移
                      </FieldLabel>
                      <Input
                        id={field.name}
                        name={field.name}
                        type="number"
                        value={field.state.value}
                        onBlur={field.handleBlur}
                        onChange={(e) => field.handleChange(parseInt(e.target.value) || 0)}
                      />
                    </Field>
                  )}
                </form.Field>

                {/* Save Path */}
                <form.Field name="save_path">
                  {(field) => (
                    <Field>
                      <FieldLabel htmlFor={field.name}>
                        <IconFolder className="size-4 text-chart-3 dark:text-chart-1" />
                        保存路径
                      </FieldLabel>
                      <Input
                        id={field.name}
                        name={field.name}
                        value={field.state.value}
                        onBlur={field.handleBlur}
                        onChange={(e) => field.handleChange(e.target.value)}
                        placeholder="留空使用默认路径"
                      />
                    </Field>
                  )}
                </form.Field>

                {/* RSS Entries */}
                <form.Field name="rss_entries">
                  {(field) => (
                    <Field>
                      <div className="flex items-center justify-between">
                        <FieldLabel>
                          <IconRss className="size-4 text-chart-3 dark:text-chart-1" />
                          RSS 订阅地址
                        </FieldLabel>
                        <Button
                          type="button"
                          variant="outline"
                          size="sm"
                          onClick={() => setMikanModalOpen(true)}
                          className="h-7 gap-1.5 border-chart-3/30 dark:border-chart-1/30 hover:bg-chart-3/10 dark:hover:bg-chart-1/20"
                        >
                          <IconSearch className="size-3.5" />
                          Mikan 搜索
                        </Button>
                      </div>
                      <div className="space-y-3">
                        {field.state.value.map((entry, index) => (
                          <div key={index} className="space-y-2 p-3 rounded-lg border border-chart-3/20 dark:border-chart-1/20 bg-chart-3/5 dark:bg-chart-1/5">
                            <div className="flex gap-2">
                              <Input
                                value={entry.url}
                                onChange={(e) => {
                                  const newEntries = [...field.state.value];
                                  newEntries[index] = { ...entry, url: e.target.value };
                                  field.handleChange(newEntries);
                                }}
                                placeholder="RSS 订阅地址"
                                className="flex-1"
                              />
                              <Button
                                type="button"
                                variant="outline"
                                size="icon"
                                onClick={() => {
                                  const newEntries = field.state.value.filter((_: RssEntry, i: number) => i !== index);
                                  field.handleChange(newEntries);
                                }}
                                className="shrink-0 border-destructive/30 hover:bg-destructive/10 hover:text-destructive"
                              >
                                <IconTrash className="size-4" />
                              </Button>
                            </div>
                            {/* Filter Tags */}
                            <div className="flex flex-wrap gap-1.5 items-center">
                              {entry.filters.map((filter, filterIndex) => (
                                <span
                                  key={filterIndex}
                                  className="inline-flex items-center gap-1 px-2 py-0.5 rounded-md text-xs bg-chart-1/20 dark:bg-chart-3/20 text-chart-1 dark:text-chart-3"
                                >
                                  <code>{filter}</code>
                                  <button
                                    type="button"
                                    onClick={() => {
                                      const newEntries = [...field.state.value];
                                      newEntries[index] = {
                                        ...entry,
                                        filters: entry.filters.filter((_: string, fi: number) => fi !== filterIndex),
                                      };
                                      field.handleChange(newEntries);
                                    }}
                                    className="hover:text-destructive"
                                  >
                                    <IconX className="size-3" />
                                  </button>
                                </span>
                              ))}
                              <Input
                                placeholder="输入正则过滤..."
                                className="h-6 w-32 text-xs px-2"
                                onKeyDown={(e) => {
                                  if (e.key === "Enter") {
                                    e.preventDefault();
                                    const input = e.currentTarget;
                                    const value = input.value.trim();
                                    if (value && !entry.filters.includes(value)) {
                                      const newEntries = [...field.state.value];
                                      newEntries[index] = {
                                        ...entry,
                                        filters: [...entry.filters, value],
                                      };
                                      field.handleChange(newEntries);
                                      input.value = "";
                                    }
                                  }
                                }}
                              />
                            </div>
                          </div>
                        ))}
                        <Button
                          type="button"
                          variant="outline"
                          onClick={() => field.handleChange([...field.state.value, { url: "", filters: [] }])}
                          className="w-full gap-2 border-dashed border-chart-3/30 dark:border-chart-1/30 hover:bg-chart-3/10 dark:hover:bg-chart-1/20"
                        >
                          <IconPlus className="size-4" />
                          添加 RSS 地址
                        </Button>
                      </div>
                      <MikanRssModal
                        open={mikanModalOpen}
                        onOpenChange={setMikanModalOpen}
                        onSelect={(rssUrls) => {
                          const existingUrls = new Set(field.state.value.map((e: RssEntry) => e.url));
                          const newEntries = rssUrls
                            .filter((url) => !existingUrls.has(url))
                            .map((url) => ({ url, filters: [] }));
                          if (newEntries.length > 0) {
                            field.handleChange([...field.state.value, ...newEntries]);
                          }
                        }}
                        initialKeyword={parsedTitles.japanese || parsedTitles.chinese}
                      />
                    </Field>
                  )}
                </form.Field>

                {/* Auto Download Toggle */}
                <form.Field name="auto_download">
                  {(field) => (
                    <Field orientation="horizontal">
                      <FieldLabel
                        htmlFor={field.name}
                        className="flex-1 cursor-pointer"
                      >
                        <div className="flex items-center gap-2">
                          <IconDownload className="size-4 text-chart-3 dark:text-chart-1" />
                          自动下载
                        </div>
                      </FieldLabel>
                      <button
                        id={field.name}
                        type="button"
                        role="switch"
                        aria-checked={field.state.value}
                        onClick={() => field.handleChange(!field.state.value)}
                        className={cn(
                          "relative inline-flex h-6 w-11 shrink-0 cursor-pointer items-center rounded-full transition-colors duration-200",
                          "focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-chart-3 dark:focus-visible:ring-chart-1 focus-visible:ring-offset-2",
                          field.state.value
                            ? "bg-linear-to-r from-chart-3 to-chart-1"
                            : "bg-muted"
                        )}
                      >
                        <span
                          className={cn(
                            "pointer-events-none block size-5 rounded-full bg-white shadow-lg ring-0 transition-transform duration-200",
                            field.state.value ? "translate-x-5" : "translate-x-0.5"
                          )}
                        />
                      </button>
                    </Field>
                  )}
                </form.Field>
              </FieldGroup>
            </div>

            {/* Footer */}
            <div className="relative shrink-0 border-t border-chart-3/30 dark:border-chart-1/20 p-4 bg-linear-to-br from-white/95 via-white/90 to-chart-3/10 dark:from-zinc-900/95 dark:via-zinc-900/90 dark:to-chart-1/20">
              <div className="flex justify-end gap-3">
                <DialogPrimitive.Close
                  render={
                    <Button
                      type="button"
                      variant="outline"
                      className="border-chart-3/30 dark:border-chart-1/30 hover:bg-chart-3/10 dark:hover:bg-chart-1/20"
                    />
                  }
                >
                  取消
                </DialogPrimitive.Close>
                <form.Subscribe
                  selector={(state) => [state.canSubmit, state.isSubmitting]}
                >
                  {([canSubmit, isSubmitting]) => (
                    <Button
                      type="submit"
                      disabled={!canSubmit || isSubmitting}
                      className={cn(
                        "gap-2 bg-linear-to-r from-chart-3 to-chart-1 text-white",
                        "shadow-lg shadow-chart-1/30",
                        "hover:opacity-90 disabled:opacity-50"
                      )}
                    >
                      {isSubmitting ? (
                        <>
                          <IconLoader2 className="size-4 animate-spin" />
                          添加中...
                        </>
                      ) : (
                        <>
                          <IconSparkles className="size-4" />
                          添加番剧
                        </>
                      )}
                    </Button>
                  )}
                </form.Subscribe>
              </div>
            </div>
          </form>
        </DialogPrimitive.Popup>
      </DialogPrimitive.Portal>
    </DialogPrimitive.Root>
  );
}
