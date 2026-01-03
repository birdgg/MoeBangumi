import * as React from "react";
import { useForm } from "@tanstack/react-form";
import { toast } from "sonner";
import { cn } from "@/lib/utils";
import {
  useCreateBangumi,
  useUpdateBangumi,
  useGetBangumiById,
  useEpisodes,
} from "../../hooks/use-bangumi";
import { Input } from "@/components/ui/input";
import { Button } from "@/components/ui/button";
import { Field, FieldLabel, FieldGroup } from "@/components/ui/field";
import {
  AnimatedModal,
  AnimatedModalClose,
  AnimatedModalTitle,
} from "@/components/ui/animated-modal";
import {
  IconX,
  IconSparkles,
  IconEdit,
  IconLoader2,
  IconHash,
  IconMovie,
  IconRss,
  IconSearch,
  IconPlus,
} from "@tabler/icons-react";

import { TmdbMatcher } from "./tmdb-matcher";
import { MikanRssModal } from "../mikan-rss-modal";
import { BangumiInfoCard } from "./bangumi-info-card";

import type { BangumiModalProps, RssFormEntry } from "./types";
import { normalizePlatform, rssToFormEntry, formEntryToApiEntry } from "./utils";
import { RssEntryItem } from "./rss-entry-item";
import { AutoCompleteToggle } from "./auto-complete-toggle";

export function BangumiModal({
  open,
  onOpenChange,
  mode,
  data,
  onSuccess,
}: BangumiModalProps) {
  const isEdit = mode === "edit";
  const createBangumi = useCreateBangumi();
  const updateBangumi = useUpdateBangumi();

  // For edit mode, fetch the full bangumi data including RSS entries
  const { data: bangumiWithRss, isLoading } = useGetBangumiById(
    open && isEdit && data.id ? data.id : 0
  );

  // For add mode, fetch episodes to determine if finished and episode offset
  const { data: episodes } = useEpisodes(
    open && !isEdit ? data.bgmtvId : 0
  );

  // Calculate episode offset from episodes data (add mode only)
  // offset = sort - ep, used to convert RSS episode number to season-relative episode
  const calculatedEpisodeOffset = React.useMemo(() => {
    if (isEdit || !episodes || episodes.length === 0) return data.episodeOffset ?? 0;
    const first = episodes[0];
    const ep = first.ep ?? first.sort;
    return Math.floor(first.sort - ep);
  }, [isEdit, episodes, data.episodeOffset]);

  const [selectedTmdbId, setSelectedTmdbId] = React.useState<number | null>(null);
  const [mikanModalOpen, setMikanModalOpen] = React.useState(false);

  const form = useForm({
    defaultValues: {
      episode_offset: 0,
      auto_complete: true,
      rss_entries: [] as RssFormEntry[],
    },
    onSubmit: async ({ value }) => {
      try {
        if (isEdit && data.id) {
          await updateBangumi.mutateAsync({
            path: { id: data.id },
            body: {
              episode_offset: value.episode_offset,
              auto_complete: value.auto_complete,
              rss_entries: value.rss_entries.map(formEntryToApiEntry),
            },
          });
          toast.success("保存成功", {
            description: `「${data.titleChinese}」已更新`,
          });
        } else {
          // Validate required fields
          if (!data.airDate) {
            toast.error("创建失败", { description: "首播日期是必填项" });
            return;
          }
          if (data.airWeek === null || data.airWeek === undefined) {
            toast.error("创建失败", { description: "播出星期是必填项" });
            return;
          }

          await createBangumi.mutateAsync({
            body: {
              metadata: {
                title_chinese: data.titleChinese,
                title_japanese: data.titleJapanese || null,
                year: data.year || new Date().getFullYear(),
                bgmtv_id: data.bgmtvId,
                tmdb_id: selectedTmdbId ?? data.tmdbId ?? null,
                poster_url: data.posterUrl || null,
                air_date: data.airDate,
                air_week: data.airWeek,
                total_episodes: data.totalEpisodes || 0,
                platform: normalizePlatform(data.platform),
                season: data.season ?? 1,
              },
              episode_offset: value.episode_offset,
              auto_complete: value.auto_complete,
              source_type: data.sourceType || "webrip",
              rss_entries: value.rss_entries.map(formEntryToApiEntry),
            },
          });
          toast.success("添加成功", {
            description: `「${data.titleChinese}」已添加到追番列表`,
          });
        }
        onSuccess?.();
        resetForm();
        onOpenChange(false);
      } catch (error) {
        const message = error instanceof Error ? error.message : "未知错误";
        toast.error(isEdit ? "保存失败" : "添加失败", {
          description: message,
        });
      }
    },
  });

  const resetForm = React.useCallback(() => {
    form.reset();
    setSelectedTmdbId(null);
    setMikanModalOpen(false);
  }, [form]);

  const handleOpenChange = React.useCallback(
    (newOpen: boolean) => {
      if (!newOpen) {
        resetForm();
      }
      onOpenChange(newOpen);
    },
    [resetForm, onOpenChange]
  );

  // Set form values when modal opens or data changes
  React.useEffect(() => {
    if (!open) return;

    if (isEdit) {
      // Edit mode: wait for API data
      if (bangumiWithRss && bangumiWithRss.id === data.id) {
        form.setFieldValue("episode_offset", bangumiWithRss.episode_offset);
        form.setFieldValue("auto_complete", bangumiWithRss.auto_complete);
        form.setFieldValue(
          "rss_entries",
          bangumiWithRss.rss_entries.map(rssToFormEntry)
        );
      }
    } else {
      // Add mode: use data directly
      form.setFieldValue("episode_offset", calculatedEpisodeOffset);
      form.setFieldValue("auto_complete", data.autoComplete ?? true);

      if (data.rssEntries) {
        form.setFieldValue("rss_entries", data.rssEntries.map(rssToFormEntry));
      }
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [open, isEdit, data, bangumiWithRss, calculatedEpisodeOffset]);

  const searchKeyword = data.titleChinese || data.titleJapanese || "";

  return (
    <AnimatedModal
      open={open}
      onOpenChange={handleOpenChange}
      shifted={mikanModalOpen}
      className={isEdit ? "max-w-xl" : undefined}
    >
      {/* Decorative elements - Top right glow */}
      <div className="pointer-events-none absolute -right-20 -top-20 size-40 rounded-full bg-linear-to-br from-chart-3/30 to-chart-1/30 blur-3xl dark:from-chart-3/20 dark:to-chart-1/20" />

      {/* Bottom backlight - subtle glow (add mode only) */}
      {!isEdit && (
        <div className="pointer-events-none absolute -bottom-20 left-1/2 -translate-x-1/2 w-full h-20 -z-10">
          <div className="absolute inset-0 bg-linear-to-t from-chart-1/25 via-chart-3/10 to-transparent blur-2xl dark:from-chart-1/20 dark:via-chart-3/10" />
          <div className="absolute inset-x-[25%] inset-y-0 bg-linear-to-t from-chart-3/30 to-transparent blur-xl dark:from-chart-3/25 animate-pulse animation-duration-[4s]" />
        </div>
      )}

      {/* Edit mode: bottom left glow */}
      {isEdit && (
        <div className="pointer-events-none absolute -left-16 -bottom-16 size-32 rounded-full bg-linear-to-tr from-chart-1/20 to-chart-5/20 blur-2xl dark:from-chart-1/15 dark:to-chart-5/15" />
      )}

      {/* Header */}
      <div className="relative border-b border-chart-3/30 dark:border-chart-1/20 px-4 py-3">
        <div className="flex items-center gap-2.5">
          <div
            className={cn(
              "flex size-8 items-center justify-center rounded-lg bg-linear-to-br from-chart-3 to-chart-1 text-white shadow-md shadow-chart-1/30",
              isEdit && "relative overflow-hidden"
            )}
          >
            {isEdit ? (
              <IconEdit className="size-4 relative z-10" />
            ) : (
              <IconSparkles className="size-4" />
            )}
          </div>
          <AnimatedModalTitle className="flex-1 text-base font-semibold bg-linear-to-r from-chart-3 via-chart-1 to-chart-5 bg-clip-text text-transparent">
            {isEdit ? "编辑番剧" : "添加番剧"}
          </AnimatedModalTitle>
          <AnimatedModalClose
            className={cn(
              "flex size-7 items-center justify-center rounded-md",
              "text-muted-foreground hover:text-foreground",
              "hover:bg-chart-3/20 dark:hover:bg-chart-1/30",
              "transition-colors duration-200",
              "outline-none focus-visible:ring-2 focus-visible:ring-chart-3 dark:focus-visible:ring-chart-1"
            )}
          >
            <IconX className="size-4" />
          </AnimatedModalClose>
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
        <div
          className={cn(
            "flex-1 overflow-y-auto p-6 pt-4 space-y-6 [&::-webkit-scrollbar]:hidden [-ms-overflow-style:none] [scrollbar-width:none]",
            isEdit && "animate-modal-content"
          )}
        >
          {isEdit && isLoading ? (
            <div className="flex items-center justify-center py-12">
              <IconLoader2 className="size-8 animate-spin text-chart-1" />
            </div>
          ) : (
            <>
              {/* Bangumi Info Card */}
              <BangumiInfoCard
                posterUrl={data.posterUrl}
                titleChinese={data.titleChinese}
                titleJapanese={data.titleJapanese}
                year={data.year}
                broadcastSeason={
                  data.airDate
                    ? Math.ceil(parseInt(data.airDate.split("-")[1]) / 3)
                    : undefined
                }
                totalEpisodes={data.totalEpisodes}
                seasonNumber={data.season}
                platform={data.platform}
                bgmtvId={data.bgmtvId}
                className={isEdit ? "animate-modal-content [animation-delay:0.2s]" : undefined}
              />

              <FieldGroup
                className={
                  isEdit
                    ? "animate-modal-content [animation-delay:0.25s]"
                    : undefined
                }
              >
                {/* TMDB Matcher */}
                <Field>
                  <FieldLabel>
                    <IconMovie className="size-4 text-chart-3 dark:text-chart-1" />
                    TMDB 匹配
                  </FieldLabel>
                  <TmdbMatcher
                    onChange={(show) => setSelectedTmdbId(show?.id ?? null)}
                    keyword={data.titleJapanese || data.titleChinese}
                    initialTmdbId={data.tmdbId ?? undefined}
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
                        onChange={(e) =>
                          field.handleChange(parseInt(e.target.value) || 0)
                        }
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
                        {(Array.isArray(field.state.value) ? field.state.value : []).map(
                          (entry, index) => (
                            <RssEntryItem
                              key={index}
                              entry={entry}
                              onUpdate={(updatedEntry) => {
                                const newEntries = [...field.state.value];
                                newEntries[index] = updatedEntry;
                                field.handleChange(newEntries);
                              }}
                              onRemove={() => {
                                const newEntries = field.state.value.filter(
                                  (_, i) => i !== index
                                );
                                field.handleChange(newEntries);
                              }}
                            />
                          )
                        )}
                        <Button
                          type="button"
                          variant="outline"
                          onClick={() =>
                            field.handleChange([
                              ...field.state.value,
                              { url: "", filters: [], include_filters: [], subtitle_group: null },
                            ])
                          }
                          className="w-full gap-2 border-dashed border-chart-3/30 dark:border-chart-1/30 hover:bg-chart-3/10 dark:hover:bg-chart-1/20"
                        >
                          <IconPlus className="size-4" />
                          添加 RSS 地址
                        </Button>
                      </div>
                      <MikanRssModal
                        open={mikanModalOpen}
                        onOpenChange={setMikanModalOpen}
                        onSelect={(selectedEntries) => {
                          const existingEntries = Array.isArray(field.state.value) ? field.state.value : [];
                          const existingUrls = new Set(
                            existingEntries.map((e) => e.url)
                          );
                          const newEntries = selectedEntries
                            .filter((entry) => !existingUrls.has(entry.url))
                            .map((entry) => ({
                              url: entry.url,
                              subtitle_group: entry.subtitle_group,
                              filters: entry.filters,
                              include_filters: entry.include_filters,
                            }));
                          if (newEntries.length > 0) {
                            field.handleChange([
                              ...existingEntries,
                              ...newEntries,
                            ]);
                          }
                        }}
                        initialKeyword={searchKeyword}
                        mikanId={data.mikanId ?? undefined}
                      />
                    </Field>
                  )}
                </form.Field>

                {/* Auto Complete Toggle */}
                <form.Field name="auto_complete">
                  {(field) => (
                    <AutoCompleteToggle
                      id={field.name}
                      value={field.state.value}
                      onChange={field.handleChange}
                    />
                  )}
                </form.Field>
              </FieldGroup>
            </>
          )}
        </div>

        {/* Footer */}
        <div className="relative shrink-0 border-t border-chart-3/30 dark:border-chart-1/20 p-4 bg-linear-to-br from-white/95 via-white/90 to-chart-3/10 dark:from-zinc-900/95 dark:via-zinc-900/90 dark:to-chart-1/20">
          <div className="flex justify-end gap-3">
            <form.Subscribe
              selector={(state) => [state.canSubmit, state.isSubmitting]}
            >
              {([canSubmit, isSubmitting]) => (
                <Button
                  type="submit"
                  disabled={!canSubmit || isSubmitting || (isEdit && isLoading)}
                  className={cn(
                    "gap-2 bg-linear-to-r from-chart-3 to-chart-1 text-white",
                    "shadow-lg shadow-chart-1/30",
                    "hover:opacity-90 disabled:opacity-50"
                  )}
                >
                  {isSubmitting ? (
                    <>
                      <IconLoader2 className="size-4 animate-spin" />
                      {isEdit ? "保存中..." : "添加中..."}
                    </>
                  ) : (
                    <>
                      {isEdit ? (
                        <IconEdit className="size-4" />
                      ) : (
                        <IconSparkles className="size-4" />
                      )}
                      {isEdit ? "保存" : "添加番剧"}
                    </>
                  )}
                </Button>
              )}
            </form.Subscribe>
          </div>
        </div>
      </form>
    </AnimatedModal>
  );
}
