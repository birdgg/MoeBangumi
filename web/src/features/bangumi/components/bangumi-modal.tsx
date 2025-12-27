import * as React from "react";
import { useForm } from "@tanstack/react-form";
import { toast } from "sonner";
import { cn } from "@/lib/utils";
import { type TvShow, type RssEntry as ApiRssEntry, type Rss, type Platform } from "@/lib/api";
import {
  useCreateBangumi,
  useUpdateBangumi,
  useGetBangumiById,
  useEpisodes,
} from "../hooks/use-bangumi";
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
  IconDownload,
  IconHash,
  IconMovie,
  IconRss,
  IconSearch,
  IconPlus,
  IconTrash,
  IconStar,
  IconStarFilled,
  IconMagnet,
  IconUpload,
  IconUsers,
} from "@tabler/icons-react";
import { TmdbMatcher } from "./tmdb-matcher";
import { MikanRssModal } from "./mikan-rss-modal";
import { TorrentSearchModal } from "./torrent-search-modal";
import { BangumiInfoCard } from "./bangumi-info-card";

// Unified data interface for both add and edit modes
export interface BangumiModalData {
  // Identity
  id?: number; // Internal DB id (edit mode only)
  bgmtvId: number; // BGM.tv ID
  tmdbId?: number | null;

  // Titles
  titleChinese: string;
  titleJapanese?: string | null;
  titleOriginalChinese?: string | null;
  titleOriginalJapanese?: string | null;

  // Metadata
  posterUrl?: string | null;
  year?: number | null;
  season?: number;
  totalEpisodes?: number;
  platform?: string | null;
  airDate?: string | null;
  airWeek?: number | null;
  finished?: boolean;
  sourceType?: "webrip" | "bdrip";

  // Form data (for edit mode prefill)
  episodeOffset?: number;
  autoDownload?: boolean;
  rssEntries?: Rss[];
}

interface RssFormEntry {
  url: string;
  filters: string[];
  is_primary: boolean;
  group?: string | null;
}

interface BangumiModalProps {
  open: boolean;
  onOpenChange: (open: boolean) => void;
  mode: "add" | "edit";
  data: BangumiModalData;
  onSuccess?: () => void;
}

// Map external platform strings to our Platform enum
function normalizePlatform(platform: string | null | undefined): Platform {
  if (!platform) return "tv";
  const lower = platform.toLowerCase();
  if (lower === "tv" || lower === "web") return "tv";
  if (lower === "movie" || lower === "剧场版" || lower === "劇場版") return "movie";
  if (lower === "ova" || lower === "oad") return "ova";
  return "tv";
}

// Convert Rss to RssFormEntry
function rssToFormEntry(rss: Rss): RssFormEntry {
  return {
    url: rss.url,
    filters: rss.exclude_filters,
    is_primary: rss.is_primary,
    group: rss.group,
  };
}

// Convert RssFormEntry to ApiRssEntry
function formEntryToApiEntry(entry: RssFormEntry): ApiRssEntry {
  return {
    url: entry.url,
    filters: entry.filters,
    is_primary: entry.is_primary,
    group: entry.group,
  };
}

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
  const calculatedEpisodeOffset = React.useMemo(() => {
    if (isEdit || !episodes || episodes.length === 0) return data.episodeOffset ?? 0;
    return episodes[0].sort;
  }, [isEdit, episodes, data.episodeOffset]);

  // Calculate if bangumi has finished airing (add mode only)
  const calculatedIsFinished = React.useMemo(() => {
    if (isEdit) return data.finished ?? false;
    if (!episodes || !data.totalEpisodes) return false;
    return episodes.length >= data.totalEpisodes;
  }, [isEdit, episodes, data.totalEpisodes, data.finished]);

  const [selectedTmdb, setSelectedTmdb] = React.useState<TvShow | null>(null);
  const [mikanModalOpen, setMikanModalOpen] = React.useState(false);
  const [torrentSearchModalOpen, setTorrentSearchModalOpen] =
    React.useState(false);
  const torrentFileInputRef = React.useRef<HTMLInputElement>(null);

  const form = useForm({
    defaultValues: {
      title_chinese: "",
      title_japanese: "",
      episode_offset: 0,
      auto_download: true,
      rss_entries: [] as RssFormEntry[],
      torrent: "",
      torrent_file: null as File | null,
    },
    onSubmit: async ({ value }) => {
      try {
        if (isEdit && data.id) {
          await updateBangumi.mutateAsync({
            path: { id: data.id },
            body: {
              episode_offset: value.episode_offset,
              auto_download: value.auto_download,
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
              title_chinese: value.title_chinese,
              title_japanese: value.title_japanese || null,
              title_original_chinese: data.titleOriginalChinese || data.titleChinese,
              title_original_japanese: data.titleOriginalJapanese || data.titleJapanese || null,
              year: data.year || new Date().getFullYear(),
              bgmtv_id: data.bgmtvId,
              tmdb_id: selectedTmdb?.id ?? null,
              poster_url: data.posterUrl || null,
              air_date: data.airDate,
              air_week: data.airWeek,
              total_episodes: data.totalEpisodes || 0,
              episode_offset: value.episode_offset,
              auto_download: value.auto_download,
              finished: calculatedIsFinished,
              platform: normalizePlatform(data.platform),
              season: data.season ?? 1,
              source_type: data.sourceType || "webrip",
              rss_entries: value.rss_entries.map(formEntryToApiEntry),
            },
          });
          toast.success("添加成功", {
            description: `「${value.title_chinese}」已添加到追番列表`,
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
    setSelectedTmdb(null);
    setMikanModalOpen(false);
    setTorrentSearchModalOpen(false);
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
        form.setFieldValue("title_chinese", data.titleChinese);
        form.setFieldValue("title_japanese", data.titleJapanese || "");
        form.setFieldValue("episode_offset", bangumiWithRss.episode_offset);
        form.setFieldValue("auto_download", bangumiWithRss.auto_download);
        form.setFieldValue(
          "rss_entries",
          bangumiWithRss.rss_entries.map(rssToFormEntry)
        );
        // Set existing TMDB if available
        if (data.tmdbId && !selectedTmdb) {
          // Note: We don't have full TvShow data, just the ID
          // The TmdbMatcher will need to handle this
        }
      }
    } else {
      // Add mode: use data directly
      form.setFieldValue("title_chinese", data.titleChinese);
      form.setFieldValue("title_japanese", data.titleJapanese || "");
      form.setFieldValue("episode_offset", calculatedEpisodeOffset);
      form.setFieldValue("auto_download", data.autoDownload ?? true);

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
      shifted={mikanModalOpen || torrentSearchModalOpen}
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
                isFinished={calculatedIsFinished}
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
                {/* Title inputs in one row */}
                <div className="grid grid-cols-2 gap-3">
                  <form.Field name="title_chinese">
                    {(field) => (
                      <Field>
                        <FieldLabel htmlFor={field.name}>中文标题</FieldLabel>
                        <Input
                          id={field.name}
                          name={field.name}
                          value={field.state.value}
                          onBlur={field.handleBlur}
                          onChange={(e) => field.handleChange(e.target.value)}
                          placeholder="中文标题"
                        />
                      </Field>
                    )}
                  </form.Field>
                  <form.Field name="title_japanese">
                    {(field) => (
                      <Field>
                        <FieldLabel htmlFor={field.name}>日文标题</FieldLabel>
                        <Input
                          id={field.name}
                          name={field.name}
                          value={field.state.value}
                          onBlur={field.handleBlur}
                          onChange={(e) => field.handleChange(e.target.value)}
                          placeholder="日文标题"
                        />
                      </Field>
                    )}
                  </form.Field>
                </div>

                {/* TMDB Matcher */}
                <form.Subscribe selector={(state) => state.values.title_chinese}>
                  {(titleChinese) => (
                    <Field>
                      <FieldLabel>
                        <IconMovie className="size-4 text-chart-3 dark:text-chart-1" />
                        TMDB 匹配
                      </FieldLabel>
                      <TmdbMatcher
                        value={selectedTmdb}
                        onChange={setSelectedTmdb}
                        keyword={titleChinese}
                      />
                    </Field>
                  )}
                </form.Subscribe>

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
                          {field.state.value.map((entry, index) => (
                            <div
                              key={index}
                              className={cn(
                                "space-y-2 p-3 rounded-lg border bg-chart-3/5 dark:bg-chart-1/5",
                                entry.is_primary
                                  ? "border-chart-1/50 dark:border-chart-1/50 ring-1 ring-chart-1/20"
                                  : "border-chart-3/20 dark:border-chart-1/20"
                              )}
                            >
                              {/* Header: Badges */}
                              <div className="flex items-center gap-2 flex-wrap">
                                {/* Primary Badge */}
                                {entry.is_primary && (
                                  <span className="shrink-0 inline-flex items-center gap-1 px-2 py-0.5 rounded-md text-xs bg-chart-1/20 text-chart-1 font-medium">
                                    <IconStarFilled className="size-3" />
                                    主RSS
                                  </span>
                                )}
                                {/* Group Badge with remove button or Input */}
                                {entry.group ? (
                                  <span className="shrink-0 inline-flex items-center gap-1 pl-2 pr-1.5 py-0.5 rounded-md text-xs bg-chart-3/20 dark:bg-chart-1/20 text-chart-3 dark:text-chart-1 font-medium">
                                    <IconUsers className="size-3" />
                                    {entry.group}
                                    <button
                                      type="button"
                                      onClick={() => {
                                        const newEntries = [...field.state.value];
                                        newEntries[index] = {
                                          ...entry,
                                          group: null,
                                        };
                                        field.handleChange(newEntries);
                                      }}
                                      className="flex items-center justify-center size-4 rounded-full hover:bg-chart-3/30 dark:hover:bg-chart-1/30 transition-colors"
                                    >
                                      <IconX className="size-3" />
                                    </button>
                                  </span>
                                ) : (
                                  <Input
                                    value={entry.group || ""}
                                    onChange={(e) => {
                                      const newEntries = [...field.state.value];
                                      newEntries[index] = {
                                        ...entry,
                                        group: e.target.value.trim() || null,
                                      };
                                      field.handleChange(newEntries);
                                    }}
                                    placeholder="字幕组名称（可选）"
                                    className="h-6 w-36 text-xs px-2"
                                  />
                                )}
                              </div>
                              {/* URL and Actions */}
                              <div className="flex gap-2 items-center">
                                <Input
                                  value={entry.url}
                                  onChange={(e) => {
                                    const newEntries = [...field.state.value];
                                    newEntries[index] = {
                                      ...entry,
                                      url: e.target.value,
                                    };
                                    field.handleChange(newEntries);
                                  }}
                                  placeholder="RSS 订阅地址"
                                  className="flex-1"
                                />
                                {/* Toggle Primary Button */}
                                <Button
                                  type="button"
                                  variant="outline"
                                  size="icon"
                                  onClick={() => {
                                    if (entry.is_primary) return;
                                    const newEntries = field.state.value.map(
                                      (e, i) => ({
                                        ...e,
                                        is_primary: i === index,
                                      })
                                    );
                                    field.handleChange(newEntries);
                                  }}
                                  className={cn(
                                    "shrink-0",
                                    entry.is_primary
                                      ? "border-chart-1/50 bg-chart-1/10 text-chart-1 cursor-default"
                                      : "border-chart-3/30 dark:border-chart-1/30 hover:bg-chart-3/10 dark:hover:bg-chart-1/20"
                                  )}
                                  title={
                                    entry.is_primary
                                      ? "当前为主RSS"
                                      : "设为主RSS"
                                  }
                                >
                                  {entry.is_primary ? (
                                    <IconStarFilled className="size-4" />
                                  ) : (
                                    <IconStar className="size-4" />
                                  )}
                                </Button>
                                {/* Delete Button */}
                                <Button
                                  type="button"
                                  variant="outline"
                                  size="icon"
                                  onClick={() => {
                                    const newEntries = field.state.value.filter(
                                      (_, i) => i !== index
                                    );
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
                                    className="inline-flex items-center gap-1 pl-2 pr-1.5 py-0.5 rounded-full text-xs font-medium bg-red-500/20 dark:bg-red-500/30 text-red-600 dark:text-red-400 border border-red-500/40 dark:border-red-500/50"
                                  >
                                    <code>{filter}</code>
                                    <button
                                      type="button"
                                      onClick={() => {
                                        const newEntries = [
                                          ...field.state.value,
                                        ];
                                        newEntries[index] = {
                                          ...entry,
                                          filters: entry.filters.filter(
                                            (_, fi) => fi !== filterIndex
                                          ),
                                        };
                                        field.handleChange(newEntries);
                                      }}
                                      className="flex items-center justify-center size-4 rounded-full hover:bg-red-500/30 transition-colors"
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
                                      if (
                                        value &&
                                        !entry.filters.includes(value)
                                      ) {
                                        const newEntries = [
                                          ...field.state.value,
                                        ];
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
                            onClick={() =>
                              field.handleChange([
                                ...field.state.value,
                                { url: "", filters: [], is_primary: false, group: null },
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
                          onSelect={(entries) => {
                            const existingUrls = new Set(
                              field.state.value.map((e) => e.url)
                            );
                            const hasPrimary = field.state.value.some(
                              (e) => e.is_primary
                            );
                            const newEntries = entries
                              .filter((entry) => !existingUrls.has(entry.url))
                              .map((entry, idx) => ({
                                url: entry.url,
                                group: entry.group,
                                filters: entry.filters,
                                is_primary: !hasPrimary && idx === 0,
                              }));
                            if (newEntries.length > 0) {
                              field.handleChange([
                                ...field.state.value,
                                ...newEntries,
                              ]);
                            }
                          }}
                          initialKeyword={searchKeyword}
                        />
                      </Field>
                    )}
                  </form.Field>

                {/* Torrent Input */}
                <form.Field name="torrent">
                  {(field) => (
                    <Field>
                      <div className="flex items-center justify-between">
                        <FieldLabel>
                          <IconMagnet className="size-4 text-chart-3 dark:text-chart-1" />
                          种子下载
                        </FieldLabel>
                        <Button
                          type="button"
                          variant="outline"
                          size="sm"
                          onClick={() => setTorrentSearchModalOpen(true)}
                          className="h-7 gap-1.5 border-chart-3/30 dark:border-chart-1/30 hover:bg-chart-3/10 dark:hover:bg-chart-1/20"
                        >
                          <IconSearch className="size-3.5" />
                          搜索种子
                        </Button>
                      </div>
                      <div className="space-y-3">
                        {/* Magnet/Torrent URL Input */}
                        <div className="space-y-2 p-3 rounded-lg border border-chart-3/20 dark:border-chart-1/20 bg-chart-3/5 dark:bg-chart-1/5">
                          <div className="flex gap-2 items-center">
                            <div className="relative flex-1">
                              <IconMagnet className="absolute left-3 top-1/2 -translate-y-1/2 size-4 text-muted-foreground" />
                              <Input
                                id={field.name}
                                name={field.name}
                                value={field.state.value}
                                onBlur={field.handleBlur}
                                onChange={(e) =>
                                  field.handleChange(e.target.value)
                                }
                                placeholder="粘贴磁力链接或种子URL..."
                                className="pl-9"
                              />
                            </div>
                            {/* File Upload Button */}
                            <input
                              ref={torrentFileInputRef}
                              type="file"
                              accept=".torrent"
                              className="hidden"
                              onChange={(e) => {
                                const file = e.target.files?.[0];
                                if (file) {
                                  form.setFieldValue("torrent_file", file);
                                  field.handleChange(file.name);
                                }
                              }}
                            />
                            <Button
                              type="button"
                              variant="outline"
                              size="icon"
                              onClick={() =>
                                torrentFileInputRef.current?.click()
                              }
                              className="shrink-0 border-chart-3/30 dark:border-chart-1/30 hover:bg-chart-3/10 dark:hover:bg-chart-1/20"
                              title="上传种子文件"
                            >
                              <IconUpload className="size-4" />
                            </Button>
                            {/* Clear Button */}
                            {field.state.value && (
                              <Button
                                type="button"
                                variant="outline"
                                size="icon"
                                onClick={() => {
                                  field.handleChange("");
                                  form.setFieldValue("torrent_file", null);
                                  if (torrentFileInputRef.current) {
                                    torrentFileInputRef.current.value = "";
                                  }
                                }}
                                className="shrink-0 border-destructive/30 hover:bg-destructive/10 hover:text-destructive"
                                title="清除"
                              >
                                <IconTrash className="size-4" />
                              </Button>
                            )}
                          </div>
                          {/* File name indicator */}
                          <form.Subscribe
                            selector={(state) => state.values.torrent_file}
                          >
                            {(torrentFile) =>
                              torrentFile && (
                                <div className="flex items-center gap-2 text-xs text-muted-foreground">
                                  <IconUpload className="size-3" />
                                  <span>已选择文件: {torrentFile.name}</span>
                                </div>
                              )
                            }
                          </form.Subscribe>
                        </div>
                        {/* Helper text */}
                        <p className="text-xs text-muted-foreground">
                          粘贴磁力链接或上传 .torrent 文件直接下载
                        </p>
                      </div>
                      <TorrentSearchModal
                        open={torrentSearchModalOpen}
                        onOpenChange={setTorrentSearchModalOpen}
                        onSelect={(magnetUrl) => {
                          field.handleChange(magnetUrl);
                          setTorrentSearchModalOpen(false);
                        }}
                        initialKeyword={searchKeyword}
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
                            field.state.value
                              ? "translate-x-5"
                              : "translate-x-0.5"
                          )}
                        />
                      </button>
                    </Field>
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
