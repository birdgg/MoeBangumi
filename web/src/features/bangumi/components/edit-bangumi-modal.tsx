import * as React from "react";
import { Dialog as DialogPrimitive } from "@base-ui/react/dialog";
import { useForm } from "@tanstack/react-form";
import { toast } from "sonner";
import { cn } from "@/lib/utils";
import { type Bangumi, type RssEntry, type Rss } from "@/lib/api";
import { useGetBangumiById, useUpdateBangumi } from "../hooks/use-bangumi";
import { Input } from "@/components/ui/input";
import { Button } from "@/components/ui/button";
import { Field, FieldLabel, FieldGroup } from "@/components/ui/field";
import {
  IconX,
  IconEdit,
  IconLoader2,
  IconDownload,
  IconFolder,
  IconHash,
  IconRss,
  IconSearch,
  IconPlus,
  IconTrash,
  IconStar,
  IconStarFilled,
} from "@tabler/icons-react";
import { MikanRssModal } from "./mikan-rss-modal";
import { BangumiInfoCard } from "./bangumi-info-card";

interface RssFormEntry {
  url: string;
  filters: string[];
  is_primary: boolean;
}

interface EditBangumiModalProps {
  open: boolean;
  onOpenChange: (open: boolean) => void;
  bangumi: Bangumi;
  onSuccess?: () => void;
}

// Convert Rss to RssFormEntry
function rssToFormEntry(rss: Rss): RssFormEntry {
  return {
    url: rss.url,
    filters: rss.exclude_filters,
    is_primary: rss.is_primary,
  };
}

// Convert RssFormEntry to RssEntry for API
function formEntryToRssEntry(entry: RssFormEntry): RssEntry {
  return {
    url: entry.url,
    filters: entry.filters,
    is_primary: entry.is_primary,
  };
}

export function EditBangumiModal({
  open,
  onOpenChange,
  bangumi,
  onSuccess,
}: EditBangumiModalProps) {
  const updateBangumi = useUpdateBangumi();
  const { data: bangumiWithRss, isLoading } = useGetBangumiById(
    open ? bangumi.id : 0
  );
  const [mikanModalOpen, setMikanModalOpen] = React.useState(false);

  const form = useForm({
    defaultValues: {
      episode_offset: 0,
      auto_download: true,
      save_path: "",
      rss_entries: [] as RssFormEntry[],
    },
    onSubmit: async ({ value }) => {
      try {
        await updateBangumi.mutateAsync({
          path: { id: bangumi.id },
          body: {
            episode_offset: value.episode_offset,
            auto_download: value.auto_download,
            save_path: value.save_path || null,
            rss_entries: value.rss_entries.map(formEntryToRssEntry),
          },
        });
        toast.success("保存成功", {
          description: `「${bangumi.title_chinese}」已更新`,
        });
        onSuccess?.();
        onOpenChange(false);
      } catch (error) {
        const message = error instanceof Error ? error.message : "未知错误";
        toast.error("保存失败", {
          description: message,
        });
      }
    },
  });

  // Set form values when data loads
  React.useEffect(() => {
    // Check that loaded data matches the current bangumi to avoid race conditions
    if (open && bangumiWithRss && bangumiWithRss.id === bangumi.id) {
      form.setFieldValue("episode_offset", bangumiWithRss.episode_offset);
      form.setFieldValue("auto_download", bangumiWithRss.auto_download);
      form.setFieldValue("save_path", bangumiWithRss.save_path || "");
      form.setFieldValue(
        "rss_entries",
        bangumiWithRss.rss_entries.map(rssToFormEntry)
      );
    }
  }, [open, bangumiWithRss, bangumi.id, form]);

  return (
    <DialogPrimitive.Root open={open} onOpenChange={onOpenChange}>
      <DialogPrimitive.Portal>
        {/* Backdrop */}
        <DialogPrimitive.Backdrop
          className={cn(
            "fixed inset-0 z-50",
            "bg-black/20 dark:bg-black/40",
            "data-[state=open]:animate-modal-backdrop-in",
            "data-[state=closed]:animate-out data-[state=closed]:fade-out-0",
            "duration-300"
          )}
        />

        {/* Modal */}
        <DialogPrimitive.Popup
          className={cn(
            "fixed left-1/2 top-1/2 z-50 -translate-x-1/2 -translate-y-1/2",
            "w-[calc(100%-2rem)] max-w-xl",
            "max-h-[90vh] overflow-hidden",
            "rounded-2xl",
            "bg-linear-to-br from-white/95 via-white/90 to-chart-3/10",
            "dark:from-zinc-900/95 dark:via-zinc-900/90 dark:to-chart-1/20",
            "border border-chart-3/30 dark:border-chart-1/30",
            "shadow-2xl shadow-chart-3/20 dark:shadow-chart-1/50",
            "backdrop-blur-xl",
            "data-[state=open]:animate-modal-popup-in",
            "data-[state=closed]:animate-modal-popup-out",
            "outline-none",
            "will-change-transform",
            mikanModalOpen &&
            "left-[25%]! scale-95 opacity-80 transition-all duration-300"
          )}
        >
          {/* Decorative elements */}
          <div className="pointer-events-none absolute -right-20 -top-20 size-40 rounded-full bg-linear-to-br from-chart-3/30 to-chart-1/30 blur-3xl dark:from-chart-3/20 dark:to-chart-1/20 animate-modal-glow" />
          <div className="pointer-events-none absolute -left-16 -bottom-16 size-32 rounded-full bg-linear-to-tr from-chart-1/20 to-chart-5/20 blur-2xl dark:from-chart-1/15 dark:to-chart-5/15 animate-modal-glow [animation-delay:2s]" />

          {/* Header */}
          <div className="relative border-b border-chart-3/30 dark:border-chart-1/20 px-4 py-3 animate-modal-header">
            <div className="flex items-center gap-2.5">
              <div className="relative flex size-8 items-center justify-center rounded-lg bg-linear-to-br from-chart-3 to-chart-1 text-white shadow-md shadow-chart-1/30 overflow-hidden">
                <IconEdit className="size-4 relative z-10" />
                <div className="absolute inset-0 animate-shimmer" />
              </div>
              <DialogPrimitive.Title className="flex-1 text-base font-semibold bg-linear-to-r from-chart-3 via-chart-1 to-chart-5 bg-clip-text text-transparent">
                编辑番剧
              </DialogPrimitive.Title>
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
            <div className="flex-1 overflow-y-auto p-6 space-y-6 [&::-webkit-scrollbar]:hidden [-ms-overflow-style:none] [scrollbar-width:none] animate-modal-content">
              {isLoading ? (
                <div className="flex items-center justify-center py-12">
                  <IconLoader2 className="size-8 animate-spin text-chart-1" />
                </div>
              ) : (
                <>
                  {/* Bangumi Info Header */}
                  <BangumiInfoCard
                    posterUrl={bangumi.poster_url}
                    titleChinese={bangumi.title_chinese}
                    titleJapanese={bangumi.title_original_japanese}
                    year={bangumi.year}
                    broadcastSeason={bangumi.season}
                    totalEpisodes={bangumi.total_episodes}
                    platform={bangumi.kind}
                    isFinished={bangumi.finished}
                    className="animate-modal-content [animation-delay:0.2s]"
                  />

                  <FieldGroup className="animate-modal-content [animation-delay:0.25s]">
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
                              <div
                                key={index}
                                className={cn(
                                  "space-y-2 p-3 rounded-lg border bg-chart-3/5 dark:bg-chart-1/5",
                                  entry.is_primary
                                    ? "border-chart-1/50 dark:border-chart-1/50 ring-1 ring-chart-1/20"
                                    : "border-chart-3/20 dark:border-chart-1/20"
                                )}
                              >
                                <div className="flex gap-2 items-center">
                                  {/* Primary Badge */}
                                  {entry.is_primary && (
                                    <span className="shrink-0 inline-flex items-center gap-1 px-2 py-0.5 rounded-md text-xs bg-chart-1/20 text-chart-1 font-medium">
                                      <IconStarFilled className="size-3" />
                                      主RSS
                                    </span>
                                  )}
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
                                      className="inline-flex items-center gap-1 px-2 py-0.5 rounded-md text-xs bg-chart-1/20 dark:bg-chart-3/20 text-chart-1 dark:text-chart-3"
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
                                  { url: "", filters: [], is_primary: false },
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
                            onSelect={(rssUrls) => {
                              const existingUrls = new Set(
                                field.state.value.map((e) => e.url)
                              );
                              const hasPrimary = field.state.value.some(
                                (e) => e.is_primary
                              );
                              const newEntries = rssUrls
                                .filter((url) => !existingUrls.has(url))
                                .map((url, index) => ({
                                  url,
                                  filters: [],
                                  is_primary: !hasPrimary && index === 0,
                                }));
                              if (newEntries.length > 0) {
                                field.handleChange([
                                  ...field.state.value,
                                  ...newEntries,
                                ]);
                              }
                            }}
                            initialKeyword={
                              bangumi.title_japanese || bangumi.title_chinese
                            }
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
                            onClick={() =>
                              field.handleChange(!field.state.value)
                            }
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
            <div className="relative shrink-0 border-t border-chart-3/30 dark:border-chart-1/20 p-4 bg-linear-to-br from-white/95 via-white/90 to-chart-3/10 dark:from-zinc-900/95 dark:via-zinc-900/90 dark:to-chart-1/20 animate-modal-content [animation-delay:0.2s]">
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
                      disabled={!canSubmit || isSubmitting || isLoading}
                      className={cn(
                        "gap-2 bg-linear-to-r from-chart-3 to-chart-1 text-white",
                        "shadow-lg shadow-chart-1/30",
                        "hover:opacity-90 disabled:opacity-50"
                      )}
                    >
                      {isSubmitting ? (
                        <>
                          <IconLoader2 className="size-4 animate-spin" />
                          保存中...
                        </>
                      ) : (
                        <>
                          <IconEdit className="size-4" />
                          保存
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
