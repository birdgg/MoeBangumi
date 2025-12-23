import * as React from "react";
import { Dialog as DialogPrimitive } from "@base-ui/react/dialog";
import { useForm } from "@tanstack/react-form";
import { cn } from "@/lib/utils";
import { parseBgmtvName } from "@/lib/parser";
import { type Subject, type CreateBangumi, type TmdbTvShow } from "@/lib/api";
import { useCreateBangumi } from "../hooks/use-bangumi";
import { Input } from "@/components/ui/input";
import { Button } from "@/components/ui/button";
import {
  Field,
  FieldLabel,
  FieldDescription,
  FieldError,
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
} from "@tabler/icons-react";
import { TmdbMatcher } from "./tmdb-matcher";

interface AddBangumiModalProps {
  open: boolean;
  onOpenChange: (open: boolean) => void;
  subject: Subject | null;
  onSuccess?: () => void;
}

export function AddBangumiModal({
  open,
  onOpenChange,
  subject,
  onSuccess,
}: AddBangumiModalProps) {
  const createBangumi = useCreateBangumi();
  const [selectedTmdb, setSelectedTmdb] = React.useState<TmdbTvShow | null>(null);

  const form = useForm({
    defaultValues: {
      title_chinese: subject?.name_cn || subject?.name || "",
      title_japanese: subject?.name || "",
      episode_offset: 0,
      auto_download: true,
      save_path: "",
    },
    onSubmit: async ({ value }) => {
      const request: CreateBangumi = {
        title_chinese: value.title_chinese,
        title_japanese: value.title_japanese || null,
        year: subject?.date ? parseInt(subject.date.split("-")[0]) : new Date().getFullYear(),
        bgmtv_id: subject?.id,
        tmdb_id: selectedTmdb?.id ?? null,
        poster_url: subject?.image || null,
        air_date: subject?.date || null,
        total_episodes: subject?.eps || 0,
        episode_offset: value.episode_offset,
        auto_download: value.auto_download,
        save_path: value.save_path || null,
      };

      await createBangumi.mutateAsync(request);
      onSuccess?.();
      onOpenChange(false);
    },
  });

  // Reset form when subject changes
  React.useEffect(() => {
    if (subject && open) {
      form.reset();
      // 解析 BGM.tv 名称，去除季度信息
      const parsedChinese = parseBgmtvName(subject.name_cn || subject.name || "");
      const parsedJapanese = parseBgmtvName(subject.name || "");
      form.setFieldValue("title_chinese", parsedChinese.name);
      form.setFieldValue("title_japanese", parsedJapanese.name);
      setSelectedTmdb(null);
    }
  }, [subject, open, form]);

  return (
    <DialogPrimitive.Root open={open} onOpenChange={onOpenChange}>
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
            "fixed left-1/2 top-1/2 z-50 -translate-x-1/2 -translate-y-1/2",
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
            "outline-none"
          )}
        >
          {/* Decorative elements */}
          <div className="pointer-events-none absolute -right-20 -top-20 size-40 rounded-full bg-linear-to-br from-chart-3/30 to-chart-1/30 blur-3xl dark:from-chart-3/20 dark:to-chart-1/20" />
          <div className="pointer-events-none absolute -left-20 bottom-0 size-40 rounded-full bg-linear-to-br from-chart-5/30 to-chart-3/30 blur-3xl dark:from-chart-5/20 dark:to-chart-3/20" />

          {/* Header */}
          <div className="relative border-b border-chart-3/30 dark:border-chart-1/20 p-4">
            <div className="flex items-center gap-3">
              <div className="flex size-10 items-center justify-center rounded-xl bg-linear-to-br from-chart-3 to-chart-1 text-white shadow-lg shadow-chart-1/30">
                <IconSparkles className="size-5" />
              </div>
              <div className="flex-1">
                <DialogPrimitive.Title className="text-lg font-bold bg-linear-to-r from-chart-3 via-chart-1 to-chart-5 bg-clip-text text-transparent">
                  添加番剧
                </DialogPrimitive.Title>
                <DialogPrimitive.Description className="text-xs text-muted-foreground">
                  填写番剧信息并添加到追番列表
                </DialogPrimitive.Description>
              </div>
              <DialogPrimitive.Close
                className={cn(
                  "flex size-8 items-center justify-center rounded-lg",
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
              <FieldGroup>
                {/* Title Chinese */}
                <form.Field
                  name="title_chinese"
                  validators={{
                    onChange: ({ value }) =>
                      !value ? "中文标题不能为空" : undefined,
                  }}
                >
                  {(field) => (
                    <Field data-invalid={field.state.meta.errors.length > 0}>
                      <FieldLabel htmlFor={field.name}>
                        <IconDeviceTv className="size-4 text-chart-3 dark:text-chart-1" />
                        中文标题
                      </FieldLabel>
                      <Input
                        id={field.name}
                        name={field.name}
                        value={field.state.value}
                        onBlur={field.handleBlur}
                        onChange={(e) => field.handleChange(e.target.value)}
                        placeholder="请输入中文标题"
                        aria-invalid={field.state.meta.errors.length > 0}
                      />
                      <FieldError errors={field.state.meta.errors.map(e => ({ message: e }))} />
                    </Field>
                  )}
                </form.Field>

                {/* Title Japanese */}
                <form.Field name="title_japanese">
                  {(field) => (
                    <Field>
                      <FieldLabel htmlFor={field.name}>
                        <IconDeviceTv className="size-4 text-chart-3 dark:text-chart-1" />
                        日文原名
                      </FieldLabel>
                      <Input
                        id={field.name}
                        name={field.name}
                        value={field.state.value}
                        onBlur={field.handleBlur}
                        onChange={(e) => field.handleChange(e.target.value)}
                        placeholder="请输入日文原名（可选）"
                      />
                    </Field>
                  )}
                </form.Field>

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
                  <FieldDescription>
                    用于获取海报和元数据
                  </FieldDescription>
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
                      <FieldDescription>用于第二季等情况</FieldDescription>
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
                        <FieldDescription>
                          自动下载新发布的剧集
                        </FieldDescription>
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
