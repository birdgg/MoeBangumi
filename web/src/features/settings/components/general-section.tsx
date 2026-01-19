import * as React from "react";
import { Input } from "@/components/ui/input";
import { Button } from "@/components/ui/button";
import { Checkbox } from "@/components/ui/checkbox";
import {
  AlertDialog,
  AlertDialogContent,
  AlertDialogHeader,
  AlertDialogTitle,
  AlertDialogDescription,
  AlertDialogFooter,
  AlertDialogCancel,
  AlertDialogAction,
} from "@/components/ui/alert-dialog";
import {
  IconEye,
  IconEyeOff,
  IconKey,
  IconBrandOpenSource,
  IconServer,
  IconLink,
  IconPlugConnected,
  IconLoader2,
  IconCheck,
  IconX,
  IconDownload,
  IconFolder,
} from "@tabler/icons-react";
import { toast } from "sonner";
import { testEmby, importEmby, getEmbyLibraries } from "@/lib/api";
import type { EmbyLibraryInfo } from "@/lib/api";
import { FormField } from "./shared";
import { type SettingsFormInstance } from "../hooks";

type TestStatus = "idle" | "loading" | "success" | "error";

export interface GeneralSectionProps {
  form: SettingsFormInstance;
}

export function GeneralSection({ form }: GeneralSectionProps) {
  const [showTmdbApiKey, setShowTmdbApiKey] = React.useState(false);
  const [showEmbyApiKey, setShowEmbyApiKey] = React.useState(false);
  const [embyTestStatus, setEmbyTestStatus] = React.useState<TestStatus>("idle");
  const [embyErrorMessage, setEmbyErrorMessage] = React.useState<string>("");
  const [embyLibraryCount, setEmbyLibraryCount] = React.useState<number | null>(null);
  const [isImporting, setIsImporting] = React.useState(false);

  // Library selection dialog state
  const [showLibraryDialog, setShowLibraryDialog] = React.useState(false);
  const [availableLibraries, setAvailableLibraries] = React.useState<EmbyLibraryInfo[]>([]);
  const [selectedLibraryIds, setSelectedLibraryIds] = React.useState<Set<string>>(new Set());
  const [isLoadingLibraries, setIsLoadingLibraries] = React.useState(false);

  // Reset Emby test status when settings change
  React.useEffect(() => {
    let previousValues = JSON.stringify({
      url: form.store.state.values.emby.url,
      apiKey: form.store.state.values.emby.apiKey,
    });

    const unsubscribe = form.store.subscribe(() => {
      const currentValues = JSON.stringify({
        url: form.store.state.values.emby.url,
        apiKey: form.store.state.values.emby.apiKey,
      });
      if (currentValues !== previousValues) {
        previousValues = currentValues;
        setEmbyTestStatus("idle");
        setEmbyErrorMessage("");
        setEmbyLibraryCount(null);
      }
    });

    return unsubscribe;
  }, [form.store]);

  const handleTestEmby = async () => {
    setEmbyErrorMessage("");
    setEmbyLibraryCount(null);
    const emby = form.state.values.emby;

    const url = emby.url?.trim();
    const apiKey = emby.apiKey?.trim();

    if (!url) {
      setEmbyTestStatus("error");
      setEmbyErrorMessage("请填写服务器地址");
      return;
    }

    if (!apiKey) {
      setEmbyTestStatus("error");
      setEmbyErrorMessage("请填写 API Key");
      return;
    }

    setEmbyTestStatus("loading");

    try {
      const { data, response } = await testEmby({
        body: {
          settings: {
            url,
            apiKey,
          },
        },
      });

      if (response.ok && data?.success) {
        setEmbyTestStatus("success");
        setEmbyErrorMessage("");
        setEmbyLibraryCount(data.libraryCount ?? null);
      } else {
        setEmbyTestStatus("error");
        setEmbyErrorMessage(data?.message || "连接失败");
      }
    } catch {
      setEmbyTestStatus("error");
      setEmbyErrorMessage("网络错误，请检查配置");
    }
  };

  // Open library selection dialog
  const handleOpenLibraryDialog = async () => {
    setIsLoadingLibraries(true);
    setShowLibraryDialog(true);

    try {
      const { data, response } = await getEmbyLibraries();

      if (response.ok && data) {
        setAvailableLibraries(data);
        // Select all by default
        setSelectedLibraryIds(new Set(data.map((lib) => lib.id)));
      } else {
        toast.error("获取媒体库失败", {
          description: "请检查 Emby 配置是否正确",
        });
        setShowLibraryDialog(false);
      }
    } catch {
      toast.error("获取媒体库失败", {
        description: "网络错误，请检查连接",
      });
      setShowLibraryDialog(false);
    } finally {
      setIsLoadingLibraries(false);
    }
  };

  // Toggle library selection
  const toggleLibrarySelection = (libraryId: string) => {
    setSelectedLibraryIds((prev) => {
      const next = new Set(prev);
      if (next.has(libraryId)) {
        next.delete(libraryId);
      } else {
        next.add(libraryId);
      }
      return next;
    });
  };

  // Perform the actual import
  const handleImportEmby = async () => {
    setShowLibraryDialog(false);
    setIsImporting(true);

    try {
      const libraryIds = selectedLibraryIds.size > 0 ? Array.from(selectedLibraryIds) : undefined;
      const { data, response } = await importEmby({
        body: { libraryIds },
      });

      if (response.ok && data) {
        const { stats } = data;
        if (stats.imported > 0) {
          toast.success("导入完成", {
            description: `成功导入 ${stats.imported} 部番剧${stats.skipped > 0 ? `，跳过 ${stats.skipped} 部` : ""}${stats.failed > 0 ? `，失败 ${stats.failed} 部` : ""}`,
          });
        } else if (stats.skipped > 0) {
          toast.info("无新内容", {
            description: `所有 ${stats.skipped} 部番剧已存在`,
          });
        } else {
          toast.info("媒体库为空", {
            description: "未发现可导入的番剧",
          });
        }
      } else {
        toast.error("导入失败", {
          description: "请检查 Emby 配置是否正确",
        });
      }
    } catch {
      toast.error("导入失败", {
        description: "网络错误，请检查连接",
      });
    } finally {
      setIsImporting(false);
    }
  };

  return (
    <section className="space-y-5">
      <div className="space-y-4">
        {/* TMDB API Key Section */}
        <div className="rounded-lg border border-border/50 bg-muted/20 p-4">
          <div className="flex items-center gap-2 pb-3">
            <IconBrandOpenSource className="size-4 text-chart-1" />
            <span className="text-sm font-medium">TMDB API</span>
          </div>
          <p className="pb-3 text-xs text-muted-foreground">
            用于获取动画海报和元数据信息。可在{" "}
            <a
              href="https://www.themoviedb.org/settings/api"
              target="_blank"
              rel="noopener noreferrer"
              className="text-chart-1 hover:underline"
            >
              TMDB 官网
            </a>{" "}
            免费申请 API Key。
          </p>

          <form.Field name="tmdb.apiKey">
            {(field) => (
              <FormField label="API Key">
                <div className="relative">
                  <div className="pointer-events-none absolute left-2.5 top-1/2 -translate-y-1/2 text-muted-foreground">
                    <IconKey className="size-4" />
                  </div>
                  <Input
                    type={showTmdbApiKey ? "text" : "password"}
                    placeholder="输入你的 TMDB API Key"
                    value={field.state.value ?? ""}
                    onChange={(e) => field.handleChange(e.target.value)}
                    onBlur={field.handleBlur}
                    className="pl-9 pr-10 font-mono text-sm"
                  />
                  <button
                    type="button"
                    onClick={() => setShowTmdbApiKey(!showTmdbApiKey)}
                    className="absolute right-2 top-1/2 -translate-y-1/2 p-1 text-muted-foreground transition-colors hover:text-foreground"
                  >
                    {showTmdbApiKey ? (
                      <IconEyeOff className="size-4" />
                    ) : (
                      <IconEye className="size-4" />
                    )}
                  </button>
                </div>
              </FormField>
            )}
          </form.Field>
        </div>

        {/* Emby Section */}
        <div className="rounded-lg border border-border/50 bg-muted/20 p-4">
          <div className="flex items-center gap-2 pb-3">
            <IconServer className="size-4 text-emerald-500" />
            <span className="text-sm font-medium">Emby</span>
          </div>
          <p className="pb-3 text-xs text-muted-foreground">
            连接 Emby 媒体服务器，可从媒体库导入番剧信息。填写服务器地址和 API Key 后即可使用。
          </p>

          <div className="space-y-4">
            {/* Emby URL */}
            <form.Field name="emby.url">
              {(field) => (
                <FormField label="服务器地址">
                  <div className="relative">
                    <div className="pointer-events-none absolute left-2.5 top-1/2 -translate-y-1/2 text-muted-foreground">
                      <IconLink className="size-4" />
                    </div>
                    <Input
                      type="url"
                      placeholder="http://localhost:8096"
                      value={field.state.value ?? ""}
                      onChange={(e) => field.handleChange(e.target.value)}
                      onBlur={field.handleBlur}
                      className="pl-9"
                    />
                  </div>
                </FormField>
              )}
            </form.Field>

            {/* Emby API Key */}
            <form.Field name="emby.apiKey">
              {(field) => (
                <FormField label="API Key">
                  <div className="relative">
                    <div className="pointer-events-none absolute left-2.5 top-1/2 -translate-y-1/2 text-muted-foreground">
                      <IconKey className="size-4" />
                    </div>
                    <Input
                      type={showEmbyApiKey ? "text" : "password"}
                      placeholder="输入你的 Emby API Key"
                      value={field.state.value ?? ""}
                      onChange={(e) => field.handleChange(e.target.value)}
                      onBlur={field.handleBlur}
                      className="pl-9 pr-10 font-mono text-sm"
                    />
                    <button
                      type="button"
                      onClick={() => setShowEmbyApiKey(!showEmbyApiKey)}
                      className="absolute right-2 top-1/2 -translate-y-1/2 p-1 text-muted-foreground transition-colors hover:text-foreground"
                    >
                      {showEmbyApiKey ? (
                        <IconEyeOff className="size-4" />
                      ) : (
                        <IconEye className="size-4" />
                      )}
                    </button>
                  </div>
                </FormField>
              )}
            </form.Field>

            {/* Action Buttons */}
            <div className="flex flex-wrap items-center gap-3 pt-1">
              <Button
                type="button"
                variant="outline"
                size="sm"
                onClick={handleTestEmby}
                disabled={embyTestStatus === "loading" || isImporting}
                className="gap-1.5"
              >
                {embyTestStatus === "loading" ? (
                  <IconLoader2 className="size-3.5 animate-spin" />
                ) : (
                  <IconPlugConnected className="size-3.5" />
                )}
                {embyTestStatus === "loading" ? "测试中..." : "测试连接"}
              </Button>

              <Button
                type="button"
                variant="outline"
                size="sm"
                onClick={handleOpenLibraryDialog}
                disabled={isImporting || embyTestStatus === "loading"}
                className="gap-1.5"
              >
                {isImporting ? (
                  <IconLoader2 className="size-3.5 animate-spin" />
                ) : (
                  <IconDownload className="size-3.5" />
                )}
                {isImporting ? "导入中..." : "导入媒体库"}
              </Button>

              {embyTestStatus === "success" && (
                <span className="flex items-center gap-1.5 text-xs text-chart-5">
                  <IconCheck className="size-3.5" />
                  连接成功{embyLibraryCount !== null && `，发现 ${embyLibraryCount} 个媒体库`}
                </span>
              )}
              {embyTestStatus === "error" && (
                <span className="flex items-center gap-1.5 text-xs text-destructive">
                  <IconX className="size-3.5" />
                  {embyErrorMessage || "连接失败"}
                </span>
              )}
            </div>
          </div>

          {/* Help text */}
          <div className="mt-4 pt-4 border-t border-border/30">
            <p className="text-xs text-muted-foreground">
              如何获取 API Key？
            </p>
            <ul className="mt-1 text-xs text-muted-foreground list-disc list-inside space-y-0.5">
              <li>在 Emby 服务器中进入「设置」→「API 密钥」</li>
              <li>点击「新建 API 密钥」并输入应用名称</li>
            </ul>
          </div>
        </div>

        {/* Info note */}
        <p className="text-xs text-muted-foreground">
          修改设置后会立即生效，无需重启应用
        </p>
      </div>

      {/* Library Selection Dialog */}
      <AlertDialog open={showLibraryDialog} onOpenChange={setShowLibraryDialog}>
        <AlertDialogContent className="max-w-md">
          <AlertDialogHeader>
            <AlertDialogTitle>选择要导入的媒体库</AlertDialogTitle>
            <AlertDialogDescription>
              选择要从 Emby 导入番剧的媒体库，未选中的库将被跳过。
            </AlertDialogDescription>
          </AlertDialogHeader>

          {isLoadingLibraries ? (
            <div className="flex items-center justify-center py-8">
              <IconLoader2 className="size-6 animate-spin text-muted-foreground" />
            </div>
          ) : availableLibraries.length === 0 ? (
            <div className="py-8 text-center text-sm text-muted-foreground">
              未找到可用的媒体库
            </div>
          ) : (
            <div className="space-y-3 py-2">
              {availableLibraries.map((library) => (
                <label
                  key={library.id}
                  className="flex items-center gap-3 rounded-lg border border-border/50 bg-muted/30 p-3 cursor-pointer hover:bg-muted/50 transition-colors"
                >
                  <Checkbox
                    checked={selectedLibraryIds.has(library.id)}
                    onCheckedChange={() => toggleLibrarySelection(library.id)}
                  />
                  <div className="flex items-center gap-2 flex-1 min-w-0">
                    <IconFolder className="size-4 text-muted-foreground shrink-0" />
                    <span className="text-sm font-medium truncate">{library.name}</span>
                  </div>
                  {library.collectionType && (
                    <span className="text-xs text-muted-foreground bg-muted px-1.5 py-0.5 rounded shrink-0">
                      {library.collectionType}
                    </span>
                  )}
                </label>
              ))}
            </div>
          )}

          <AlertDialogFooter>
            <AlertDialogCancel>取消</AlertDialogCancel>
            <AlertDialogAction
              onClick={handleImportEmby}
              disabled={selectedLibraryIds.size === 0 || isLoadingLibraries}
            >
              导入 ({selectedLibraryIds.size})
            </AlertDialogAction>
          </AlertDialogFooter>
        </AlertDialogContent>
      </AlertDialog>
    </section>
  );
}
