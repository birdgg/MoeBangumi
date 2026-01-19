import * as React from "react";
import { Input } from "@/components/ui/input";
import { Button } from "@/components/ui/button";
import {
  IconEye,
  IconEyeOff,
  IconFolder,
  IconPlugConnected,
  IconLoader2,
  IconCheck,
  IconX,
} from "@tabler/icons-react";
import { postApiDownloaderTest } from "@/lib/api";
import { FormField } from "./shared";
import { getErrorMessage, type SettingsFormInstance } from "../hooks";

type TestStatus = "idle" | "loading" | "success" | "error";

export interface DownloaderSectionProps {
  form: SettingsFormInstance;
}

export function DownloaderSection({ form }: DownloaderSectionProps) {
  const [showPassword, setShowPassword] = React.useState(false);
  const [testStatus, setTestStatus] = React.useState<TestStatus>("idle");
  const [errorMessage, setErrorMessage] = React.useState<string>("");

  // Reset test status when downloader settings change
  React.useEffect(() => {
    let previousValues = JSON.stringify({
      url: form.store.state.values.downloader.url,
      username: form.store.state.values.downloader.username,
      password: form.store.state.values.downloader.password,
    });

    const unsubscribe = form.store.subscribe(() => {
      const currentValues = JSON.stringify({
        url: form.store.state.values.downloader.url,
        username: form.store.state.values.downloader.username,
        password: form.store.state.values.downloader.password,
      });
      if (currentValues !== previousValues) {
        previousValues = currentValues;
        setTestStatus("idle");
        setErrorMessage("");
      }
    });

    return unsubscribe;
  }, [form.store]);

  const handleTestConnection = async () => {
    setErrorMessage("");
    const downloader = form.state.values.downloader;

    const url = downloader.url?.trim();
    const username = downloader.username?.trim();
    const password = downloader.password;

    if (!url) {
      setTestStatus("error");
      setErrorMessage("请填写服务器地址");
      return;
    }

    if (!username) {
      setTestStatus("error");
      setErrorMessage("请填写用户名");
      return;
    }

    if (!password) {
      setTestStatus("error");
      setErrorMessage("请填写密码");
      return;
    }

    setTestStatus("loading");

    try {
      const { data, response } = await postApiDownloaderTest({
        body: {
          settings: {
            url,
            username,
            password,
            savePath: downloader.savePath || "",
          },
        },
      });

      if (response.ok && data?.success) {
        setTestStatus("success");
        setErrorMessage("");
      } else {
        setTestStatus("error");
        setErrorMessage(data?.message || "连接失败");
      }
    } catch {
      setTestStatus("error");
      setErrorMessage("网络错误，请检查配置");
    }
  };

  return (
    <section className="space-y-5">
      <div className="space-y-4">
        {/* URL */}
        <form.Field name="downloader.url">
          {(field) => {
            const error = getErrorMessage(field.state.meta.errors[0]);
            return (
              <FormField label="服务器地址">
                <Input
                  type="url"
                  placeholder="http://localhost:8080"
                  value={field.state.value}
                  onChange={(e) => field.handleChange(e.target.value)}
                  onBlur={field.handleBlur}
                />
                {error && (
                  <p className="text-xs text-destructive mt-1">{error}</p>
                )}
              </FormField>
            );
          }}
        </form.Field>

        {/* Username & Password */}
        <div className="grid gap-4 sm:grid-cols-2">
          <form.Field name="downloader.username">
            {(field) => {
              const error = getErrorMessage(field.state.meta.errors[0]);
              return (
                <FormField label="用户名">
                  <Input
                    type="text"
                    placeholder="admin"
                    value={field.state.value}
                    onChange={(e) => field.handleChange(e.target.value)}
                    onBlur={field.handleBlur}
                  />
                  {error && (
                    <p className="text-xs text-destructive mt-1">{error}</p>
                  )}
                </FormField>
              );
            }}
          </form.Field>

          <form.Field name="downloader.password">
            {(field) => {
              const error = getErrorMessage(field.state.meta.errors[0]);
              return (
                <FormField label="密码">
                  <div className="relative">
                    <Input
                      type={showPassword ? "text" : "password"}
                      placeholder="••••••••"
                      value={field.state.value}
                      onChange={(e) => field.handleChange(e.target.value)}
                      onBlur={field.handleBlur}
                      className="pr-10"
                    />
                    <button
                      type="button"
                      onClick={() => setShowPassword(!showPassword)}
                      className="absolute right-2 top-1/2 -translate-y-1/2 p-1 text-muted-foreground transition-colors hover:text-foreground"
                    >
                      {showPassword ? (
                        <IconEyeOff className="size-4" />
                      ) : (
                        <IconEye className="size-4" />
                      )}
                    </button>
                  </div>
                  {error && (
                    <p className="text-xs text-destructive mt-1">{error}</p>
                  )}
                </FormField>
              );
            }}
          </form.Field>
        </div>

        {/* Save Path */}
        <form.Field name="downloader.savePath">
          {(field) => {
            const error = getErrorMessage(field.state.meta.errors[0]);
            return (
              <FormField label="默认保存路径">
                <div className="relative">
                  <div className="pointer-events-none absolute left-2.5 top-1/2 -translate-y-1/2 text-muted-foreground">
                    <IconFolder className="size-4" />
                  </div>
                  <Input
                    type="text"
                    placeholder="/Media/Bangumi"
                    value={field.state.value}
                    onChange={(e) => field.handleChange(e.target.value)}
                    onBlur={field.handleBlur}
                    className="pl-9"
                  />
                </div>
                {error && (
                  <p className="text-xs text-destructive mt-1">{error}</p>
                )}
              </FormField>
            );
          }}
        </form.Field>

        {/* Test Connection Button */}
        <div className="flex items-center gap-3 pt-1">
          <Button
            type="button"
            variant="outline"
            size="sm"
            onClick={handleTestConnection}
            disabled={testStatus === "loading"}
            className="gap-1.5"
          >
            {testStatus === "loading" ? (
              <IconLoader2 className="size-3.5 animate-spin" />
            ) : (
              <IconPlugConnected className="size-3.5" />
            )}
            {testStatus === "loading" ? "测试中..." : "测试连接"}
          </Button>

          {testStatus === "success" && (
            <span className="flex items-center gap-1.5 text-xs text-chart-5">
              <IconCheck className="size-3.5" />
              连接成功
            </span>
          )}
          {testStatus === "error" && (
            <span className="flex items-center gap-1.5 text-xs text-destructive">
              <IconX className="size-3.5" />
              {errorMessage || "连接失败"}
            </span>
          )}
        </div>
      </div>
    </section>
  );
}
