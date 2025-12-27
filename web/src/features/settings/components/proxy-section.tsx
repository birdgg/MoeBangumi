import * as React from "react";
import { Input } from "@/components/ui/input";
import { Button } from "@/components/ui/button";
import {
  IconEye,
  IconEyeOff,
  IconWorld,
  IconUser,
  IconLock,
  IconPlugConnected,
  IconLoader2,
  IconCheck,
  IconX,
} from "@tabler/icons-react";
import { testProxy } from "@/lib/api";
import { FormField } from "./shared";
import { type SettingsFormInstance } from "../hooks";

type ConnectionStatus = "idle" | "loading" | "success" | "error";

export interface ProxySectionProps {
  form: SettingsFormInstance;
}

export function ProxySection({ form }: ProxySectionProps) {
  const [showPassword, setShowPassword] = React.useState(false);
  const [connectionStatus, setConnectionStatus] = React.useState<ConnectionStatus>("idle");
  const [errorMessage, setErrorMessage] = React.useState<string>("");

  // Reset connection status when proxy settings change
  React.useEffect(() => {
    let previousValues = JSON.stringify(form.store.state.values.proxy);

    const unsubscribe = form.store.subscribe(() => {
      const currentValues = JSON.stringify(form.store.state.values.proxy);
      if (currentValues !== previousValues) {
        previousValues = currentValues;
        setConnectionStatus("idle");
        setErrorMessage("");
      }
    });

    return unsubscribe;
  }, [form.store]);

  const handleTestProxy = async () => {
    setErrorMessage("");
    const values = form.state.values.proxy;

    const url = values.url?.trim();
    if (!url) {
      setConnectionStatus("error");
      setErrorMessage("请填写代理地址");
      return;
    }

    setConnectionStatus("loading");

    try {
      // Trim whitespace from credentials
      const username = values.username?.trim() || undefined;
      const password = values.password?.trim() || undefined;

      const { response } = await testProxy({
        body: {
          url,
          username,
          password,
        },
      });

      if (response.ok) {
        setConnectionStatus("success");
        setErrorMessage("");
      } else {
        // Parse JSON error response
        try {
          const errorData = await response.json();
          setConnectionStatus("error");
          setErrorMessage(errorData.details || errorData.error || "连接失败");
        } catch {
          setConnectionStatus("error");
          setErrorMessage("连接失败");
        }
      }
    } catch {
      setConnectionStatus("error");
      setErrorMessage("网络错误，请检查代理配置");
    }
  };

  return (
    <section className="space-y-5">
      <div className="space-y-4">
        {/* Proxy URL */}
        <form.Field name="proxy.url">
          {(field) => (
            <FormField label="代理地址">
              <div className="relative">
                <div className="pointer-events-none absolute left-2.5 top-1/2 -translate-y-1/2 text-muted-foreground">
                  <IconWorld className="size-4" />
                </div>
                <Input
                  type="text"
                  placeholder="http://127.0.0.1:7890 或 socks5://127.0.0.1:1080"
                  value={field.state.value ?? ""}
                  onChange={(e) => field.handleChange(e.target.value)}
                  onBlur={field.handleBlur}
                  className="pl-9"
                />
              </div>
              <p className="mt-1 text-xs text-muted-foreground">
                支持 HTTP 和 SOCKS5 代理协议
              </p>
            </FormField>
          )}
        </form.Field>

        {/* Auth section */}
        <div className="rounded-lg border border-border/50 bg-muted/20 p-4">
          <div className="flex items-center gap-2 pb-3">
            <IconLock className="size-4 text-chart-1" />
            <span className="text-sm font-medium">代理认证（可选）</span>
          </div>
          <p className="pb-3 text-xs text-muted-foreground">
            如果代理服务器需要认证，请填写用户名和密码
          </p>

          <div className="grid gap-4 sm:grid-cols-2">
            {/* Username */}
            <form.Field name="proxy.username">
              {(field) => (
                <FormField label="用户名">
                  <div className="relative">
                    <div className="pointer-events-none absolute left-2.5 top-1/2 -translate-y-1/2 text-muted-foreground">
                      <IconUser className="size-4" />
                    </div>
                    <Input
                      type="text"
                      placeholder="可选"
                      value={field.state.value ?? ""}
                      onChange={(e) => field.handleChange(e.target.value)}
                      onBlur={field.handleBlur}
                      className="pl-9"
                    />
                  </div>
                </FormField>
              )}
            </form.Field>

            {/* Password */}
            <form.Field name="proxy.password">
              {(field) => (
                <FormField label="密码">
                  <div className="relative">
                    <div className="pointer-events-none absolute left-2.5 top-1/2 -translate-y-1/2 text-muted-foreground">
                      <IconLock className="size-4" />
                    </div>
                    <Input
                      type={showPassword ? "text" : "password"}
                      placeholder="可选"
                      value={field.state.value ?? ""}
                      onChange={(e) => field.handleChange(e.target.value)}
                      onBlur={field.handleBlur}
                      className="pl-9 pr-10"
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
                </FormField>
              )}
            </form.Field>
          </div>
        </div>

        {/* Test Connection */}
        <div className="flex items-center gap-3">
          <Button
            type="button"
            variant="outline"
            size="sm"
            onClick={handleTestProxy}
            disabled={connectionStatus === "loading"}
            className="gap-1.5"
          >
            {connectionStatus === "loading" ? (
              <IconLoader2 className="size-3.5 animate-spin" />
            ) : (
              <IconPlugConnected className="size-3.5" />
            )}
            {connectionStatus === "loading" ? "测试中..." : "测试连接"}
          </Button>

          {connectionStatus === "success" && (
            <span className="flex items-center gap-1.5 text-xs text-chart-5">
              <IconCheck className="size-3.5" />
              连接成功
            </span>
          )}
          {connectionStatus === "error" && (
            <span className="flex items-center gap-1.5 text-xs text-destructive">
              <IconX className="size-3.5" />
              {errorMessage || "连接失败"}
            </span>
          )}
        </div>

        {/* Info note */}
        <p className="text-xs text-muted-foreground">
          测试会通过代理访问 mikanani.me 来验证连接是否正常工作
        </p>
      </div>
    </section>
  );
}
