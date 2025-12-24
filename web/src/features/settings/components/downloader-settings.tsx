import * as React from "react";
import { cn } from "@/lib/utils";
import { Input } from "@/components/ui/input";
import { Button } from "@/components/ui/button";
import { Label } from "@/components/ui/label";
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from "@/components/ui/select";
import {
  IconPlugConnected,
  IconEye,
  IconEyeOff,
  IconLoader2,
  IconCheck,
  IconX,
} from "@tabler/icons-react";
import { testDownloaderConnection } from "@/lib/api";

function SettingsCard({
  children,
  className,
}: {
  children: React.ReactNode;
  className?: string;
}) {
  return (
    <div
      className={cn(
        "relative overflow-hidden rounded-2xl border border-border/50 bg-card/80 p-6 backdrop-blur-sm",
        "ring-1 ring-foreground/5",
        "before:absolute before:inset-0 before:-z-10 before:bg-linear-to-br before:from-chart-1/5 before:via-transparent before:to-chart-3/5",
        className
      )}
    >
      {children}
    </div>
  );
}

function FormField({
  label,
  description,
  children,
}: {
  label: string;
  description?: string;
  children: React.ReactNode;
}) {
  return (
    <div className="group space-y-2">
      <div className="flex items-baseline justify-between">
        <Label className="text-sm font-medium text-foreground transition-colors group-focus-within:text-chart-1">
          {label}
        </Label>
        {description && (
          <span className="text-xs text-muted-foreground">{description}</span>
        )}
      </div>
      {children}
    </div>
  );
}

type ConnectionStatus = "idle" | "loading" | "success" | "error";

type DownloaderType = "qbittorrent";

export function DownloaderSettings() {
  const [downloaderType, setDownloaderType] = React.useState<DownloaderType>("qbittorrent");
  const [showPassword, setShowPassword] = React.useState(false);
  const [connectionStatus, setConnectionStatus] = React.useState<ConnectionStatus>("idle");
  const [errorMessage, setErrorMessage] = React.useState<string>("");
  const [url, setUrl] = React.useState("");
  const [username, setUsername] = React.useState("");
  const [password, setPassword] = React.useState("");

  const handleCheckConnection = async () => {
    setErrorMessage("");

    if (!url || !username || !password) {
      setConnectionStatus("error");
      setErrorMessage("请填写所有必要字段");
      return;
    }

    setConnectionStatus("loading");

    try {
      const { response } = await testDownloaderConnection({
        body: {
          type: downloaderType,
          url,
          username,
          password,
        },
      });

      if (response.ok) {
        setConnectionStatus("success");
        setErrorMessage("");
      } else {
        const text = await response.text();
        setConnectionStatus("error");
        setErrorMessage(text || "连接失败");
      }
    } catch {
      setConnectionStatus("error");
      setErrorMessage("网络错误，请检查服务器是否运行");
    }
  };

  return (
    <SettingsCard>
      <div className="space-y-5">
        <FormField label="下载器类型">
          <Select value={downloaderType} onValueChange={(v) => setDownloaderType(v as DownloaderType)}>
            <SelectTrigger className="w-full transition-all duration-200 focus:ring-2 focus:ring-chart-1/20">
              <SelectValue />
            </SelectTrigger>
            <SelectContent>
              <SelectItem value="qbittorrent">qBittorrent</SelectItem>
            </SelectContent>
          </Select>
        </FormField>

        <FormField label="服务器地址">
          <Input
            type="url"
            placeholder="http://localhost:8080"
            value={url}
            onChange={(e) => setUrl(e.target.value)}
            className="transition-all duration-200 focus:ring-2 focus:ring-chart-1/20"
          />
        </FormField>

        <div className="grid gap-5 sm:grid-cols-2">
          <FormField label="用户名">
            <Input
              type="text"
              placeholder="admin"
              value={username}
              onChange={(e) => setUsername(e.target.value)}
              className="transition-all duration-200 focus:ring-2 focus:ring-chart-1/20"
            />
          </FormField>

          <FormField label="密码">
            <div className="relative">
              <Input
                type={showPassword ? "text" : "password"}
                placeholder="••••••••"
                value={password}
                onChange={(e) => setPassword(e.target.value)}
                className="pr-10 transition-all duration-200 focus:ring-2 focus:ring-chart-1/20"
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
        </div>

        <div className="flex items-center gap-4 pt-2">
          <Button
            onClick={handleCheckConnection}
            disabled={connectionStatus === "loading"}
            className={cn(
              "group relative gap-2 overflow-hidden",
              "bg-linear-to-r from-chart-1 via-chart-2 to-chart-3 text-white",
              "shadow-lg shadow-chart-2/20 transition-all duration-300",
              "hover:shadow-xl hover:shadow-chart-2/30",
              "disabled:opacity-70"
            )}
          >
            <span className="absolute inset-0 bg-linear-to-r from-chart-2 via-chart-3 to-chart-1 opacity-0 transition-opacity duration-500 group-hover:opacity-100" />
            {connectionStatus === "loading" ? (
              <IconLoader2 className="relative z-10 size-4 animate-spin" />
            ) : (
              <IconPlugConnected className="relative z-10 size-4" />
            )}
            <span className="relative z-10">
              {connectionStatus === "loading" ? "连接中..." : "测试连接"}
            </span>
          </Button>

          {/* Connection status indicator */}
          {connectionStatus === "success" && (
            <div className="flex items-center gap-2 text-sm text-green-600 dark:text-green-400">
              <div className="flex size-6 items-center justify-center rounded-full bg-green-500/10">
                <IconCheck className="size-3.5" />
              </div>
              连接成功
            </div>
          )}
          {connectionStatus === "error" && (
            <div className="flex items-center gap-2 text-sm text-destructive">
              <div className="flex size-6 shrink-0 items-center justify-center rounded-full bg-destructive/10">
                <IconX className="size-3.5" />
              </div>
              <span>{errorMessage || "连接失败"}</span>
            </div>
          )}
        </div>
      </div>
    </SettingsCard>
  );
}
