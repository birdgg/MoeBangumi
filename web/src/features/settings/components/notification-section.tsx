import * as React from "react";
import { Input } from "@/components/ui/input";
import { Button } from "@/components/ui/button";
import {
  IconEye,
  IconEyeOff,
  IconBrandTelegram,
  IconSend,
  IconLoader2,
  IconCheck,
  IconX,
  IconKey,
  IconHash,
} from "@tabler/icons-react";
import { testNotification } from "@/lib/api";
import { FormField } from "./shared";
import { type SettingsFormInstance } from "../hooks";

type TestStatus = "idle" | "loading" | "success" | "error";

export interface NotificationSectionProps {
  form: SettingsFormInstance;
}

export function NotificationSection({ form }: NotificationSectionProps) {
  const [showToken, setShowToken] = React.useState(false);
  const [testStatus, setTestStatus] = React.useState<TestStatus>("idle");
  const [errorMessage, setErrorMessage] = React.useState<string>("");

  // Reset test status when telegram settings change
  React.useEffect(() => {
    let previousValues = JSON.stringify({
      botToken: form.store.state.values.notification.telegram.botToken,
      chatId: form.store.state.values.notification.telegram.chatId,
    });

    const unsubscribe = form.store.subscribe(() => {
      const currentValues = JSON.stringify({
        botToken: form.store.state.values.notification.telegram.botToken,
        chatId: form.store.state.values.notification.telegram.chatId,
      });
      if (currentValues !== previousValues) {
        previousValues = currentValues;
        setTestStatus("idle");
        setErrorMessage("");
      }
    });

    return unsubscribe;
  }, [form.store]);

  const handleTestNotification = async () => {
    setErrorMessage("");
    const telegram = form.state.values.notification.telegram;

    const botToken = telegram.botToken?.trim();
    const chatId = telegram.chatId?.trim();

    if (!botToken) {
      setTestStatus("error");
      setErrorMessage("请填写 Bot Token");
      return;
    }

    if (!chatId) {
      setTestStatus("error");
      setErrorMessage("请填写 Chat ID");
      return;
    }

    setTestStatus("loading");

    try {
      const { response } = await testNotification({
        body: {
          botToken: botToken,
          chatId: chatId,
        },
      });

      if (response.ok) {
        setTestStatus("success");
        setErrorMessage("");
      } else {
        try {
          const errorData = await response.json();
          setTestStatus("error");
          setErrorMessage(errorData.details || errorData.error || "发送失败");
        } catch {
          setTestStatus("error");
          setErrorMessage("发送失败");
        }
      }
    } catch {
      setTestStatus("error");
      setErrorMessage("网络错误，请检查配置");
    }
  };

  return (
    <section className="space-y-5">
      <div className="space-y-4">
        {/* Telegram Configuration */}
        <div className="rounded-xl border border-border/50 bg-muted/20 p-4">
          {/* Telegram Header */}
          <div className="flex items-center gap-2 pb-4">
            <IconBrandTelegram className="size-5 text-[#2AABEE]" />
            <span className="text-sm font-medium">Telegram</span>
          </div>

          {/* Telegram Fields */}
          <div className="space-y-4">
            {/* Bot Token */}
            <form.Field name="notification.telegram.botToken">
              {(field) => (
                <FormField label="Bot Token">
                  <div className="relative">
                    <div className="pointer-events-none absolute left-2.5 top-1/2 -translate-y-1/2 text-muted-foreground">
                      <IconKey className="size-4" />
                    </div>
                    <Input
                      type={showToken ? "text" : "password"}
                      placeholder="123456789:ABCdefGHIjklMNOpqrSTUvwxYZ"
                      value={field.state.value ?? ""}
                      onChange={(e) => field.handleChange(e.target.value)}
                      onBlur={field.handleBlur}
                      className="pl-9 pr-10"
                    />
                    <button
                      type="button"
                      onClick={() => setShowToken(!showToken)}
                      className="absolute right-2 top-1/2 -translate-y-1/2 p-1 text-muted-foreground transition-colors hover:text-foreground"
                    >
                      {showToken ? (
                        <IconEyeOff className="size-4" />
                      ) : (
                        <IconEye className="size-4" />
                      )}
                    </button>
                  </div>
                </FormField>
              )}
            </form.Field>

            {/* Chat ID */}
            <form.Field name="notification.telegram.chatId">
              {(field) => (
                <FormField label="Chat ID">
                  <div className="relative">
                    <div className="pointer-events-none absolute left-2.5 top-1/2 -translate-y-1/2 text-muted-foreground">
                      <IconHash className="size-4" />
                    </div>
                    <Input
                      type="text"
                      placeholder="-1001234567890"
                      value={field.state.value ?? ""}
                      onChange={(e) => field.handleChange(e.target.value)}
                      onBlur={field.handleBlur}
                      className="pl-9"
                    />
                  </div>
                </FormField>
              )}
            </form.Field>

            {/* Test Button */}
            <div className="flex items-center gap-3 pt-1">
              <Button
                type="button"
                variant="outline"
                size="sm"
                onClick={handleTestNotification}
                disabled={testStatus === "loading"}
                className="gap-1.5"
              >
                {testStatus === "loading" ? (
                  <IconLoader2 className="size-3.5 animate-spin" />
                ) : (
                  <IconSend className="size-3.5" />
                )}
                {testStatus === "loading" ? "发送中..." : "发送测试通知"}
              </Button>

              {testStatus === "success" && (
                <span className="flex items-center gap-1.5 text-xs text-chart-5">
                  <IconCheck className="size-3.5" />
                  发送成功
                </span>
              )}
              {testStatus === "error" && (
                <span className="flex items-center gap-1.5 text-xs text-destructive">
                  <IconX className="size-3.5" />
                  {errorMessage || "发送失败"}
                </span>
              )}
            </div>
          </div>

          {/* Help text */}
          <div className="mt-4 pt-4 border-t border-border/30">
            <p className="text-xs text-muted-foreground">
              如何获取 Bot Token 和 Chat ID？
            </p>
            <ul className="mt-1 text-xs text-muted-foreground list-disc list-inside space-y-0.5">
              <li>
                使用{" "}
                <a
                  href="https://t.me/BotFather"
                  target="_blank"
                  rel="noopener noreferrer"
                  className="text-chart-1 hover:underline"
                >
                  @BotFather
                </a>{" "}
                创建 Bot 获取 Token
              </li>
              <li>
                使用{" "}
                <a
                  href="https://t.me/userinfobot"
                  target="_blank"
                  rel="noopener noreferrer"
                  className="text-chart-1 hover:underline"
                >
                  @userinfobot
                </a>{" "}
                获取你的 Chat ID
              </li>
            </ul>
          </div>
        </div>
      </div>
    </section>
  );
}
