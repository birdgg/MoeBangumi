import * as React from "react";
import { useForm } from "@tanstack/react-form";
import { settingsFormSchema, type SettingsFormData } from "../schema";
import type { Settings, UpdateSettings } from "@/lib/api/client/types.gen";

/**
 * Convert server Settings to form data format
 */
export function settingsToFormData(settings?: Settings): SettingsFormData {
  return {
    downloader: {
      type: settings?.downloader?.type ?? "qBittorrent",
      save_path: settings?.downloader?.save_path ?? "/Media/Bangumi",
      configs: {
        qbittorrent: {
          url: settings?.downloader?.configs?.qbittorrent?.url ?? "http://localhost:8080",
          username: settings?.downloader?.configs?.qbittorrent?.username ?? "",
          password: settings?.downloader?.configs?.qbittorrent?.password ?? "",
        },
        transmission: {
          url: settings?.downloader?.configs?.transmission?.url ?? "http://localhost:9091/transmission/rpc",
          username: settings?.downloader?.configs?.transmission?.username ?? "",
          password: settings?.downloader?.configs?.transmission?.password ?? "",
        },
      },
    },
    filter: {
      global_rss_filters: settings?.filter?.global_rss_filters ?? [],
    },
    proxy: {
      url: settings?.proxy?.url ?? "",
      username: settings?.proxy?.username ?? "",
      password: settings?.proxy?.password ?? "",
    },
    notification: {
      telegram: {
        bot_token: settings?.notification?.telegram?.bot_token ?? "",
        chat_id: settings?.notification?.telegram?.chat_id ?? "",
      },
    },
    priority: {
      subtitle_groups: settings?.priority?.subtitle_groups ?? [],
      subtitle_languages: settings?.priority?.subtitle_languages ?? [],
    },
  };
}

/**
 * Convert form data to API UpdateSettings format.
 * Only sends the active downloader's config to avoid overwriting inactive configs.
 */
export function formDataToUpdateSettings(data: SettingsFormData): UpdateSettings {
  const isQBittorrent = data.downloader.type === "qBittorrent";

  return {
    downloader: {
      type: data.downloader.type,
      save_path: data.downloader.save_path,
      // Only send the active downloader's config
      ...(isQBittorrent
        ? {
            qbittorrent: {
              url: data.downloader.configs.qbittorrent.url,
              username: data.downloader.configs.qbittorrent.username,
              password: data.downloader.configs.qbittorrent.password,
            },
          }
        : {
            transmission: {
              url: data.downloader.configs.transmission.url,
              username: data.downloader.configs.transmission.username || null,
              password: data.downloader.configs.transmission.password || null,
            },
          }),
    },
    filter: {
      global_rss_filters: data.filter.global_rss_filters,
    },
    proxy: {
      url: data.proxy.url.trim() || null,
      username: data.proxy.username.trim() || null,
      password: data.proxy.password.trim() || null,
    },
    notification: {
      // Auto-enable when both bot_token and chat_id are provided
      enabled: !!(data.notification.telegram.bot_token.trim() && data.notification.telegram.chat_id.trim()),
      telegram: {
        enabled: !!(data.notification.telegram.bot_token.trim() && data.notification.telegram.chat_id.trim()),
        bot_token: data.notification.telegram.bot_token.trim() || null,
        chat_id: data.notification.telegram.chat_id.trim() || null,
      },
    },
    priority: {
      subtitle_groups: data.priority.subtitle_groups,
      subtitle_languages: data.priority.subtitle_languages,
    },
  };
}

/**
 * Settings form hook with Zod validation
 */
export function useSettingsForm(initialSettings?: Settings) {
  const form = useForm({
    defaultValues: settingsToFormData(initialSettings),
    validators: {
      onChange: settingsFormSchema,
    },
  });

  // Sync form values when settings data loads/updates
  React.useEffect(() => {
    if (initialSettings) {
      form.reset(settingsToFormData(initialSettings));
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [initialSettings]);

  return form;
}

/**
 * Type for the settings form instance
 */
export type SettingsFormInstance = ReturnType<typeof useSettingsForm>;

/**
 * Extract error message from validation error
 */
export function getErrorMessage(error: unknown): string | undefined {
  if (!error) return undefined;
  if (typeof error === "string") return error;
  if (typeof error === "object" && "message" in error) {
    return (error as { message: string }).message;
  }
  return String(error);
}
