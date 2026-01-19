import * as React from "react";
import { useForm } from "@tanstack/react-form";
import { settingsFormSchema, mergeWithDefaultPatterns, type SettingsFormData } from "../schema";
import type { Settings } from "@/lib/api/client/types.gen";

/**
 * Convert server Settings to form data format
 */
export function settingsToFormData(settings?: Settings): SettingsFormData {
  return {
    downloader: {
      url: settings?.downloader?.url ?? "http://localhost:8080",
      username: settings?.downloader?.username ?? "",
      password: settings?.downloader?.password ?? "",
      savePath: settings?.downloader?.savePath ?? "/Media/Bangumi",
    },
    filter_: {
      globalRssFilters: settings?.filter_?.globalRssFilters ?? [],
    },
    notification: {
      telegram: {
        botToken: settings?.notification?.telegram?.botToken ?? "",
        chatId: settings?.notification?.telegram?.chatId ?? "",
      },
    },
    priority: {
      groups: settings?.priority?.groups ?? [],
      languages: mergeWithDefaultPatterns(settings?.priority?.languages ?? []),
    },
    tmdb: {
      apiKey: settings?.tmdb?.apiKey ?? "",
    },
    emby: {
      url: settings?.emby?.url ?? "",
      apiKey: settings?.emby?.apiKey ?? "",
    },
  };
}

/**
 * Convert form data to API Settings format.
 */
export function formDataToSettings(data: SettingsFormData): Settings {
  return {
    downloader: {
      url: data.downloader.url,
      username: data.downloader.username,
      password: data.downloader.password,
      savePath: data.downloader.savePath,
    },
    filter_: {
      globalRssFilters: data.filter_.globalRssFilters,
    },
    notification: {
      telegram: {
        botToken: data.notification.telegram.botToken,
        chatId: data.notification.telegram.chatId,
      },
    },
    priority: {
      groups: data.priority.groups,
      languages: data.priority.languages,
    },
    tmdb: {
      apiKey: data.tmdb.apiKey,
    },
    emby: {
      url: data.emby.url,
      apiKey: data.emby.apiKey,
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
