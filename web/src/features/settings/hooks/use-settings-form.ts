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
      type: "qBittorrent" as const,
      url: settings?.downloader?.url ?? "",
      username: settings?.downloader?.username ?? "",
      password: settings?.downloader?.password ?? "",
      save_path: settings?.downloader?.save_path ?? "/Media/Bangumi",
    },
    filter: {
      global_rss_filters: settings?.filter?.global_rss_filters ?? [],
    },
    proxy: {
      url: settings?.proxy?.url ?? "",
      username: settings?.proxy?.username ?? "",
      password: settings?.proxy?.password ?? "",
    },
  };
}

/**
 * Convert form data to API UpdateSettings format
 */
export function formDataToUpdateSettings(data: SettingsFormData): UpdateSettings {
  return {
    downloader: {
      type: data.downloader.type,
      url: data.downloader.url,
      username: data.downloader.username,
      password: data.downloader.password,
      save_path: data.downloader.save_path,
    },
    filter: {
      global_rss_filters: data.filter.global_rss_filters,
    },
    proxy: {
      url: data.proxy.url?.trim() || null,
      username: data.proxy.username?.trim() || null,
      password: data.proxy.password?.trim() || null,
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
      const formData = settingsToFormData(initialSettings);
      form.setFieldValue("downloader.type", formData.downloader.type);
      form.setFieldValue("downloader.url", formData.downloader.url);
      form.setFieldValue("downloader.username", formData.downloader.username);
      form.setFieldValue("downloader.password", formData.downloader.password);
      form.setFieldValue("downloader.save_path", formData.downloader.save_path);
      form.setFieldValue("filter.global_rss_filters", formData.filter.global_rss_filters);
      form.setFieldValue("proxy.url", formData.proxy.url);
      form.setFieldValue("proxy.username", formData.proxy.username);
      form.setFieldValue("proxy.password", formData.proxy.password);
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
