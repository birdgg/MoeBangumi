import { z } from "zod";

/**
 * Downloader configuration schema
 */
export const downloaderSchema = z.object({
  type: z.literal("qBittorrent"),
  url: z
    .string()
    .min(1, "服务器地址不能为空")
    .url("请输入有效的 URL (如 http://localhost:8080)"),
  username: z.string().min(1, "用户名不能为空"),
  password: z.string().min(1, "密码不能为空"),
  save_path: z.string().min(1, "保存路径不能为空"),
  webhook_url: z.string().optional(),
});

/**
 * Filter configuration schema
 */
export const filterSchema = z.object({
  global_rss_filters: z.array(z.string()),
});

/**
 * Proxy configuration schema
 */
export const proxySchema = z.object({
  url: z.string().optional(),
  username: z.string().optional(),
  password: z.string().optional(),
});

/**
 * Complete settings form schema
 */
export const settingsFormSchema = z.object({
  downloader: downloaderSchema,
  filter: filterSchema,
  proxy: proxySchema,
});

/**
 * TypeScript types inferred from schemas
 */
export type DownloaderFormData = z.infer<typeof downloaderSchema>;
export type FilterFormData = z.infer<typeof filterSchema>;
export type ProxyFormData = z.infer<typeof proxySchema>;
export type SettingsFormData = z.infer<typeof settingsFormSchema>;
