import { z } from "zod";

/**
 * Downloader types supported by the system
 */
export const downloaderTypes = ["qBittorrent", "Transmission"] as const;
export type DownloaderTypeValue = (typeof downloaderTypes)[number];

/**
 * Downloader configuration schema
 * - qBittorrent: requires username and password
 * - Transmission: username and password are optional
 */
export const downloaderSchema = z
  .object({
    type: z.enum(downloaderTypes),
    url: z
      .string()
      .min(1, "服务器地址不能为空")
      .url("请输入有效的 URL (如 http://localhost:8080)"),
    username: z.string(),
    password: z.string(),
    save_path: z.string().min(1, "保存路径不能为空"),
  })
  .superRefine((data, ctx) => {
    // qBittorrent requires username and password
    if (data.type === "qBittorrent") {
      if (!data.username || data.username.length === 0) {
        ctx.addIssue({
          code: z.ZodIssueCode.custom,
          message: "用户名不能为空",
          path: ["username"],
        });
      }
      if (!data.password || data.password.length === 0) {
        ctx.addIssue({
          code: z.ZodIssueCode.custom,
          message: "密码不能为空",
          path: ["password"],
        });
      }
    }
    // Transmission: username and password are optional
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
