# RSS Scheduler

RSS 调度器负责定时获取和处理 RSS 订阅，自动下载新番剧集。

## 架构概览

```
┌─────────────────┐     ┌──────────────────────┐     ┌─────────────────┐
│  SchedulerJob   │────▶│  RssProcessingService │────▶│  DownloaderService │
│  (RssFetchJob)  │     │                      │     │                 │
└─────────────────┘     └──────────────────────┘     └─────────────────┘
        │                        │
        │                        ▼
        │               ┌──────────────────┐
        │               │   RssClient      │
        │               │ (Mikan/Nyaa)     │
        │               └──────────────────┘
        ▼
┌─────────────────┐
│   Database      │
│ (RSS, Torrent)  │
└─────────────────┘
```

## 核心组件

### RssFetchJob

定时任务，每小时执行一次，负责触发 RSS 处理流程。

```rust
// crates/server/src/services/scheduler/rss_fetch_job.rs
impl SchedulerJob for RssFetchJob {
    fn interval(&self) -> Duration {
        Duration::from_secs(3600) // 每小时
    }

    async fn execute(&self) -> JobResult {
        let rss_list = RssRepository::get_enabled(&self.db).await?;
        let global_exclude_filters = self.rss_processing.get_global_exclude_filters();
        self.rss_processing.process_batch(rss_list, &global_exclude_filters).await;
        Ok(())
    }
}
```

### RssProcessingService

RSS 处理的核心服务，提供三种处理模式：

| 方法 | 用途 | 调用方 |
|------|------|--------|
| `process_single()` | 处理单个 RSS 订阅 | 内部调用 |
| `process_batch()` | 批量并发处理多个 RSS | RssFetchJob (定时任务) |
| `spawn_background()` | 后台异步处理 | BangumiService (API 触发) |

## 主 RSS 与备用 RSS

每个 RSS 订阅有 `is_primary` 字段区分主 RSS 和备用 RSS。

### 处理优先级

`process_batch()` 采用两阶段处理，确保主 RSS 优先：

```
Phase 1: 并发处理所有备用 RSS
         └── 备用 RSS 先下载，作为 fallback

Phase 2: 并发处理所有主 RSS
         └── 主 RSS 可覆盖备用 RSS 的下载
```

### 覆盖逻辑（洗版）

当主 RSS 处理时发现某集已存在：

| 已存在来源 | 处理方式 |
|------------|----------|
| 备用 RSS | 洗版：删除旧 torrent + 下载器任务 + 文件，用主 RSS 替换 |
| 主 RSS | 跳过（不重复下载） |
| 手动添加 | 跳过（不覆盖） |

这保证了：
- 备用 RSS 可快速下载新集
- 主 RSS 最终会覆盖备用源，使用首选资源
- 已有主 RSS 下载不会被备用 RSS 覆盖

### 洗版流程

洗版时的完整处理流程：

```
1. 主 RSS 检测到某集已存在（来自备用 RSS）
   ↓
2. 调用 delete_from_downloader()
   ├── 从下载器删除旧任务
   └── 删除已下载的文件
   ↓
3. 从数据库删除旧 torrent 记录
   ↓
4. 创建新 torrent 记录并添加下载任务
```

### 典型使用场景

| 场景 | 备用 RSS | 主 RSS |
|------|----------|--------|
| WebRip → BDRip | WebRip 源（播出时快速下载） | BDRip 源（蓝光发售后升级） |
| 分辨率升级 | 720p 源 | 1080p 源 |
| 字幕组偏好 | 速度快的字幕组 | 质量好的字幕组 |

**配置示例**：

```
备用 RSS (is_primary=false):
  URL: https://mikanani.me/RSS/Bangumi?bangumiId=xxx&subgroupid=123  # 速度快的字幕组
  include_filters: ["720p"]

主 RSS (is_primary=true):
  URL: https://mikanani.me/RSS/Bangumi?bangumiId=xxx&subgroupid=456  # 质量好的字幕组
  include_filters: ["1080p", "BDRip"]
```

## 处理流程

### 单个 RSS 处理流程 (`process_single`)

```
1. 获取 Bangumi 信息
   └── 用于生成保存路径和文件名

2. 解析 RSS 源类型
   ├── Nyaa (nyaa.si)
   └── Mikan (默认)

3. 获取 RSS 条目
   └── 通过 RssClient 抓取

4. 过滤条目
   ├── Include 过滤器 (AND 逻辑，必须全部匹配)
   └── Exclude 过滤器 (OR 逻辑，匹配任一则排除)
       ├── 全局排除过滤器 (来自设置)
       └── RSS 专属排除过滤器

5. 自动完成模式处理
   ├── auto_complete=true: 处理所有条目
   └── auto_complete=false: 只处理最新一集

6. 检查是否已存在
   ├── 检查 info_hash 是否存在
   └── 检查 (bangumi_id, episode) 是否存在
       ├── 备用 RSS: 存在则跳过
       └── 主 RSS: 存在且来自备用 RSS 则覆盖

7. 创建 Torrent 记录
   └── 解析标题提取集数

8. 添加下载任务
   ├── 生成保存路径
   ├── 生成文件名 (Plex/Jellyfin 兼容)
   └── 添加 "moe" 标签
```

## 过滤器

### Include 过滤器

- 使用 AND 逻辑：标题必须匹配**所有**过滤器
- 支持正则表达式
- 大小写不敏感

### Exclude 过滤器

- 使用 OR 逻辑：标题匹配**任一**过滤器则排除
- 合并全局和 RSS 专属过滤器
- 支持正则表达式
- 大小写不敏感

### 示例

```json
{
  "include_filters": ["1080p", "简体"],
  "exclude_filters": ["HEVC", "合集"]
}
```

上述配置会：
- 只保留同时包含 "1080p" 和 "简体" 的条目
- 排除包含 "HEVC" 或 "合集" 的条目

## 文件命名

使用 `pathgen` crate 生成 Plex/Jellyfin 兼容的文件名：

```
{bangumi.save_path}/{title} - S{season}E{episode}.{ext}
```

## 错误处理

| 错误类型 | 处理方式 |
|----------|----------|
| 数据库错误 | 记录日志，继续处理下一个 |
| RSS 获取失败 | 记录日志，继续处理下一个 |
| Bangumi 不存在 | 跳过该 RSS |
| 标题解析失败 | 跳过该条目 |
| 下载添加失败 | 记录错误，继续处理 |

## 配置

### 全局排除过滤器

通过设置服务配置：

```rust
settings.filter.global_rss_filters
```

### 调度间隔

目前硬编码为 1 小时，定义在 `RssFetchJob::interval()`。
