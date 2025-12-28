# 文件重命名流程

## 概述

系统通过定时任务扫描下载器中带有 `rename` tag 的已完成任务，并根据任务来源（moe 管理 / 外部）采用不同的重命名策略。

## 流程图

```
┌─────────────────────────────────────────────────────────────┐
│              FileRenameJob (每 10 分钟)                      │
└─────────────────────────────────────────────────────────────┘
                            │
                            ▼
               get_tasks_info(rid) → SyncMainData
                            │
                            ▼
          ┌─────────────────────────────────────┐
          │ 过滤: state=completed               │
          │       && tags 含 "rename"           │
          └─────────────────────────────────────┘
                            │
              ┌─────────────┴─────────────┐
              ▼                           ▼
        有 "moe" tag                无 "moe" tag
              │                           │
              ▼                           ▼
    ┌─────────────────────┐     ┌─────────────────────┐
    │  查找数据库记录      │     │  Parser 解析文件名   │
    │  - Torrent          │     │  提取 episode/season │
    │  - DownloadTask     │     │  title 等信息        │
    │  - Bangumi          │     └─────────────────────┘
    └─────────────────────┘               │
              │                           ▼
              ▼                 ┌─────────────────────┐
    ┌─────────────────────┐     │  Pathgen 生成路径   │
    │ FileRenameService   │     │  简化版（无 tmdb）   │
    │ 使用 Bangumi 信息    │     │  {title} - sXXeYY   │
    │ 生成规范路径         │     └─────────────────────┘
    └─────────────────────┘               │
              │                           │
              └───────────┬───────────────┘
                          ▼
              downloader.rename_file()
                          │
                          ▼
              remove_tags("rename")
```

## Tag 机制

### rename tag

- **添加时机**: `DownloaderService::add_task()` 自动添加
- **作用**: 标记需要处理的任务
- **移除时机**: 重命名成功后自动移除

### moe tag

- **添加时机**: RSS 处理添加任务时 (`rss_processing.rs`)
- **作用**: 区分 moe 系统管理的任务和外部任务
- **移除时机**: 不移除（永久标记）

## 重命名策略

### Moe 任务 (有 moe tag)

通过数据库记录获取完整信息，生成规范的 Plex/Jellyfin 兼容路径：

```
/Media/Bangumi/迷宫饭 (2024) {tmdb-119121}/Season 01/迷宫饭 - s01e12.mkv
```

使用 `FileRenameService` 处理，支持：
- **Episode 策略**: 单集种子，重命名最大视频文件
- **Collection 策略**: 合集种子，解析每个文件名

### 外部任务 (无 moe tag)

直接从文件名解析信息，生成简化路径：

```
原始: [ANi] Spy x Family S2 - 01 [1080P].mkv
解析: episode=1, season=2, title="Spy x Family S2"
新名: Spy x Family S2 - s02e01.mkv
```

限制：
- 仅处理单文件种子
- 多文件种子暂时跳过（留空）
- 保留原目录结构

## 关键代码

### DownloaderService 标签管理

```rust
// 添加标签
pub async fn add_tags(&self, hash: &str, tags: &[&str]) -> Result<()>;

// 移除标签
pub async fn remove_tags(&self, hash: &str, tags: &[&str]) -> Result<()>;
```

### RSS 处理添加 moe 标签

```rust
// crates/server/src/services/rss_processing.rs
let options = AddTorrentOptions::new(torrent_url)
    .save_path(&save_path)
    .rename(&filename)
    .add_tag("moe");  // 标记为 moe 系统管理
```

### FileRenameJob 核心逻辑

```rust
// crates/server/src/services/scheduler/file_rename_job.rs

// 检查标签
fn has_tag(tags_str: &Option<String>, tag: &str) -> bool {
    tags_str
        .as_ref()
        .is_some_and(|tags| tags.split(',').any(|t| t.trim() == tag))
}

// 检查完成状态
fn is_completed(state: &Option<String>, progress: &Option<f64>) -> bool {
    progress.unwrap_or(0.0) >= 1.0
        || matches!(
            state.as_deref(),
            Some("uploading" | "stalledUP" | "pausedUP" | ...)
        )
}
```

## 数据流

### Moe 任务流程

```
RSS Feed → RssProcessingService
                ↓
    AddTorrentOptions.add_tag("moe")
    AddTorrentOptions.add_tag("rename")  // 自动
                ↓
         qBittorrent 下载
                ↓
       FileRenameJob 扫描
                ↓
    TorrentRepository::get_by_info_hash()
    DownloadTaskRepository::get_latest_by_torrent_id()
    BangumiRepository::get_by_id()
                ↓
      FileRenameService::rename_completed_torrent()
                ↓
      downloader.remove_tags("rename")
```

### 外部任务流程

```
手动添加 / 其他来源
                ↓
    AddTorrentOptions.add_tag("rename")  // 自动
    (无 "moe" tag)
                ↓
         qBittorrent 下载
                ↓
       FileRenameJob 扫描
                ↓
    downloader.get_task_files()
    parser.parse(filename)
    pathgen::generate_filename()
                ↓
    downloader.rename_file()
                ↓
    downloader.remove_tags("rename")
```

## 增量同步

使用 qBittorrent 的 `sync/maindata` API 实现增量更新：

```rust
pub struct FileRenameJob {
    rid: AtomicI64,  // 响应 ID，实现增量同步
}

// 首次调用 rid=0 获取全量数据
// 后续调用传上次返回的 rid 只获取变化
let sync_data = downloader.get_tasks_info(rid).await?;
self.rid.store(sync_data.rid, Ordering::Relaxed);
```

## 错误处理

| 场景 | 处理方式 |
|------|---------|
| 数据库记录不存在 | 跳过，保留 rename tag |
| 无视频文件 | 标记成功，移除 tag |
| 解析失败 | 跳过，保留 rename tag |
| 重命名失败 | 记录错误，保留 tag 等待重试 |

## 相关文件

| 文件 | 职责 |
|------|------|
| `services/scheduler/file_rename_job.rs` | 定时任务主逻辑 |
| `services/file_rename.rs` | FileRenameService 入口 |
| `services/file_rename/strategy.rs` | 重命名策略实现 |
| `services/downloader.rs` | 下载器服务（含标签管理） |
| `services/rss_processing.rs` | RSS 处理（添加 moe 标签） |

## 未来扩展

1. **多文件支持**: 实现外部任务的多文件重命名
2. **失败重试限制**: 添加重试计数，多次失败后清理 tag
