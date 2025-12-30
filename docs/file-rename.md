# 文件重命名流程

## 概述

系统通过定时任务扫描下载器中带有 `rename` tag 的已完成任务，根据数据库中的 Bangumi 信息自动重命名为 Plex/Jellyfin 兼容格式。

## 流程图

```
┌─────────────────────────────────────────────────────────────┐
│              RenameJob (每 10 分钟)                          │
└─────────────────────────────────────────────────────────────┘
                            │
                            ▼
     get_tasks(status=completed/seeding, tag="rename")
                            │
                            ▼
          ┌─────────────────────────────────────┐
          │ 遍历任务，匹配数据库记录              │
          │  - TorrentRepository::get_by_info_hash │
          │  - BangumiRepository::get_by_id        │
          └─────────────────────────────────────┘
                            │
              ┌─────────────┴─────────────┐
              ▼                           ▼
        有数据库记录                  无数据库记录
              │                           │
              ▼                           ▼
    ┌─────────────────────┐          跳过（保留 tag）
    │ RenameService       │
    │ 处理重命名           │
    └─────────────────────┘
              │
              ▼
    ┌─────────────────────┐
    │ 1. 重命名字幕文件    │
    │ 2. 重命名视频文件    │
    │ 3. 生成 NFO 元数据   │
    └─────────────────────┘
              │
              ▼
      remove_tags("rename")
```

## 重命名策略

### 命名规范

生成 Plex/Jellyfin 兼容的文件名：

```
原始: [ANi] 迷宫饭 - 12 [1080P].mkv
新名: 迷宫饭 (2024) [Mikan] - s01e12.mkv

完整路径示例:
/downloads/迷宫饭/迷宫饭 (2024) [Mikan] - s01e12.mkv
```

### 处理逻辑

对每个视频文件：
- **单文件 torrent**: 优先使用数据库存储的 `episode_number`，否则从文件名解析
- **多文件 torrent**: 始终从每个文件名解析集数

### 关联文件处理

1. **字幕文件**: 自动匹配并重命名 `.ass/.srt/.ssa/.sub/.vtt` 文件
2. **NFO 元数据**: 自动生成包含 TMDB/BGM.tv ID 的元数据文件

## 核心组件

### RenameService

主要职责：
- 查询待处理任务
- 匹配数据库记录
- 执行文件重命名
- 生成 NFO 文件

```rust
// crates/server/src/services/rename.rs
pub struct RenameService {
    db: SqlitePool,
    downloader: Arc<DownloaderService>,
    parser: Parser,
}

impl RenameService {
    /// 处理所有待重命名任务（调度器入口）
    pub async fn process_all(&self) -> Result<()>;

    /// 处理单个任务
    async fn process_task(&self, task: &Task, torrent: &Torrent, bangumi: &Bangumi) -> Result<()>;

    /// 重命名单个视频文件及关联字幕
    async fn rename_file(&self, task: &Task, file: &TaskFile, bangumi: &Bangumi, episode: i32, all_files: &[TaskFile]) -> Result<()>;
}
```

### RenameJob

调度任务包装：

```rust
// crates/server/src/services/scheduler/rename_job.rs
pub struct RenameJob {
    rename_service: Arc<RenameService>,
}

impl SchedulerJob for RenameJob {
    fn name(&self) -> &'static str { "FileRename" }
    fn interval(&self) -> Duration { Duration::from_secs(600) } // 10 分钟

    async fn execute(&self) -> JobResult {
        self.rename_service.process_all().await?;
        Ok(())
    }
}
```

### Downloader rename_file

下载器层的文件重命名接口：

```rust
// crates/downloader/src/traits.rs
#[async_trait]
pub trait Downloader: Send + Sync {
    /// 重命名任务中的文件
    async fn rename_file(&self, id: &str, old_path: &str, new_path: &str) -> Result<()>;
}
```

支持的下载器：
- **qBittorrent**: 调用 `torrents/renameFile` API
- **Transmission**: 调用 `torrent-rename-path` RPC

## Tag 机制

### rename tag

- **添加时机**: RSS 处理添加任务时自动添加
- **作用**: 标记需要重命名的任务
- **移除时机**: 重命名成功后自动移除

### moe tag

- **添加时机**: RSS 处理添加任务时
- **作用**: 标记 moe 系统管理的任务（有数据库记录）
- **移除时机**: 不移除（永久标记）

## 数据流

```
RSS Feed → RssProcessingService
                ↓
    创建 Torrent/DownloadTask 记录
    AddTaskOptions.add_tag("moe")
    AddTaskOptions.add_tag("rename")
                ↓
         下载器开始下载
                ↓
         下载完成 (seeding/completed)
                ↓
       RenameJob 定时扫描
                ↓
    TorrentRepository::get_by_info_hash()
    BangumiRepository::get_by_id()
                ↓
      RenameService::process_task()
        1. rename_subtitles()    ← 先重命名字幕
        2. rename_file()         ← 再重命名视频
        3. write_nfo_file()      ← 生成 NFO
                ↓
      downloader.remove_tags("rename")
```

## NFO 文件格式

生成的 NFO 文件示例：

```xml
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<episodedetails>
  <title>Episode 12</title>
  <showtitle>迷宫饭</showtitle>
  <season>1</season>
  <episode>12</episode>
  <year>2024</year>
  <aired>2024-01-01</aired>
  <tmdbid>119121</tmdbid>
  <uniqueid type="tmdb" default="true">119121</uniqueid>
  <uniqueid type="bangumi">12345</uniqueid>
  <original_filename>[ANi] 迷宫饭 - 12 [1080P].mkv</original_filename>
</episodedetails>
```

## 错误处理

| 场景 | 处理方式 |
|------|---------|
| 数据库记录不存在 | 跳过，保留 rename tag |
| Bangumi 不存在 | 跳过，记录警告 |
| 无视频文件 | 移除 tag（无需处理） |
| 解析集数失败 | 跳过该文件，继续处理其他 |
| 重命名失败 | 记录错误，保留 tag 等待重试 |

## 相关文件

| 文件 | 职责 |
|------|------|
| `services/rename.rs` | RenameService 核心逻辑 |
| `services/scheduler/rename_job.rs` | RenameJob 调度任务 |
| `services/downloader.rs` | DownloaderService（含 rename_file） |
| `downloader/src/traits.rs` | Downloader trait 定义 |
| `downloader/src/impls/qbittorrent.rs` | qBittorrent 实现 |
| `downloader/src/impls/transmission.rs` | Transmission 实现 |

## 未来扩展

1. **外部任务支持**: 支持无数据库记录的任务（纯解析模式）
2. **失败重试限制**: 添加重试计数，多次失败后清理 tag
3. **手动触发**: 添加 API 端点支持手动触发重命名
