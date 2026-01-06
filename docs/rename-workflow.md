# 剧集重命名工作流架构指南

## 概述

剧集重命名系统负责将下载完成的视频文件自动整理为媒体库兼容格式。核心特性包括：

- **定时扫描**：每 10 分钟检查已完成的下载任务
- **标签驱动**：通过 `rename` 标签标识待处理任务
- **Plex/Jellyfin 兼容**：生成标准命名格式 `{标题} - s{季}e{集}.{扩展名}`
- **字幕同步**：自动重命名关联的字幕文件
- **进度追踪**：更新 Bangumi 的 `current_episode`
- **自动完结**：当 `current_episode >= total_episodes` 时自动禁用 RSS
- **更新通知**：处理完成后发送通知（支持海报图片）

## 架构层次

```
┌─────────────────────────────────────────────────────────────┐
│                        触发层                                │
│  ┌────────────────────────────────────────────────────────┐ │
│  │                    RenameJob                           │ │
│  │                  (每 10 分钟定时)                       │ │
│  └──────────────────────────┬─────────────────────────────┘ │
└─────────────────────────────┼───────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                      核心处理层                              │
│  ┌────────────────────────────────────────────────────────┐ │
│  │                   RenameService                        │ │
│  │  • process_all()    - 处理所有待重命名任务              │ │
│  │  • process_task()   - 处理单个任务                     │ │
│  │  • rename_file()    - 重命名视频+字幕                  │ │
│  └────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                       依赖服务层                             │
│  ┌──────────────────┐ ┌──────────────────┐ ┌─────────────┐  │
│  │ DownloaderHandle │ │ NotificationSvc  │ │   PathGen   │  │
│  │   (Actor模式)    │ │   (更新通知)     │ │  (文件名)   │  │
│  └──────────────────┘ └──────────────────┘ └─────────────┘  │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                       数据访问层                             │
│  ┌──────────────────────┐    ┌───────────────────────────┐  │
│  │  TorrentRepository   │    │    BangumiRepository      │  │
│  └──────────────────────┘    └───────────────────────────┘  │
└─────────────────────────────────────────────────────────────┘
```

## 触发条件

| 条件 | 说明 |
|------|------|
| 定时任务 | 每 10 分钟执行一次 |
| 任务状态 | `Completed` 或 `Seeding` |
| 标签要求 | 必须有 `rename` 标签 |
| 数据库匹配 | `info_hash` 在 Torrent 表中存在 |

## 数据流程

```
下载完成 ──► 定时扫描 ──► 查询匹配 ──► 文件处理 ──► 收尾工作
             RenameJob     数据库       重命名        更新+通知
```

### 详细流程

```
1. 查询待处理任务
   get_pending_tasks()
   ├── 查询下载器：状态 ∈ {Completed, Seeding} 且有 "rename" 标签
   ├── 通过 info_hash 匹配数据库 Torrent 记录
   └── 获取关联的 Bangumi + 元数据

2. 并发处理（最多 4 个同时）
   stream::buffer_unordered(4)
   └── 对每个任务调用 process_task()

3. 处理单个任务
   process_task()
   ├── 获取任务的所有文件
   ├── 筛选视频文件（按扩展名）
   └── 对每个视频文件：
       ├── 确定剧集号（数据库或文件名解析）
       └── 调用 rename_file()

4. 重命名文件
   rename_file()
   ├── 应用剧集偏移量（episode_offset）
   ├── 使用 pathgen 生成新文件名
   ├── 重命名字幕文件
   └── 重命名视频文件

5. 收尾工作
   finalize_task()
   ├── 移除 "rename" 标签
   ├── 按 bangumi_id 分组结果
   ├── 更新 current_episode（仅当 > 现有值）
   ├── 自动禁用 RSS（当 current_episode >= total_episodes）
   └── 发送通知
```

## 核心组件

### 调度器 (SchedulerService)

通用的定时任务调度器，管理多个 `SchedulerJob` 实例。

```rust
// 位置: services/scheduler.rs
impl SchedulerService {
    pub fn start(&self) {
        for job in &self.jobs {
            tokio::spawn(async move {
                let mut timer = tokio::time::interval(job.interval());
                timer.set_missed_tick_behavior(MissedTickBehavior::Skip);
                loop {
                    timer.tick().await;
                    job.execute().await;
                }
            });
        }
    }
}
```

### 重命名任务 (RenameJob)

实现 `SchedulerJob` trait，每 10 分钟触发一次。

```rust
// 位置: services/scheduler/rename_job.rs
impl SchedulerJob for RenameJob {
    fn name(&self) -> &'static str { "FileRename" }
    fn interval(&self) -> Duration { Duration::from_secs(600) }

    async fn execute(&self) -> JobResult {
        self.rename_service.process_all().await
    }
}
```

### 重命名服务 (RenameService)

核心业务逻辑，负责文件重命名、通知发送。

主要方法：
| 方法 | 功能 |
|------|------|
| `process_all()` | 入口，处理所有待重命名任务 |
| `get_pending_tasks()` | 查询待处理任务并匹配数据库 |
| `process_task()` | 处理单个下载任务 |
| `rename_file()` | 重命名视频+字幕 |
| `rename_subtitles()` | 重命名关联字幕文件 |

### 标签机制

通过下载器标签控制任务生命周期：

```
添加任务时 ──► 自动打标签 ["moe", "rename"]
                              │
重命名完成后 ──► 移除 "rename" 标签
```

| 标签 | 用途 |
|------|------|
| `moe` | 标识由本应用添加的任务 |
| `rename` | 标识需要重命名处理的任务 |

标签定义位置：`services/downloader.rs`

```rust
mod tags {
    pub const MOE: &str = "moe";
    pub const RENAME: &str = "rename";
}
```

## 文件重命名逻辑

### 视频文件

使用 `pathgen` 模块生成 Plex/Jellyfin 兼容的文件名。

**命名格式**：`{标题} - s{季:02}e{集:02}.{扩展名}`

**示例**：
```
原文件: [ANi] 迷宮飯 - 05 [1080p].mkv
新文件: 迷宫饭 - s01e05.mkv
```

**剧集号确定逻辑**：
```
单文件任务 → 优先使用数据库 episode_number，否则解析文件名
多文件任务 → 必须从文件名解析（每个文件独立）
```

**剧集偏移**：通过 `Bangumi.episode_offset` 字段调整剧集号
- 用于处理 RSS 中剧集号与实际季度剧集号不一致的情况
- 示例：RSS 第 13 集 + offset(-12) = s02e01

### 字幕文件

自动识别并重命名与视频同名的字幕文件。

**支持格式**：`ass`, `srt`, `ssa`, `sub`, `vtt`

**匹配规则**：
```
video.mkv  →  video.ass           (直接匹配)
           →  video.zh-CN.ass     (带语言标签)
           →  video.简体中文.ass   (带中文标签)
```

**重命名示例**：
```
原文件: [ANi] 迷宮飯 - 05 [1080p].zh-CN.ass
新文件: 迷宫饭 - s01e05.zh-CN.ass
```

## 关键数据模型

### Task（下载器任务）

```
Task
├── id: String           # info_hash，与 Torrent 表关联
├── name: String         # 任务名称
├── status: TaskStatus   # Downloading, Completed, Seeding 等
├── save_path: String    # 保存路径
└── tags: Vec<String>    # 包括 "moe", "rename"
```

### Torrent（数据库记录）

```
Torrent
├── id: i64
├── bangumi_id: i64           # 关联的番剧
├── info_hash: String         # 与 Task.id 对应
├── episode_number: Option    # 单文件任务的剧集号
└── ...
```

### BangumiWithMetadata

```
BangumiWithMetadata
├── bangumi
│   ├── id, name
│   └── episode_offset        # 剧集偏移量
└── metadata
    ├── title_chinese         # 用于生成文件名
    ├── season                # 季度
    ├── year, air_date
    ├── tmdb_id, bgmtv_id     # 外部 ID
    └── poster_url            # 用于通知
```

### RenameTaskResult

```
RenameTaskResult
├── bangumi_id: i64
├── bangumi_title: String
├── poster_url: Option<String>
└── renamed_episodes: Vec<i32>  # 成功重命名的剧集号列表
```

## 关键文件

| 功能 | 位置 |
|------|------|
| 调度器主服务 | `crates/server/src/services/scheduler.rs` |
| 重命名定时任务 | `crates/server/src/services/scheduler/rename_job.rs` |
| SchedulerJob trait | `crates/server/src/services/scheduler/traits.rs` |
| 重命名服务核心 | `crates/server/src/services/rename.rs` |
| 标签定义 | `crates/server/src/services/downloader.rs` |
| DownloaderHandle | `crates/server/src/services/downloader/actor/handle.rs` |
| Actor 消息处理 | `crates/server/src/services/downloader/actor/runner.rs` |
| 文件名生成 | `crates/pathgen/src/lib.rs` |
| 应用初始化 | `crates/server/src/state.rs` |

## 自动完结机制

当番剧下载完成所有集数时，系统会自动禁用相关 RSS 订阅。

### 触发条件

```
current_episode >= total_episodes && total_episodes > 0
```

- `total_episodes = 0` 表示集数未知，不会触发自动禁用
- 使用 `>=` 而非 `==`，避免因剧集偏移导致的边界问题

### 处理逻辑

```
重命名完成
    │
    ▼
更新 current_episode
    │
    ▼
检查 current_episode >= total_episodes?
    │
    ├── 否 → 继续
    │
    └── 是 → 禁用该 bangumi 的所有 RSS
              │
              └── 记录日志：Auto-disabled N RSS subscription(s)
```

### 相关方法

| 方法 | 位置 | 功能 |
|------|------|------|
| `RssRepository::disable_by_bangumi_id()` | `repositories/rss.rs` | 禁用指定番剧的所有 RSS |
