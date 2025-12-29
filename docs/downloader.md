# Downloader 抽象层

## 概述

`downloader` crate 提供了统一的下载器接口，通过 **Trait + 枚举分发** 模式抽象不同的 torrent 客户端。目前支持 qBittorrent 和 Transmission，架构设计可扩展至其他客户端。

## 设计理念

- **通用**: 所有方法适用于 BitTorrent、HTTP 等协议
- **类型安全**: 强类型模型替代字符串 API
- **异步优先**: 所有操作都是异步的

## 架构分层

```
┌─────────────────────────────────────────────────────────────┐
│                    API 层 (handlers)                         │
└─────────────────────────────────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────┐
│           DownloaderService (服务层)                         │
│     - 客户端生命周期管理                                      │
│     - 认证缓存与自动重试                                      │
│     - 配置变更检测                                           │
└─────────────────────────────────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────┐
│           DownloaderClient (枚举分发)                        │
│     - QBittorrent(QBittorrentDownloader)                    │
│     - Transmission(TransmissionDownloader)                  │
│     - 未来可添加: Aria2, Deluge 等                           │
└─────────────────────────────────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────┐
│               Downloader Trait (核心接口)                    │
│     - 8 个核心方法                                           │
│     - Send + Sync 线程安全                                   │
└─────────────────────────────────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────┐
│           底层 API 客户端                                    │
│     - qbittorrent crate                                     │
│     - transmission-rpc                                      │
└─────────────────────────────────────────────────────────────┘
```

## Trait 接口

### Downloader (核心接口 - 8 个方法)

```rust
#[async_trait]
pub trait Downloader: Send + Sync {
    /// 登录认证
    async fn login(&self) -> Result<()>;

    /// 检查是否已登录
    async fn is_login(&self) -> Result<bool>;

    /// 添加新任务，返回任务 ID
    async fn add_task(&self, options: AddTaskOptions) -> Result<String>;

    /// 删除任务
    async fn delete_task(&self, ids: &[&str], delete_files: bool) -> Result<()>;

    /// 获取任务列表（支持过滤）
    async fn get_tasks(&self, filter: Option<&TaskFilter>) -> Result<Vec<Task>>;

    /// 获取任务内的文件列表
    async fn get_task_files(&self, id: &str) -> Result<Vec<TaskFile>>;

    /// 添加标签
    async fn add_tags(&self, id: &str, tags: &[&str]) -> Result<()>;

    /// 移除标签，tags 为空则移除全部
    async fn remove_tags(&self, id: &str, tags: &[&str]) -> Result<()>;
}
```

## 数据模型

### Task (下载任务)

```rust
pub struct Task {
    pub id: String,              // 任务 ID (BitTorrent 为 hash)
    pub name: String,            // 任务名称
    pub status: TaskStatus,      // 状态枚举
    pub progress: f64,           // 进度: 0.0 ~ 1.0
    pub save_path: String,       // 保存路径
    pub total_size: i64,         // 总大小 (bytes)
    pub downloaded: i64,         // 已下载 (bytes)
    pub eta: i64,                // 预计剩余时间 (seconds, -1 表示未知)
    pub tags: String,            // 标签 (逗号分隔)
    pub category: Option<String>, // 分类 (可选)
}

impl Task {
    pub fn is_completed(&self) -> bool;  // 检查是否已完成
    pub fn tags_vec(&self) -> Vec<String>; // 获取标签列表
    pub fn has_tag(&self, tag: &str) -> bool; // 检查是否有特定标签
}
```

### TaskStatus (任务状态)

```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TaskStatus {
    Queued,       // 排队中
    Downloading,  // 下载中
    Paused,       // 已暂停
    Seeding,      // 做种中 (BitTorrent)
    Completed,    // 已完成
    Stalled,      // 停滞
    Checking,     // 校验中
    Error,        // 出错
    Unknown,      // 未知
}

impl TaskStatus {
    pub fn is_active(&self) -> bool;   // 是否活跃传输
    pub fn is_finished(&self) -> bool; // 是否已完成下载
}
```

### TaskFile (任务文件)

```rust
pub struct TaskFile {
    pub index: i32,      // 文件索引
    pub path: String,    // 文件路径 (相对于 save_path)
    pub size: i64,       // 文件大小 (bytes)
    pub progress: f64,   // 下载进度
}

impl TaskFile {
    pub fn is_completed(&self) -> bool;   // 是否已完成
    pub fn is_video(&self) -> bool;       // 是否为视频文件
    pub fn extension(&self) -> Option<&str>; // 获取扩展名
}
```

### AddTaskOptions (添加任务选项)

```rust
pub struct AddTaskOptions {
    pub url: String,               // 下载 URL (HTTP, magnet, torrent)
    pub save_path: Option<String>, // 保存路径
    pub category: Option<String>,  // 分类
    pub tags: Vec<String>,         // 标签
    pub rename: Option<String>,    // 重命名
}

// Builder 模式
AddTaskOptions::new("magnet:?xt=...")
    .save_path("/downloads/anime")
    .category("anime")
    .add_tag("moe")
    .rename("My Anime");
```

### TaskFilter (任务过滤器)

```rust
pub struct TaskFilter {
    pub status: Option<TaskStatus>,  // 按状态过滤
    pub category: Option<String>,    // 按分类过滤
    pub tag: Option<String>,         // 按标签过滤
    pub ids: Option<Vec<String>>,    // 按 ID 过滤
}

// Builder 模式
TaskFilter::new()
    .status(TaskStatus::Completed)
    .tag("rename")
    .id("abc123hash");
```

## 配置

### DownloaderType (下载器类型)

```rust
pub enum DownloaderType {
    QBittorrent,
    Transmission,
}
```

### DownloaderConfig (配置)

```rust
pub struct DownloaderConfig {
    pub downloader_type: DownloaderType,
    pub url: String,
    pub username: Option<String>,
    pub password: Option<String>,
}

// 快捷创建
DownloaderConfig::qbittorrent(url, username, password);
DownloaderConfig::transmission(url, username, password);
```

## 使用示例

### 基本查询

```rust
// 获取所有任务
let all_tasks = downloader.get_tasks(None).await?;

// 获取已完成且带 "rename" 标签的任务
let filter = TaskFilter::new()
    .status(TaskStatus::Completed)
    .tag("rename");
let completed = downloader.get_tasks(Some(&filter)).await?;

// 获取特定任务
let filter = TaskFilter::new().id("abc123hash");
let task = downloader.get_tasks(Some(&filter)).await?.pop();
```

### 任务管理

```rust
// 添加任务
let options = AddTaskOptions::new("magnet:?xt=...")
    .save_path("/downloads/anime")
    .add_tag("moe");
let task_id = downloader.add_task(options).await?;

// 删除任务（保留文件）
downloader.delete_task(&["hash1", "hash2"], false).await?;

// 删除任务并删除文件
downloader.delete_task(&["hash1"], true).await?;
```

### 标签管理

```rust
// 添加标签
downloader.add_tags("hash", &["rename", "moe"]).await?;

// 移除特定标签
downloader.remove_tags("hash", &["rename"]).await?;

// 移除所有标签
downloader.remove_tags("hash", &[]).await?;
```

### 文件操作

```rust
// 获取任务内的文件
let files = downloader.get_task_files("hash").await?;

// 找到最大的视频文件
let main_file = files.iter()
    .filter(|f| f.is_completed() && f.is_video())
    .max_by_key(|f| f.size);
```

## DownloaderService (服务层)

服务层封装，提供：

- **懒加载创建**: 首次使用时创建客户端
- **配置变更检测**: 设置更新后自动重建
- **认证重试**: 会话失效时自动重新登录
- **客户端缓存**: 使用读写锁实现并发访问

```rust
pub struct DownloaderService {
    settings: Arc<SettingsService>,
    cached: RwLock<Option<CachedClient>>,
}
```

### 自动添加 rename 标签

`DownloaderService::add_task()` 会自动为所有任务添加 `rename` 标签：

```rust
pub async fn add_task(&self, options: AddTaskOptions) -> Result<String> {
    let options = options.add_tag("rename");  // 自动添加
    // ...
}
```

## 错误处理

```rust
pub enum DownloaderError {
    Request(reqwest::Error),      // HTTP 请求错误
    QBittorrent(...),             // qBittorrent 特定错误
    Transmission(String),         // Transmission 错误
    Config(String),               // 配置错误
    Auth(String),                 // 认证错误
    NotConfigured,                // 未配置
    NotSupported(String),         // 功能不支持
}
```

## 扩展新客户端

添加新的下载器实现只需 3 步：

1. 在 `impls/` 目录创建实现文件
2. 实现 `Downloader` trait
3. 在 `DownloaderClient` 枚举添加新变体

```rust
// 1. 实现结构体 (impls/deluge.rs)
pub struct DelugeDownloader { ... }

// 2. 实现 trait
impl Downloader for DelugeDownloader {
    async fn login(&self) -> Result<()> { ... }
    async fn is_login(&self) -> Result<bool> { ... }
    async fn add_task(&self, options: AddTaskOptions) -> Result<String> { ... }
    async fn delete_task(&self, ids: &[&str], delete_files: bool) -> Result<()> { ... }
    async fn get_tasks(&self, filter: Option<&TaskFilter>) -> Result<Vec<Task>> { ... }
    async fn get_task_files(&self, id: &str) -> Result<Vec<TaskFile>> { ... }
    async fn add_tags(&self, id: &str, tags: &[&str]) -> Result<()> { ... }
    async fn remove_tags(&self, id: &str, tags: &[&str]) -> Result<()> { ... }
}

// 3. 添加到枚举 (client.rs)
pub enum DownloaderClient {
    QBittorrent(QBittorrentDownloader),
    Transmission(TransmissionDownloader),
    Deluge(DelugeDownloader),  // 新增
}
```

## 相关文件

| 文件 | 职责 |
|------|------|
| [traits.rs](../crates/downloader/src/traits.rs) | Trait 定义 |
| [client.rs](../crates/downloader/src/client.rs) | 枚举分发 |
| [config.rs](../crates/downloader/src/config.rs) | 配置结构 |
| [models.rs](../crates/downloader/src/models.rs) | 数据模型聚合 |
| [models/task.rs](../crates/downloader/src/models/task.rs) | Task 模型 |
| [models/task_status.rs](../crates/downloader/src/models/task_status.rs) | TaskStatus 枚举 |
| [models/task_file.rs](../crates/downloader/src/models/task_file.rs) | TaskFile 模型 |
| [models/add_task_options.rs](../crates/downloader/src/models/add_task_options.rs) | AddTaskOptions |
| [models/task_filter.rs](../crates/downloader/src/models/task_filter.rs) | TaskFilter |
| [impls/qbittorrent.rs](../crates/downloader/src/impls/qbittorrent.rs) | qBittorrent 实现 |
| [impls/transmission.rs](../crates/downloader/src/impls/transmission.rs) | Transmission 实现 |
| [error.rs](../crates/downloader/src/error.rs) | 错误类型 |
| [downloader.rs](../crates/server/src/services/downloader.rs) | 服务层封装 |
