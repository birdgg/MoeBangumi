# Downloader 抽象层

## 概述

`downloader` crate 提供了统一的下载器接口，通过 **Trait + 枚举分发** 模式抽象不同的 torrent 客户端。目前支持 qBittorrent，架构设计可扩展至其他客户端。

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
│     - 未来可添加: Transmission, Aria2, openlist.              │
└─────────────────────────────────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────┐
│           Downloader + DownloaderExt Traits                  │
│     - 统一接口定义                                           │
│     - 核心操作 vs 扩展功能分离                                │
└─────────────────────────────────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────┐
│           qbittorrent crate (底层 API)                       │
│     - qBittorrent Web API 封装                               │
└─────────────────────────────────────────────────────────────┘
```

## Trait 接口

### Downloader (核心接口)

所有下载器必须实现的基本操作。

```rust
#[async_trait]
pub trait Downloader: Send + Sync {
    /// 登录到下载器
    async fn login(&self) -> Result<()>;

    /// 检查是否已登录
    async fn is_login(&self) -> Result<bool>;

    /// 获取所有任务（等价于 get_tasks_filtered(None, None)）
    async fn get_tasks(&self) -> Result<Vec<TorrentInfo>> {
        self.get_tasks_filtered(None, None).await
    }

    /// 获取任务（支持过滤）
    /// filter: "completed", "downloading", "seeding", "stopped", etc.
    /// tag: 按标签过滤
    async fn get_tasks_filtered(
        &self,
        filter: Option<&str>,
        tag: Option<&str>,
    ) -> Result<Vec<TorrentInfo>>;

    /// 获取增量同步数据
    /// rid=0 获取全量，后续传入返回的 rid 获取增量
    async fn get_tasks_info(&self, rid: i64) -> Result<SyncMainData>;

    /// 添加新任务，返回任务标识
    async fn add_task(&self, options: AddTorrentOptions) -> Result<String>;

    /// 暂停任务，hashes=["all"] 暂停全部
    async fn pause_task(&self, hashes: &[&str]) -> Result<()>;

    /// 恢复任务，hashes=["all"] 恢复全部
    async fn resume_task(&self, hashes: &[&str]) -> Result<()>;

    /// 删除任务
    async fn delete_task(&self, hashes: &[&str], delete_files: bool) -> Result<()>;

    /// 添加标签
    async fn add_tags(&self, hash: &str, tags: &[&str]) -> Result<()>;

    /// 移除标签，tags 为空则移除全部
    async fn remove_tags(&self, hash: &str, tags: &[&str]) -> Result<()>;

    /// 获取任务内的文件列表
    async fn get_task_files(&self, hash: &str) -> Result<Vec<TorrentFile>>;

    /// 重命名任务内的文件
    async fn rename_file(&self, hash: &str, old_path: &str, new_path: &str) -> Result<()>;

    /// 获取下载器类型名称
    fn downloader_type(&self) -> &'static str;
}
```

### DownloaderExt (扩展接口)

可选的高级功能，默认返回 `NotSupported` 错误。

```rust
#[async_trait]
pub trait DownloaderExt: Downloader {
    /// 通过 hash 获取单个任务信息
    async fn get_task_info(&self, hash: &str) -> Result<Option<TorrentInfo>>;

    /// 配置 autorun webhook（任务完成时回调）
    async fn configure_autorun(&self, webhook_url: &str) -> Result<()>;

    /// 禁用 autorun 回调
    async fn disable_autorun(&self) -> Result<()>;
}
```

## 数据模型

### TorrentInfo

```rust
pub struct TorrentInfo {
    pub hash: String,      // 任务哈希
    pub name: String,      // 任务名称
    pub state: String,     // 状态: downloading, uploading, pausedDL, etc.
    pub progress: f64,     // 进度: 0.0 ~ 1.0
    pub save_path: String, // 保存路径
    pub size: i64,         // 总大小 (bytes)
    pub downloaded: i64,   // 已下载 (bytes)
    pub eta: i64,          // 预计剩余时间 (seconds)
    pub tags: String,      // 标签 (逗号分隔)
}

impl TorrentInfo {
    /// 检查是否已完成下载
    pub fn is_completed(&self) -> bool;
}
```

### TorrentFile

```rust
pub struct TorrentFile {
    pub index: i32,     // 文件索引
    pub name: String,   // 文件名（含相对路径）
    pub size: i64,      // 文件大小 (bytes)
    pub progress: f64,  // 下载进度
    pub priority: i32,  // 优先级: 0=不下载, 1-7=优先级
    pub is_seed: bool,  // 是否做种
}

impl TorrentFile {
    pub fn is_completed(&self) -> bool;
    pub fn is_video(&self) -> bool;      // 检查是否为视频文件
    pub fn extension(&self) -> Option<&str>;
}
```

### AddTorrentOptions

```rust
pub struct AddTorrentOptions {
    pub url: String,
    pub save_path: Option<String>,
    pub category: Option<String>,
    pub tags: Vec<String>,
    pub rename: Option<String>,
}

// Builder 模式
AddTorrentOptions::new(url)
    .save_path("/downloads")
    .category("anime")
    .add_tag("moe")
    .rename("My Torrent");
```

### SyncMainData (增量同步)

```rust
pub struct SyncMainData {
    pub rid: i64,                                    // 下次请求使用的响应 ID
    pub full_update: bool,                           // 是否全量更新
    pub torrents: HashMap<String, SyncTorrentInfo>,  // 变更的任务
    pub torrents_removed: Vec<String>,               // 已删除的任务 hash
    pub server_state: Option<ServerState>,           // 服务器状态
}
```

## Filter 类型

`get_tasks_filtered` 支持的 filter 参数：

| 值 | 说明 |
|---|------|
| `all` | 所有任务 |
| `downloading` | 正在下载 |
| `seeding` | 正在做种 |
| `completed` | 已完成 |
| `stopped` | 已停止 |
| `active` | 活动中 |
| `inactive` | 非活动 |
| `stalled` | 停滞 |
| `stalled_uploading` | 做种停滞 |
| `stalled_downloading` | 下载停滞 |
| `errored` | 出错 |
| `running` | 运行中 |

## 使用示例

### 基本查询

```rust
// 获取所有任务
let tasks = downloader.get_tasks().await?;

// 获取已完成且带 "rename" 标签的任务
let tasks = downloader
    .get_tasks_filtered(Some("completed"), Some("rename"))
    .await?;

// 检查特定任务
let task = downloader.get_task_info("abc123hash").await?;
```

### 任务管理

```rust
// 添加任务
let options = AddTorrentOptions::new("magnet:?xt=...")
    .save_path("/downloads/anime")
    .add_tag("moe");
downloader.add_task(options).await?;

// 暂停/恢复
downloader.pause_task(&["hash1", "hash2"]).await?;
downloader.resume_task(&["all"]).await?;

// 删除（保留文件）
downloader.delete_task(&["hash1"], false).await?;
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

// 重命名文件
downloader.rename_file("hash", "old/path.mkv", "new/path.mkv").await?;
```

## DownloaderService

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
pub async fn add_task(&self, options: AddTorrentOptions) -> Result<String> {
    let options = options.add_tag("rename");  // 自动添加
    // ...
}
```

## 错误处理

```rust
pub enum DownloaderError {
    Auth(String),           // 认证错误
    Config(String),         // 配置错误
    NotConfigured,          // 未配置
    NotSupported(String),   // 功能不支持
    QBittorrent(...),       // qBittorrent 特定错误
}
```

## 扩展新客户端

添加新的下载器实现只需 3 步：

1. 创建 `XXXDownloader` 结构体
2. 实现 `Downloader` trait（+ 可选 `DownloaderExt`）
3. 在 `DownloaderClient` 枚举添加新变体

```rust
// 1. 定义结构体
pub struct TransmissionDownloader { ... }

// 2. 实现 trait
impl Downloader for TransmissionDownloader { ... }

// 3. 添加到枚举
pub enum DownloaderClient {
    QBittorrent(QBittorrentDownloader),
    Transmission(TransmissionDownloader),  // 新增
}
```

## 相关文件

| 文件 | 职责 |
|------|------|
| `crates/downloader/src/traits.rs` | Trait 定义 |
| `crates/downloader/src/client.rs` | 枚举分发 |
| `crates/downloader/src/qbittorrent_impl.rs` | qBittorrent 实现 |
| `crates/downloader/src/models.rs` | 数据模型 |
| `crates/downloader/src/error.rs` | 错误类型 |
| `crates/server/src/services/downloader.rs` | 服务层封装 |
| `crates/qbittorrent/` | qBittorrent Web API 客户端 |
