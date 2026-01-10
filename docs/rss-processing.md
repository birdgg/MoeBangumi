# RSS Processing Service

RSS 处理服务负责从 RSS 订阅源获取新资源、解析元数据、管理下载任务，并支持基于优先级的洗版（washing）功能。

## 架构设计

服务采用 trait 抽象设计，分离数据库访问、RSS 获取和业务逻辑：

```
┌─────────────────────────────────────────────────────────────────┐
│                     RssProcessingService                         │
│  ┌──────────────────────────────────────────────────────────┐   │
│  │                    RssProcessor                           │   │
│  │              (纯业务逻辑，无副作用)                         │   │
│  │  - filter_items()      - parse_items()                   │   │
│  │  - filter_by_priority() - determine_action()             │   │
│  └──────────────────────────────────────────────────────────┘   │
│                              │                                   │
│         ┌────────────────────┼────────────────────┐             │
│         ▼                    ▼                    ▼             │
│  ┌─────────────┐    ┌─────────────┐    ┌──────────────┐        │
│  │RssDataAccess│    │ RssFetcher  │    │TaskScheduler │        │
│  │  (trait)    │    │  (trait)    │    │   (trait)    │        │
│  └─────────────┘    └─────────────┘    └──────────────┘        │
│         │                    │                    │             │
│         ▼                    ▼                    ▼             │
│  ┌─────────────┐    ┌─────────────┐    ┌──────────────┐        │
│  │   SQLite    │    │  RssClient  │    │DownloaderHdl │        │
│  └─────────────┘    └─────────────┘    └──────────────┘        │
└─────────────────────────────────────────────────────────────────┘
```

### Trait 定义

```rust
/// 数据库访问 trait
#[async_trait]
pub trait RssDataAccess: Send + Sync {
    async fn get_bangumi_with_series(&self, bangumi_id: i64) -> Result<Option<BangumiWithSeries>>;
    async fn get_bangumi_torrents(&self, bangumi_id: i64) -> Result<Vec<Torrent>>;
    async fn create_torrent(&self, create: CreateTorrent) -> Result<Torrent>;
    async fn delete_torrents(&self, ids: &[i64]) -> Result<()>;
    async fn update_rss_cache(&self, rss_id: i64, ...) -> Result<()>;
}

/// RSS 获取 trait
#[async_trait]
pub trait RssFetcher: Send + Sync {
    async fn fetch_conditional(&self, source: &RssSource, context: Option<&FetchContext>)
        -> Result<FetchResult>;
}

/// 下载任务调度 trait
#[async_trait]
pub trait TaskScheduler: Send + Sync {
    async fn add_download_task(&self, options: AddTaskOptions) -> Result<String>;
    async fn delete_tasks(&self, info_hashes: &[&str], delete_files: bool) -> Result<()>;
    async fn get_task_files(&self, info_hash: &str) -> Result<Vec<TaskFile>>;
}
```

### ProcessAction 枚举

业务逻辑决策结果：

```rust
pub enum ProcessAction {
    /// 跳过 - torrent 已存在或优先级较低
    Skip { reason: String },

    /// 添加新 torrent
    Add {
        info_hash: String,
        torrent_url: String,
        episode: i32,
        adjusted_episode: i32,
        filename: String,
        save_path: String,
        bangumi_id: i64,
        rss_id: Option<i64>,
    },

    /// 洗版 - 用更高优先级的 torrent 替换现有的
    Wash {
        info_hash: String,
        torrent_url: String,
        episode: i32,
        existing_torrent_ids: Vec<i64>,
        existing_info_hashes: Vec<String>,
        // ... 其他字段
    },
}
```

### 优点

1. **可测试性**: 可以 mock 数据库和外部服务进行单元测试
2. **可复用性**: RssProcessor 可被其他模块复用
3. **职责分离**: 业务逻辑与副作用分离
4. **可扩展性**: 可以轻松添加新的数据源或下载器

## 核心流程

```
┌─────────────────────────────────────────────────────────────────────────┐
│                         process_single(rss)                              │
└─────────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────────┐
│ 1. prepare_context()                                                     │
│    - 获取 BangumiWithSeries 信息                                          │
│    - 动态生成 save_path                                                   │
└─────────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────────┐
│ 2. fetch_and_parse_items()                                               │
│    - 发送 HTTP 请求（支持 ETag/Last-Modified 缓存）                         │
│    - 应用 pubDate 过滤（只处理新条目）                                       │
│    - 应用 include/exclude 过滤器                                          │
│    - 解析标题提取集数和元数据                                               │
│    - 按优先级预筛选（每集只保留最高优先级）                                    │
└─────────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────────┐
│ 3. build_torrent_lookup()                                                │
│    - 获取该 bangumi 所有现有 torrents                                      │
│    - 构建 hash 集合（快速去重）                                             │
│    - 从 qBittorrent 解析每个 torrent 的集数                                 │
│    - 构建 episode -> torrents 映射（用于洗版）                              │
└─────────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────────┐
│ 4. process_item() - 对每个 RSS 条目                                       │
└─────────────────────────────────────────────────────────────────────────┘
                                    │
                    ┌───────────────┴───────────────┐
                    ▼                               ▼
            hash 已存在?                      该集有现有 torrent?
                │                                   │
           ┌────┴────┐                    ┌────────┴────────┐
           ▼         ▼                    ▼                 ▼
          是        否                   是                否
           │         │                    │                 │
           ▼         │                    ▼                 ▼
         跳过        │            should_wash()?    create_and_add_task()
                     │                    │                 │
                     │           ┌────────┴────────┐        │
                     │           ▼                 ▼        │
                     │          是                否        │
                     │           │                 │        │
                     │           ▼                 ▼        │
                     │    wash_episode()         跳过       │
                     │    (删旧 + 加新)                      │
                     └───────────────────────────────────────┘
```

## 数据结构

### ProcessingContext

处理上下文，避免重复传参：

```rust
struct ProcessingContext {
    bangumi: BangumiWithSeries,  // 番剧信息（含 series）
    save_path: String,           // 动态生成的保存路径
}
```

### TorrentLookup

现有 torrent 的查找结构（包含元数据用于洗版判断）：

```rust
struct TorrentLookup {
    // O(1) 去重检查
    existing_hashes: HashSet<String>,

    // 按集数索引的 torrents 及其元数据（用于洗版判断）
    // Key: 集数（从 qBittorrent 文件名解析）
    // Value: (Torrent, ComparableTorrent) 元组
    episode_torrents: HashMap<i32, Vec<(Torrent, ComparableTorrent)>>,
}
```

## 过滤机制

### 1. HTTP 缓存过滤

使用 `ETag` 和 `Last-Modified` 头实现条件请求：

```rust
let fetch_context = FetchContext {
    etag: rss.etag.clone(),
    last_modified: rss.last_modified.clone(),
};

match rss_client.fetch_conditional(&source, Some(&fetch_context)).await {
    FetchResult::NotModified => return None,  // 304，无需处理
    FetchResult::Modified { items, etag, last_modified } => ...
}
```

### 2. pubDate 过滤

只处理比上次记录的 `last_pub_date` 更新的条目：

```rust
fn filter_by_pub_date(items: Vec<RssItem>, last_pub_date: Option<&str>) -> Vec<RssItem> {
    items.into_iter()
        .filter(|item| {
            match &item.pub_date {
                Some(pub_date) => pub_date.as_str() > last_pub_date,
                None => true,  // 保守处理：无日期的条目保留
            }
        })
        .collect()
}
```

### 3. Include/Exclude 过滤

支持正则表达式，大小写不敏感：

```rust
// 逻辑：
// 1. include 为空：全部通过
// 2. include 非空：必须匹配至少一个 include 规则
// 3. exclude：不能匹配任何 exclude 规则
// 最终：include_ok AND exclude_ok

fn filter_rss_items(
    items: Vec<RssItem>,
    include_patterns: &[String],  // OR 逻辑
    exclude_patterns: &[String],  // OR 逻辑（匹配任一则排除）
) -> Vec<RssItem>
```

### 4. 数量限制

每个 RSS 最多处理 200 个条目，防止处理过多数据：

```rust
const MAX_ITEMS_PER_RSS: usize = 200;

// 过滤后检查数量
if filtered_items.len() > MAX_ITEMS_PER_RSS {
    tracing::warn!("[{}] RSS has {} items, limiting to {}", ...);
    filtered_items.truncate(MAX_ITEMS_PER_RSS);
}
```

### 5. 优先级预筛选

同一集有多个来源时，只保留优先级最高的：

```rust
fn filter_by_priority(parsed_items: Vec<(RssItem, i32, ParseResult)>)
    -> Vec<(RssItem, i32, ParseResult)>
{
    // 按集数分组
    // 每组保留 priority score 最低（优先级最高）的条目
    // 优先级由 subtitle_group 和 subtitle_language 决定
}
```

## 洗版（Washing）机制

当新资源的优先级高于现有资源时，自动替换。

### 优先级判断

从 qBittorrent 获取实际文件名解析元数据：

```rust
// washing.rs
async fn parse_torrent_metadata(&self, torrent: &Torrent) -> ComparableTorrent {
    // 1. 从 qBittorrent 获取文件列表
    let files = self.downloader.get_task_files(&torrent.info_hash).await?;

    // 2. 找到第一个视频文件
    let video_file = files.iter().find(|f| f.is_video())?;

    // 3. 解析文件名获取字幕组和语言
    let result = self.parser.parse(&video_file.path)?;

    ComparableTorrent {
        subtitle_group: result.subtitle_group,
        subtitle_languages: result.subtitle_language,
    }
}
```

### 洗版执行

```rust
// 事务性操作：
// 1. 删除数据库中旧 torrent 记录
// 2. 创建新 torrent 记录
// 3. 从 qBittorrent 删除旧任务（含文件）
// 4. 添加新下载任务
// 5. 提交事务（任一步骤失败则回滚）

pub async fn wash_episode(&self, params: WashParams<'_>) -> Result<Vec<String>, WashingError>
```

### WashParams

```rust
pub struct WashParams<'a> {
    pub bangumi_id: i64,
    pub rss_id: Option<i64>,
    pub rss_title: &'a str,
    pub existing_torrents: &'a [Torrent],  // 要替换的旧 torrents
    pub info_hash: &'a str,
    pub torrent_url: &'a str,
    pub episode: i32,
    pub parse_result: &'a ParseResult,
    pub save_path: &'a str,
    pub rename: &'a str,
}
```

## 路径生成

保存路径动态生成，不存储在数据库：

```rust
// 格式：{base_path}/{series_title} ({year}) {tmdb-ID}/Season {season:02}/
let save_path = pathgen::generate_directory(
    base_path,                      // 从设置获取
    &bangumi.series.title_chinese,  // 系列中文标题
    bangumi.bangumi.year,           // 年份
    bangumi.bangumi.season,         // 季数
    bangumi.series.tmdb_id,         // TMDB ID（可选）
    Some(bangumi.bangumi.platform.as_str()),  // tv/movie/ova
)?;

// 文件名格式：{title} - s{season:02}e{episode:02}
let filename = pathgen::generate_filename(
    &bangumi.bangumi.title_chinese,
    bangumi.bangumi.season,
    adjusted_episode,  // 应用 episode_offset 后的集数
    Some(bangumi.bangumi.platform.as_str()),
);
```

## 集数偏移

RSS 中的集数可能是绝对集数（如第二季第1集显示为第13集），需要转换：

```rust
// bangumi.episode_offset = 12（第二季从第13集开始）
let adjusted_episode = ctx.bangumi.bangumi.adjust_episode(episode);
// episode=13 -> adjusted_episode=1
```

## 并发控制

批量处理时限制并发数：

```rust
const RSS_FETCH_CONCURRENCY: usize = 5;

pub async fn process_batch(&self, rss_list: Vec<Rss>, global_exclude_filters: &[String]) {
    stream::iter(rss_list)
        .map(|rss| async move {
            self.process_single(&rss, global_exclude_filters).await
        })
        .buffer_unordered(RSS_FETCH_CONCURRENCY)
        .collect::<Vec<_>>()
        .await;
}
```

## 后台处理

创建/更新 bangumi 后立即触发 RSS 处理：

```rust
pub fn spawn_background(&self, rss_ids: Vec<i64>) {
    tokio::spawn(async move {
        // 创建临时 service 实例
        // 逐个处理 rss_ids
    });
}
```

## 日志示例

```
DEBUG Processing RSS: [Lilith-Raws] 葬送的芙莉莲
DEBUG RSS not modified (HTTP 304), skipping
DEBUG E12: kept highest priority item (group=Some("Lilith-Raws"), lang=["CHT"]) from 3 candidates
DEBUG Skipping existing torrent: [Lilith-Raws] 葬送的芙莉莲 - 11
DEBUG [Lilith-Raws] E13: skipping lower/equal priority torrent (group=Some("SubGroup-B"), lang=["CHS"])
INFO  [Lilith-Raws] Washing E14: replacing 1 existing torrent(s) with higher priority resource
INFO  [Lilith-Raws] Deleted old torrent from downloader: abc123...
INFO  [Lilith-Raws] Washed E14: replaced 1 torrent(s)
DEBUG [Lilith-Raws] Queued download for E15: def456...
```
