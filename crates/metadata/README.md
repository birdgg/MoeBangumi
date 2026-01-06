# Metadata Provider 技术规范

## 1. 概述

`metadata` crate 提供统一的元数据搜索抽象层，将不同数据源（BGM.tv、TMDB）的 API 封装为标准化接口。

### 设计目标

- **统一接口**：不同数据源使用相同的 trait 方法
- **标准化输出**：所有搜索结果使用 `SearchedMetadata` 统一格式
- **类型安全**：通过 `MetadataSource` 枚举区分数据来源
- **易于扩展**：添加新数据源只需实现 `MetadataProvider` trait

### 与其他模块的关系

```
┌─────────────────────────────────────────────────────────────┐
│                       server crate                          │
│  ┌─────────────────────────────────────────────────────┐   │
│  │              API Handlers (search.rs)                │   │
│  │   /api/search/metadata, /api/search/metadata/find   │   │
│  └──────────────────────────┬──────────────────────────┘   │
│                             │                               │
│  ┌──────────────────────────▼──────────────────────────┐   │
│  │                    AppState                          │   │
│  │   bgmtv_provider: Arc<BgmtvProvider>                │   │
│  │   tmdb_provider: Arc<TmdbProvider>                  │   │
│  └─────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                     metadata crate                          │
│  ┌─────────────────────────────────────────────────────┐   │
│  │              MetadataProvider trait                  │   │
│  │   search(&SearchQuery) -> Vec<SearchedMetadata>      │   │
│  │   find(&SearchQuery) -> Option<SearchedMetadata>     │   │
│  └─────────────────────────────────────────────────────┘   │
│                    △                △                       │
│        ┌───────────┘                └───────────┐           │
│  ┌─────┴─────────┐                    ┌─────────┴─────┐    │
│  │ BgmtvProvider │                    │ TmdbProvider  │    │
│  └───────────────┘                    └───────────────┘    │
└─────────────────────────────────────────────────────────────┘
                    │                           │
                    ▼                           ▼
           ┌──────────────┐             ┌─────────────┐
           │ bgmtv crate  │             │ tmdb crate  │
           │ BgmtvClient  │             │ TmdbClient  │
           └──────────────┘             └─────────────┘
```

## 2. 核心类型

### 2.1 MetadataProvider Trait

```rust
#[async_trait]
pub trait MetadataProvider: Send + Sync {
    /// 搜索元数据，返回多个结果（按相关度排序）
    async fn search(&self, query: &SearchQuery) -> Result<Vec<SearchedMetadata>, ProviderError>;

    /// 查找最佳匹配的元数据
    /// 当 query.year 有值时，优先返回年份匹配（±1年容差）的结果
    async fn find(&self, query: &SearchQuery) -> Result<Option<SearchedMetadata>, ProviderError>;

    /// 获取剧集偏移量
    /// 用于将 RSS 绝对集数转换为季度相对集数
    async fn get_episode_offset(&self, external_id: &str) -> Result<i32, ProviderError>;

    /// Provider 名称（用于日志和调试）
    fn name(&self) -> &'static str;
}
```

**关键设计**：
- `find()` 方法有默认实现，基于 `search()` 结果进行年份过滤
- 年份容差为 ±1 年，处理跨年播出的动画
- `get_episode_offset()` 默认返回 0，BgmtvProvider 重写此方法

### 2.2 SearchQuery

```rust
pub struct SearchQuery {
    /// 搜索关键词（必填）
    pub keyword: String,
    /// 年份过滤（可选）
    pub year: Option<i32>,
}

impl SearchQuery {
    pub fn new(keyword: impl Into<String>) -> Self;
    pub fn with_year(mut self, year: i32) -> Self;
}
```

**使用示例**：
```rust
// 仅关键词搜索
let query = SearchQuery::new("葬送のフリーレン");

// 带年份过滤
let query = SearchQuery::new("葬送のフリーレン").with_year(2023);
```

### 2.3 SearchedMetadata

统一的搜索结果格式：

```rust
pub struct SearchedMetadata {
    /// 数据来源
    pub source: MetadataSource,
    /// 外部 ID（字符串形式，统一 i64 和 String）
    pub external_id: String,

    /// 中文标题
    pub title_chinese: Option<String>,
    /// 日文/原始标题
    pub title_original: Option<String>,

    /// 年份
    pub year: Option<i32>,
    /// 季度（仅 BGM.tv 提供）
    pub season: Option<i32>,
    /// 平台类型
    pub platform: Option<Platform>,

    /// 总集数（0 = 未知）
    pub total_episodes: i32,
    /// 海报 URL
    pub poster_url: Option<String>,
    /// 首播日期（YYYY-MM-DD 格式）
    pub air_date: Option<String>,
}
```

**字段来源对比**：

| 字段 | BGM.tv | TMDB |
|------|--------|------|
| source | Bgmtv | Tmdb |
| external_id | bgmtv_id | id |
| title_chinese | title_chinese | name |
| title_original | title_japanese | original_name |
| year | year | 从 first_air_date 解析 |
| season | season | ❌ 不提供 |
| platform | platform | ❌ 不提供 |
| total_episodes | total_episodes | ❌ 搜索结果不提供 |
| poster_url | poster_url | poster_path + 基础URL |
| air_date | air_date | first_air_date |

### 2.4 MetadataSource

```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum MetadataSource {
    Bgmtv,
    Tmdb,
}
```

### 2.5 Platform

```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Platform {
    #[default]
    Tv,
    Movie,
    Ova,
}
```

### 2.6 ProviderError

```rust
#[derive(Debug, thiserror::Error)]
pub enum ProviderError {
    #[error("Data source {0:?} is not available")]
    SourceNotAvailable(MetadataSource),

    #[error("BGM.tv error: {0}")]
    Bgmtv(#[from] bgmtv::BgmtvError),

    #[error("TMDB error: {0}")]
    Tmdb(#[from] tmdb::TmdbError),
}
```

## 3. Provider 实现

### 3.1 BgmtvProvider

```rust
use metadata::{BgmtvProvider, MetadataProvider, SearchQuery};
use std::sync::Arc;

let provider = BgmtvProvider::new(Arc::clone(&bgmtv_client));

// 搜索
let query = SearchQuery::new("葬送のフリーレン");
let results = provider.search(&query).await?;

// 查找最佳匹配
let query = SearchQuery::new("葬送のフリーレン").with_year(2023);
let best_match = provider.find(&query).await?;

// 获取剧集偏移量（通过 trait 方法）
let offset = provider.get_episode_offset("425651").await?;
```

**特点**：
- 直接调用 `BgmtvClient::search_bangumi()`
- 通过 `From<ParsedSubject>` trait 转换结果
- 实现 `get_episode_offset()` 从 BGM.tv 获取剧集偏移量

#### Episode Offset 计算

`get_episode_offset(external_id)` 用于计算剧集偏移量，将 RSS 中的绝对集数转换为季度相对集数。

**使用场景**：
- 第二季从第 13 集开始（绝对编号），但应显示为第 1 集（季度相对）
- RSS 提供绝对集数，需要转换为 Plex/Jellyfin 的 SxxExx 格式

**计算公式**：
```
offset = sort - ep

其中：
- sort: BGM.tv 中的绝对集数（如第二季第一集的 sort = 13）
- ep: 季度相对集数（如第二季第一集的 ep = 1）
- offset = 13 - 1 = 12
```

**应用**：
```rust
// RSS 提供的集数
let rss_episode = 15;

// 获取偏移量（external_id 为字符串）
let offset = provider.get_episode_offset("425651").await?; // 返回 12

// 转换为季度相对集数
let season_episode = rss_episode - offset; // 15 - 12 = 3 (S02E03)
```

**注意**：TmdbProvider 的 `get_episode_offset()` 返回默认值 0（TMDB 不支持此功能）。

### 3.2 TmdbProvider

```rust
use metadata::{TmdbProvider, MetadataProvider, SearchQuery};
use std::sync::Arc;

let provider = TmdbProvider::new(Arc::clone(&tmdb_client));

let query = SearchQuery::new("SPY×FAMILY 第2クール");
let results = provider.search(&query).await?;
```

**特点**：
- 搜索前自动清理标题（移除季度/分割放送标记）
- 海报 URL 自动拼接 TMDB 基础 URL

**标题清理规则**：

| 原标题 | 清理后 |
|--------|--------|
| `SPY×FAMILY 第2クール` | `SPY×FAMILY` |
| `葬送のフリーレン 第2部分` | `葬送のフリーレン` |
| `进击的巨人 第四季 Part 2` | `进击的巨人` |
| `BLEACH 千年血戦篇 S2` | `BLEACH 千年血戦篇` |

## 4. 扩展指南

### 添加新的 Provider

需要修改 5 个文件：

| 步骤 | 文件 | 修改内容 |
|------|------|----------|
| 1 | `adapters/new_adapter.rs` | 创建新文件，实现 Provider |
| 2 | `adapters.rs` | 添加 mod 和 pub use |
| 3 | `models.rs` | 在 `MetadataSource` 枚举添加新变体 |
| 4 | `error.rs` | 在 `ProviderError` 添加新错误类型 |
| 5 | `lib.rs` | 导出新 Provider |

#### Step 1: 在 `crates/metadata/src/adapters/` 创建新文件

```rust
// new_adapter.rs
use std::sync::Arc;
use async_trait::async_trait;
use crate::{MetadataProvider, MetadataSource, ProviderError, SearchQuery, SearchedMetadata};

pub struct NewProvider {
    client: Arc<NewClient>,
}

impl NewProvider {
    pub fn new(client: Arc<NewClient>) -> Self {
        Self { client }
    }
}

#[async_trait]
impl MetadataProvider for NewProvider {
    async fn search(&self, query: &SearchQuery) -> Result<Vec<SearchedMetadata>, ProviderError> {
        // 调用底层客户端
        let results = self.client.search(&query.keyword).await?;

        // 转换为 SearchedMetadata
        Ok(results.into_iter().map(|r| SearchedMetadata {
            source: MetadataSource::New,
            external_id: r.id.to_string(),
            // ... 其他字段映射
        }).collect())
    }

    fn name(&self) -> &'static str {
        "new"
    }
}
```

2. 更新 `adapters.rs`：

```rust
mod new_adapter;
pub use new_adapter::NewProvider;
```

3. 更新 `MetadataSource` 枚举：

```rust
pub enum MetadataSource {
    Bgmtv,
    Tmdb,
    New, // 新增
}
```

4. 更新 `ProviderError`：

```rust
pub enum ProviderError {
    // ...
    #[error("New provider error: {0}")]
    New(#[from] new_crate::NewError),
}
```

5. 在 `server/src/infra/state.rs` 中初始化并添加到 AppState。

## 5. 依赖关系

```toml
[dependencies]
async-trait = "..."
regex = "..."        # TmdbProvider 标题清理
serde = "..."
thiserror = "..."
tracing = "..."

# 数据源客户端
bgmtv = { path = "../bgmtv" }
tmdb = { path = "../tmdb" }
```

## 6. 设计决策

### 为什么不包含 Mikan？

Mikan 用于 RSS 订阅和资源发现，不是元数据源。它的搜索返回下载资源（种子），而非番剧元数据。

### 为什么 external_id 是 String？

不同数据源的 ID 类型不同：
- BGM.tv: i64
- TMDB: i64
- 其他可能: String

统一使用 String 避免类型转换问题。

### 为什么 find() 有默认实现？

大多数 provider 的 `find()` 逻辑相同：搜索 → 年份过滤 → 返回第一个。默认实现减少重复代码，同时允许 provider 覆盖以实现特殊逻辑。

### 为什么年份容差是 ±1 年？

动画经常跨年播出（如 2023年10月 开播到 2024年3月）。±1 年容差确保：
- 2023年秋季番在搜索 2024 年时也能匹配
- 避免因播出时间跨年导致的匹配失败

### 为什么 Provider 需要 Arc？

`MetadataProvider` trait 要求 `Send + Sync`，因为：
- Provider 在多个 async 任务间共享（通过 AppState）
- Axum handler 可能并发处理多个请求
- `Arc<Client>` 允许多个 Provider 共享底层 HTTP 客户端

### SearchedMetadata 与数据库 Metadata 的关系

| 类型 | 用途 | id 字段 |
|------|------|---------|
| `SearchedMetadata` | 搜索结果（来自外部 API） | `external_id: String` |
| `Metadata` (server crate) | 数据库实体 | `id: i64` (自增主键) |

`SearchedMetadata` 是临时数据结构，用于 API 响应。持久化时需要通过 `MetadataService` 创建 `Metadata` 记录。

### 缓存策略

`metadata` crate 本身不实现缓存。缓存在上层处理：
- HTTP 响应缓存由 server 的 `CacheService` 管理
- 底层 HTTP 客户端可配置连接池复用
