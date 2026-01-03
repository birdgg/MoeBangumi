# Metadata 架构设计文档

本文档面向熟悉 Rust 但不熟悉本项目的贡献者，介绍 MoeBangumi 的 Metadata 系统架构设计。

## 目录

1. [概述](#概述)
2. [数据模型](#数据模型)
3. [服务层架构](#服务层架构)
4. [数据流](#数据流)
5. [自动完结检测](#自动完结检测)
6. [扩展指南](#扩展指南)

---

## 概述

Metadata 是 MoeBangumi 的**元数据中心**，作为统一缓存层存储从多个外部服务获取的番剧信息。

### 设计目标

1. **数据聚合** - 整合 BGM.tv、TMDB、Mikan 等多源数据
2. **避免重复请求** - 缓存外部 API 响应，减少网络请求
3. **数据一致性** - 提供单一数据源，避免同一番剧在系统中有多份不同的元数据

### 核心概念

```
┌─────────────────────────────────────────────────────────┐
│                    External Services                     │
│  ┌─────────┐    ┌─────────┐    ┌─────────┐              │
│  │ BGM.tv  │    │  TMDB   │    │  Mikan  │              │
│  └────┬────┘    └────┬────┘    └────┬────┘              │
└───────┼──────────────┼──────────────┼───────────────────┘
        │              │              │
        └──────────────┼──────────────┘
                       ▼
              ┌────────────────┐
              │    Metadata    │  ← 统一缓存层
              │   (元数据表)    │
              └────────┬───────┘
                       │
                       ▼
              ┌────────────────┐
              │    Bangumi     │  ← 用户订阅
              │   (订阅表)      │
              └────────────────┘
```

### Metadata 与 Bangumi 的关系

- **Metadata**: 番剧的客观信息（标题、年份、集数、海报等）
- **Bangumi**: 用户的订阅配置（保存路径、集数偏移、RSS 源等）
- 关系: Bangumi → Metadata (多对一，但实际使用中通常是一对一)

---

## 数据模型

### 数据库表结构

```sql
CREATE TABLE metadata (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    created_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,

    -- 外部服务 ID（三选一或多个）
    mikan_id TEXT,                    -- Mikan 番剧 ID
    bgmtv_id INTEGER,                 -- BGM.tv subject ID
    tmdb_id INTEGER,                  -- TMDB ID

    -- 标题（双语支持）
    title_chinese TEXT NOT NULL,      -- 中文标题（主显示）
    title_japanese TEXT,              -- 日文原名

    -- 基本信息
    season INTEGER NOT NULL DEFAULT 1,      -- 季数
    year INTEGER NOT NULL,                  -- 年份
    platform TEXT NOT NULL DEFAULT 'tv',    -- 平台: tv/movie/ova

    -- 元数据
    total_episodes INTEGER NOT NULL DEFAULT 0,  -- 总集数（0=未知）
    poster_url TEXT,                            -- 海报 URL
    air_date DATE,                              -- 首播日期
    air_week INTEGER NOT NULL                   -- 播出星期（0=周日 ~ 6=周六）
);
```

### 设计决策

#### 1. 外部 ID 作为唯一标识

```sql
-- 每个外部 ID 单独建立唯一索引（仅非空值）
CREATE UNIQUE INDEX idx_metadata_mikan_id ON metadata(mikan_id)
    WHERE mikan_id IS NOT NULL;
CREATE UNIQUE INDEX idx_metadata_bgmtv_id ON metadata(bgmtv_id)
    WHERE bgmtv_id IS NOT NULL AND bgmtv_id != 0;
CREATE UNIQUE INDEX idx_metadata_tmdb_id ON metadata(tmdb_id)
    WHERE tmdb_id IS NOT NULL AND tmdb_id != 0;
```

**为什么这样设计？**
- 同一番剧可能从不同来源创建（用户可能先从 Mikan 添加，后从 BGM.tv 搜索）
- 通过外部 ID 去重，避免重复创建
- 使用部分索引 `WHERE ... IS NOT NULL` 允许多条记录的外部 ID 为 NULL

#### 2. 双语标题设计

```rust
pub struct Metadata {
    pub title_chinese: String,        // 必填，作为主显示
    pub title_japanese: Option<String>, // 可选
}
```

**为什么？**
- `title_chinese` 为必填项，确保系统始终有可显示的标题
- 前端 UI 以中文标题为主，日文标题作为补充信息

### Rust 数据模型

```rust
// crates/server/src/models/metadata.rs

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Metadata {
    pub id: i64,
    pub created_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,

    // 外部服务 ID
    pub mikan_id: Option<String>,
    pub bgmtv_id: Option<i64>,
    pub tmdb_id: Option<i64>,

    // 标题
    pub title_chinese: String,
    pub title_japanese: Option<String>,

    // 基本信息
    pub season: i32,
    pub year: i32,
    pub platform: Platform,  // enum: Tv, Movie, Ova

    // 元数据
    pub total_episodes: i32,
    pub poster_url: Option<String>,
    pub air_date: Option<String>,
    pub air_week: i32,
}
```

### 三态更新模式 (Clearable)

更新 Metadata 时，某些字段需要区分三种状态：

1. **不变** - 保留原值
2. **设置新值** - 更新为新值
3. **清空** - 设置为 NULL

为此设计了 `Clearable<T>` 枚举：

```rust
// crates/server/src/models/clearable.rs

#[derive(Debug, Clone, Default)]
pub enum Clearable<T> {
    #[default]
    Unchanged,     // 不修改
    Set(T),        // 设置新值
    Clear,         // 清空为 NULL
}

impl<T> Clearable<T> {
    /// 解析为最终值：Unchanged 返回原值，Set 返回新值，Clear 返回 None
    pub fn resolve(self, existing: Option<T>) -> Option<T> {
        match self {
            Clearable::Unchanged => existing,
            Clearable::Set(value) => Some(value),
            Clearable::Clear => None,
        }
    }
}
```

**使用示例：**

```rust
pub struct UpdateMetadata {
    pub mikan_id: Clearable<String>,      // 可清空
    pub title_chinese: Option<String>,     // 仅可更新或不变
    // ...
}
```

---

## 服务层架构

### 分层架构

```
┌─────────────────────────────────────────────────────────────┐
│                      API Layer (handlers)                    │
│                    crates/server/src/api/                    │
└──────────────────────────┬──────────────────────────────────┘
                           │
┌──────────────────────────▼──────────────────────────────────┐
│                     Service Layer                            │
│                 crates/server/src/services/                  │
│  ┌──────────────────┐  ┌──────────────────┐                 │
│  │ MetadataService  │  │  BangumiService  │                 │
│  └────────┬─────────┘  └────────┬─────────┘                 │
│           │                     │                            │
│  ┌────────▼─────────────────────▼─────────┐                 │
│  │           External API Clients          │                 │
│  │  BgmtvClient | TmdbClient | MikanClient │                 │
│  └─────────────────────────────────────────┘                 │
└──────────────────────────┬──────────────────────────────────┘
                           │
┌──────────────────────────▼──────────────────────────────────┐
│                    Repository Layer                          │
│               crates/server/src/repositories/                │
│  ┌────────────────────┐  ┌────────────────────┐             │
│  │ MetadataRepository │  │ BangumiRepository  │             │
│  └────────────────────┘  └────────────────────┘             │
└──────────────────────────┬──────────────────────────────────┘
                           │
┌──────────────────────────▼──────────────────────────────────┐
│                     Database (SQLite)                        │
└─────────────────────────────────────────────────────────────┘
```

### MetadataService

`MetadataService` 是 Metadata 系统的核心服务，负责：

1. **CRUD 操作** - 创建、读取、更新、删除 metadata
2. **外部 API 集成** - 从 BGM.tv/TMDB 获取数据
3. **去重逻辑** - 通过外部 ID 查找已存在的 metadata
4. **完结状态检查** - 通过 BGM.tv Episodes API 判断番剧是否完结

```rust
// crates/server/src/services/metadata.rs

pub struct MetadataService {
    db: SqlitePool,
    bgmtv: Arc<BgmtvClient>,
    tmdb: Arc<TmdbClient>,
}

impl MetadataService {
    // 基础 CRUD
    pub async fn create(&self, data: CreateMetadata) -> Result<Metadata, MetadataError>;
    pub async fn get_by_id(&self, id: i64) -> Result<Metadata, MetadataError>;
    pub async fn update(&self, id: i64, data: UpdateMetadata) -> Result<Metadata, MetadataError>;
    pub async fn delete(&self, id: i64) -> Result<bool, MetadataError>;

    // 外部 API 集成
    pub async fn fetch_from_bgmtv(&self, id: i64) -> Result<FetchedMetadata, MetadataError>;
    pub async fn find_tmdb_id(&self, title: &str) -> Result<Option<i64>, MetadataError>;

    // 去重逻辑
    pub async fn get_by_external_id(...) -> Result<Option<Metadata>, MetadataError>;
    pub async fn find_or_create(&self, data: CreateMetadata) -> Result<Metadata, MetadataError>;
    pub async fn find_or_update(&self, data: CreateMetadata) -> Result<Metadata, MetadataError>;
}
```

### MetadataRepository

Repository 层负责数据库操作，不包含业务逻辑：

```rust
// crates/server/src/repositories/metadata.rs

pub struct MetadataRepository;

impl MetadataRepository {
    // 基础 CRUD
    pub async fn create(pool: &SqlitePool, data: CreateMetadata) -> Result<Metadata, sqlx::Error>;
    pub async fn get_by_id(pool: &SqlitePool, id: i64) -> Result<Option<Metadata>, sqlx::Error>;
    pub async fn update(pool: &SqlitePool, id: i64, data: UpdateMetadata) -> Result<Option<Metadata>, sqlx::Error>;
    pub async fn delete(pool: &SqlitePool, id: i64) -> Result<bool, sqlx::Error>;

    // 按外部 ID 查询
    pub async fn get_by_mikan_id(pool: &SqlitePool, mikan_id: &str) -> Result<Option<Metadata>, sqlx::Error>;
    pub async fn get_by_bgmtv_id(pool: &SqlitePool, bgmtv_id: i64) -> Result<Option<Metadata>, sqlx::Error>;
    pub async fn get_by_tmdb_id(pool: &SqlitePool, tmdb_id: i64) -> Result<Option<Metadata>, sqlx::Error>;
}
```

### 依赖注入

服务通过 `AppState` 注入依赖：

```rust
// crates/server/src/state.rs

pub struct AppState {
    pub metadata: Arc<MetadataService>,
    pub bangumi: Arc<BangumiService>,
    // ...
}

impl AppState {
    pub async fn new(config: &Config) -> Result<Self, Box<dyn Error>> {
        let db = create_pool(&config.database_url).await?;
        let bgmtv = Arc::new(BgmtvClient::new());
        let tmdb = Arc::new(TmdbClient::new(&config.tmdb_api_key));

        let metadata = Arc::new(MetadataService::new(db.clone(), bgmtv, tmdb));
        // ...
    }
}
```

---

## 数据流

### 创建 Bangumi 时的 Metadata 流程

当用户通过前端添加新番剧订阅时，系统执行以下流程：

```
用户提交 → BangumiService.create() → MetadataService.find_or_update()
                                              │
                    ┌─────────────────────────┼─────────────────────────┐
                    │                         │                         │
                    ▼                         ▼                         ▼
             检查 mikan_id              检查 bgmtv_id              检查 tmdb_id
                    │                         │                         │
                    └─────────────────────────┼─────────────────────────┘
                                              │
                              ┌───────────────┴───────────────┐
                              │                               │
                              ▼                               ▼
                         找到已存在                        未找到
                              │                               │
                              ▼                               ▼
                       更新现有 Metadata                创建新 Metadata
                              │                               │
                              └───────────────┬───────────────┘
                                              │
                                              ▼
                                    检查完结状态 (BGM.tv)
                                              │
                                              ▼
                                    返回 Metadata 对象
```

### 外部 ID 查找优先级

```rust
// MetadataService::get_by_external_id()

// 1. 优先使用 mikan_id（最常见的添加入口）
if let Some(mikan_id) = mikan_id {
    if let Some(metadata) = repo.get_by_mikan_id(mikan_id).await? {
        return Ok(Some(metadata));
    }
}

// 2. 其次使用 bgmtv_id（权威数据源）
if let Some(bgmtv_id) = bgmtv_id {
    if let Some(metadata) = repo.get_by_bgmtv_id(bgmtv_id).await? {
        return Ok(Some(metadata));
    }
}

// 3. 最后使用 tmdb_id
if let Some(tmdb_id) = tmdb_id {
    if let Some(metadata) = repo.get_by_tmdb_id(tmdb_id).await? {
        return Ok(Some(metadata));
    }
}
```

### find_or_create vs find_or_update

系统提供两种去重策略：

| 方法 | 找到已存在时 | 未找到时 | 使用场景 |
|------|-------------|---------|---------|
| `find_or_create` | 返回原值 | 创建新记录 | 批量导入，保留原有数据 |
| `find_or_update` | 合并更新 | 创建新记录 | 用户手动添加，使用最新数据 |

**合并更新逻辑：**

```rust
// find_or_update() 内部实现
let update_data = create_data.into_update();  // CreateMetadata → UpdateMetadata
return self.update(existing.id, update_data).await;
```

`into_update()` 将 `CreateMetadata` 转换为 `UpdateMetadata`，其中：
- `Some(value)` → 更新为新值
- `None` → 保持原值（不会清空）

### 从 BGM.tv 获取数据流程

```rust
// MetadataService::fetch_from_bgmtv()

let detail = self.bgmtv.get_subject(id).await?;  // 调用 BGM.tv API

// 解析标题，提取季数
let parsed_cn = parse_bgmtv_name(&detail.name_cn);  // "某番剧 第二季" → { name: "某番剧", season: 2 }
let parsed_jp = parse_bgmtv_name(&detail.name);

Ok(FetchedMetadata {
    bgmtv_id: detail.id,
    title_chinese: parsed_cn.name,
    title_japanese: parsed_jp.name,
    season: parsed_cn.season,
    year: parse_year(&detail.date),
    total_episodes: detail.total_episodes,
    poster_url: detail.images.large,
    air_date: detail.date,
    platform: parse_platform(&detail.platform),
})
```

---

## 扩展指南

### 添加新的外部数据源

假设要添加 AniList 作为新的外部数据源：

#### 1. 修改数据库 Schema

```sql
-- migrations/001_init.sql

ALTER TABLE metadata ADD COLUMN anilist_id INTEGER;

CREATE UNIQUE INDEX IF NOT EXISTS idx_metadata_anilist_id
    ON metadata(anilist_id)
    WHERE anilist_id IS NOT NULL AND anilist_id != 0;
```

#### 2. 更新数据模型

```rust
// crates/server/src/models/metadata.rs

pub struct Metadata {
    // ... 现有字段
    pub anilist_id: Option<i64>,  // 新增
}

pub struct CreateMetadata {
    // ... 现有字段
    pub anilist_id: Option<i64>,  // 新增
}

pub struct UpdateMetadata {
    // ... 现有字段
    pub anilist_id: Clearable<i64>,  // 新增，支持清空
}
```

#### 3. 添加 Repository 方法

```rust
// crates/server/src/repositories/metadata.rs

impl MetadataRepository {
    pub async fn get_by_anilist_id(
        pool: &SqlitePool,
        anilist_id: i64,
    ) -> Result<Option<Metadata>, sqlx::Error> {
        let query = format!("{} WHERE anilist_id = $1", SELECT_METADATA);
        let row = sqlx::query_as::<_, MetadataRow>(&query)
            .bind(anilist_id)
            .fetch_optional(pool)
            .await?;
        Ok(row.map(Into::into))
    }
}
```

#### 4. 更新 Service 查找逻辑

```rust
// crates/server/src/services/metadata.rs

impl MetadataService {
    pub async fn get_by_external_id(
        &self,
        mikan_id: Option<&str>,
        bgmtv_id: Option<i64>,
        tmdb_id: Option<i64>,
        anilist_id: Option<i64>,  // 新增参数
    ) -> Result<Option<Metadata>, MetadataError> {
        // ... 现有逻辑

        // 添加 AniList 查找
        if let Some(anilist_id) = anilist_id {
            if let Some(metadata) = MetadataRepository::get_by_anilist_id(&self.db, anilist_id).await? {
                return Ok(Some(metadata));
            }
        }

        Ok(None)
    }
}
```

### 添加新的定时任务

参考 `MetadataFinishCheckJob` 的实现模式：

#### 1. 创建 Job 文件

```rust
// crates/server/src/services/scheduler/my_new_job.rs

use std::sync::Arc;
use std::time::Duration;
use async_trait::async_trait;
use crate::services::{JobResult, SchedulerJob, MyService};

pub struct MyNewJob {
    my_service: Arc<MyService>,
}

impl MyNewJob {
    pub fn new(my_service: Arc<MyService>) -> Self {
        Self { my_service }
    }
}

#[async_trait]
impl SchedulerJob for MyNewJob {
    fn name(&self) -> &'static str {
        "MyNewJob"
    }

    fn interval(&self) -> Duration {
        Duration::from_secs(60 * 60)  // 每小时
    }

    async fn execute(&self) -> JobResult {
        // 实现任务逻辑
        self.my_service.do_something().await?;
        Ok(())
    }
}
```

#### 2. 注册模块

```rust
// crates/server/src/services/scheduler.rs

mod my_new_job;
pub use my_new_job::MyNewJob;

// crates/server/src/services.rs
pub use scheduler::{JobResult, MyNewJob, SchedulerJob, SchedulerService};
```

#### 3. 注册到 Scheduler

```rust
// crates/server/src/state.rs

let scheduler = SchedulerService::new()
    .with_job(MyNewJob::new(Arc::clone(&my_service)))  // 添加新任务
    .with_job(/* 其他任务 */);
scheduler.start();
```

### 添加新的 Metadata 字段

#### 1. 评估字段类型

| 字段特性 | 推荐类型 |
|---------|---------|
| 必填，不可为空 | `String` / `i32` |
| 可选，允许为空 | `Option<T>` |
| 更新时可清空 | `Clearable<T>` |

#### 2. 更新 SQL 常量

```rust
// crates/server/src/repositories/metadata.rs

const SELECT_METADATA: &str = r#"
    SELECT
        id, created_at, updated_at,
        // ... 现有字段
        new_field  // 新增字段
    FROM metadata
"#;
```

#### 3. 更新 Row 映射

```rust
#[derive(sqlx::FromRow)]
struct MetadataRow {
    // ... 现有字段
    new_field: Option<String>,  // 新增
}

impl From<MetadataRow> for Metadata {
    fn from(row: MetadataRow) -> Self {
        Self {
            // ... 现有字段
            new_field: row.new_field,
        }
    }
}
```

### 测试建议

1. **单元测试** - 测试 `Clearable::resolve()` 的三种情况
2. **集成测试** - 测试 `find_or_create` 和 `find_or_update` 的去重逻辑
3. **定时任务测试** - 使用较短的 interval 验证任务执行

### 文件位置参考

```
crates/server/src/
├── models/
│   ├── metadata.rs         # Metadata 数据模型
│   └── clearable.rs        # Clearable<T> 枚举
├── repositories/
│   └── metadata.rs         # MetadataRepository
├── services/
│   ├── metadata.rs         # MetadataService
│   └── scheduler/
│       ├── mod.rs          # 调度器模块
│       └── metadata_finish_check_job.rs  # 完结检查任务
└── state.rs                # AppState 和服务注册
```
