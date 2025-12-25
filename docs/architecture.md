# 服务层架构文档

本文档描述 `moe-rs` 项目的分层架构设计。

## 架构概览

```
┌─────────────────────────────────────────────────────────────┐
│                      API Handlers                           │
│  (HTTP 协议转换、请求验证、响应序列化)                         │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                      Service Layer                          │
│  (业务逻辑、跨实体协调、缓存策略、外部 API 调用)                │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                    Repository Layer                         │
│  (数据访问、SQL 查询、数据映射)                               │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                       Database                              │
│  (SQLite)                                                   │
└─────────────────────────────────────────────────────────────┘
```

## 核心原则

1. **Handler 只负责协议转换** - 不包含业务逻辑
2. **Service 封装业务逻辑** - 协调多个 Repository 和外部服务
3. **Repository 只负责数据访问** - 纯 SQL 操作，无业务逻辑
4. **依赖注入** - 通过 `AppState` 共享服务实例

## 服务层详解

### AppState 结构

```rust
pub struct AppState {
    // 数据库连接池
    pub db: SqlitePool,

    // 配置
    pub config: Arc<Config>,

    // HTTP 客户端
    pub http_client: Client,

    // 外部 API 客户端
    pub tmdb: Arc<TmdbClient>,
    pub bgmtv: Arc<BgmtvClient>,
    pub mikan: Arc<MikanClient>,
    pub rss: Arc<RssClient>,

    // 业务服务
    pub settings: Arc<SettingsService>,
    pub downloader: Arc<DownloaderService>,
    pub poster: Arc<PosterService>,
    pub logs: Arc<LogService>,
    pub bangumi: Arc<BangumiService>,
    pub cache: Arc<CacheService>,

    // 调度器
    pub scheduler: Arc<SchedulerService>,
    pub rss_fetch_job: Arc<RssFetchJob>,
}
```

### 服务职责

| 服务 | 文件 | 职责 |
|------|------|------|
| `SettingsService` | `services/settings.rs` | 配置管理、TOML 持久化、变更订阅 |
| `DownloaderService` | `services/downloader.rs` | 下载器客户端管理、懒加载、自动重试 |
| `LogService` | `services/log.rs` | 日志记录、内存缓冲、SSE 广播 |
| `PosterService` | `services/poster.rs` | 海报下载、本地缓存、路径安全 |
| `BangumiService` | `services/bangumi.rs` | Bangumi CRUD、RSS 同步、海报集成 |
| `CacheService` | `services/cache.rs` | 通用缓存、get_or_fetch 模式 |
| `SchedulerService` | `services/scheduler.rs` | 周期任务调度 |

## Handler 与 Service 映射

| Handler | Service | 说明 |
|---------|---------|------|
| `bangumi.rs` | `BangumiService` | Bangumi 增删改查 |
| `logs.rs` | `LogService` | 日志查询、SSE 流 |
| `settings.rs` | `SettingsService` | 配置获取、更新 |
| `mikan.rs` | `CacheService` | Mikan 详情（带缓存） |
| `search.rs` | `CacheService` + 外部客户端 | 搜索（Mikan 带缓存） |
| `scheduler.rs` | `RssFetchJob` | 手动触发 RSS 抓取 |
| `downloader.rs` | 临时客户端 | 测试下载器连接 |
| `episodes.rs` | `BgmtvClient` | 获取剧集信息 |

## 服务设计模式

### 1. Cache-Aside 模式 (CacheService)

```rust
// 自动处理缓存命中/未命中
let data = state.cache
    .get_or_fetch(&cache_key, TTL, || async {
        external_api.fetch().await
    })
    .await?;
```

### 2. 观察者模式 (SettingsService, LogService)

```rust
// 订阅配置变更
let rx = state.settings.subscribe();

// 订阅日志广播 (SSE)
let rx = state.logs.subscribe();
```

### 3. 懒加载 + 双重检查锁定 (DownloaderService)

```rust
// 仅在首次使用时创建客户端
// 配置变更时自动重建
let client = self.ensure_client().await?;
```

## 错误处理

所有服务错误通过 `AppError` 统一处理：

```rust
pub enum AppError {
    NotFound(String),
    BadRequest(String),
    Database(sqlx::Error),
    Settings(SettingsError),
    Downloader(DownloaderError),
    Log(LogError),
    Poster(PosterError),
    Bangumi(BangumiError),
    ExternalApi(String),
    Internal(String),
}
```

错误自动映射到 HTTP 状态码：

| 错误类型 | HTTP 状态码 |
|---------|------------|
| `NotFound` | 404 |
| `BadRequest` | 400 |
| `Bangumi::NotFound` | 404 |
| `Downloader::NotConfigured` | 400 |
| `Downloader::Auth` | 401 |
| `ExternalApi` | 502 |
| 其他 | 500 |

## 文件结构

```
crates/server/src/
├── api/
│   ├── handlers/           # API 处理器
│   │   ├── bangumi.rs
│   │   ├── logs.rs
│   │   ├── settings.rs
│   │   ├── mikan.rs
│   │   ├── search.rs
│   │   ├── scheduler.rs
│   │   ├── downloader.rs
│   │   └── episodes.rs
│   └── handlers.rs         # 模块导出
├── services/               # 业务服务
│   ├── bangumi.rs
│   ├── cache.rs
│   ├── downloader.rs
│   ├── log.rs
│   ├── poster.rs
│   ├── scheduler.rs
│   ├── settings.rs
│   └── tracing_layer.rs
├── repositories/           # 数据访问
│   ├── bangumi.rs
│   ├── cache.rs
│   ├── log.rs
│   └── rss.rs
├── models/                 # 数据模型
├── state.rs               # AppState 定义
├── error.rs               # 错误处理
└── lib.rs                 # 入口
```

## 最佳实践

### Handler 编写

```rust
// ✅ Good: Handler 只做协议转换
pub async fn get_bangumi(State(state): State<AppState>) -> AppResult<Json<Vec<Bangumi>>> {
    let list = state.bangumi.get_all().await?;
    Ok(Json(list))
}

// ❌ Bad: Handler 包含业务逻辑
pub async fn get_bangumi(State(state): State<AppState>) -> AppResult<Json<Vec<Bangumi>>> {
    let list = BangumiRepository::get_all(&state.db).await?;
    // 不应在 handler 中直接调用 repository
    Ok(Json(list))
}
```

### Service 编写

```rust
// ✅ Good: Service 封装完整业务流程
impl BangumiService {
    pub async fn create(&self, data: CreateBangumi) -> Result<Bangumi, BangumiError> {
        // 1. 下载海报
        if let Some(url) = &data.poster_url {
            self.poster.try_download(url).await;
        }
        // 2. 创建 Bangumi
        let bangumi = BangumiRepository::create(&self.db, data).await?;
        // 3. 创建 RSS 订阅
        for entry in rss_entries {
            RssRepository::create(&self.db, entry).await?;
        }
        Ok(bangumi)
    }
}
```

### 新增功能检查清单

- [ ] 是否需要新建 Service？
- [ ] 是否需要新建 Repository？
- [ ] Handler 是否只做协议转换？
- [ ] 错误类型是否添加到 `AppError`？
- [ ] Service 是否添加到 `AppState`？
