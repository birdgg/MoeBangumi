# 日志系统设计

## 概述

系统采用事件驱动的日志架构，将 `tracing` 日志自动捕获并持久化到 SQLite 数据库，同时通过 SSE (Server-Sent Events) 实时推送到前端。

## 架构图

```
┌─────────────────────────────────────────────────────────────────┐
│                         Application                              │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────────────────┐  │
│  │ RssFetchJob │  │ Downloader  │  │    Other Services       │  │
│  └──────┬──────┘  └──────┬──────┘  └────────────┬────────────┘  │
│         │                │                      │                │
│         └────────────────┼──────────────────────┘                │
│                          ▼                                       │
│              ┌─────────────────────┐                            │
│              │  tracing::error!()  │                            │
│              │  tracing::warn!()   │                            │
│              │  tracing::info!()   │                            │
│              └──────────┬──────────┘                            │
└─────────────────────────┼───────────────────────────────────────┘
                          ▼
              ┌─────────────────────┐
              │   DatabaseLayer     │  ← tracing Layer
              │  (tracing_layer.rs) │
              └──────────┬──────────┘
                         │ mpsc channel
                         ▼
              ┌─────────────────────┐
              │   Log Writer Task   │  ← tokio::spawn
              │  - Deduplication    │     (60s window)
              │  - Level filtering  │     (error/warn only)
              └──────────┬──────────┘
                         ▼
              ┌─────────────────────┐
              │    EventService     │
              │   (event.rs)        │
              └──────────┬──────────┘
                         │
          ┌──────────────┼──────────────┐
          ▼              ▼              ▼
   ┌────────────┐ ┌────────────┐ ┌────────────┐
   │  SQLite    │ │  Memory    │ │ Broadcast  │
   │  Database  │ │  Buffer    │ │  Channel   │
   │  (event)   │ │ (100 items)│ │   (SSE)    │
   └────────────┘ └────────────┘ └─────┬──────┘
                                       │
                                       ▼
                              ┌────────────────┐
                              │   Frontend     │
                              │  EventSource   │
                              │  + Toast       │
                              └────────────────┘
```

## 核心组件

### 1. DatabaseLayer (`tracing_layer.rs`)

自定义 tracing Layer，捕获日志事件并发送到处理管道。

```rust
pub struct DatabaseLayer {
    sender: mpsc::Sender<LogEvent>,
}

pub struct LogEvent {
    pub level: EventLevel,
    pub message: String,
    pub details: Option<String>,  // 额外字段转为 JSON
}
```

**特性：**
- 仅捕获 `ERROR` 和 `WARN` 级别日志
- 自动提取 tracing 事件的额外字段作为 `details`
- 通过 mpsc channel 异步发送，不阻塞主线程

### 2. Log Writer (`start_log_writer`)

后台任务，处理日志去重和写入。

**去重机制：**
- 60 秒内相同消息不重复记录
- 使用 `HashMap<String, Instant>` 追踪已处理消息
- 每小时清理过期条目

```rust
const DEDUP_WINDOW: Duration = Duration::from_secs(60);
```

### 3. EventService (`event.rs`)

事件服务，管理事件的存储、广播和查询。

```rust
pub struct EventService {
    db: SqlitePool,
    buffer: Arc<RwLock<EventBuffer>>,  // 内存缓冲 (100 条)
    broadcaster: broadcast::Sender<Event>,  // SSE 广播
}
```

**方法：**
| 方法 | 用途 |
|------|------|
| `record()` | 记录事件到数据库并广播 |
| `info/warning/error()` | 便捷记录方法 |
| `subscribe()` | 获取 SSE 订阅 |
| `recent()` | 获取内存缓冲中的最近事件 |
| `cleanup()` | 清理过期事件 |

### 4. EventCleanupJob

定时清理任务，每天运行一次，删除 30 天前的事件。

```rust
impl SchedulerJob for EventCleanupJob {
    fn interval(&self) -> Duration {
        Duration::from_secs(86400)  // 24 hours
    }
}
```

## 数据模型

### Event 表

```sql
CREATE TABLE event (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    created_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
    level TEXT NOT NULL CHECK(level IN ('info', 'warning', 'error')),
    message TEXT NOT NULL,
    details TEXT
);

CREATE INDEX idx_event_created_at ON event(created_at);
CREATE INDEX idx_event_level ON event(level);
```

### Event 结构

```rust
pub struct Event {
    pub id: i64,
    pub created_at: DateTime<Utc>,
    /// 事件级别: info, warning, error
    pub level: EventLevel,
    /// 事件消息，简短描述发生了什么
    pub message: String,
    /// 详细信息，如错误堆栈或额外上下文
    pub details: Option<String>,
}
```

## API 端点

| 方法 | 路径 | 描述 |
|------|------|------|
| GET | `/api/events` | 获取事件列表 (支持 level/limit 过滤) |
| GET | `/api/events/stream` | SSE 事件流 |
| DELETE | `/api/events` | 清理 30 天前的事件 |

## 前端集成

### SSE Hook (`use-event-stream.ts`)

```typescript
export function useEventStream() {
  useEffect(() => {
    const eventSource = new EventSource("/api/events/stream");

    eventSource.onmessage = (e) => {
      const event = JSON.parse(e.data);
      // 仅对 warning/error 显示 toast
      if (event.level === "error") {
        toast.error(event.message);
      } else if (event.level === "warning") {
        toast.warning(event.message);
      }
    };

    return () => eventSource.close();
  }, []);
}
```

### 事件页面 (`/events`)

显示所有事件列表，支持按级别过滤，自动刷新。

## 错误处理 (AppError)

统一的应用错误类型，自动记录日志并返回适当的 HTTP 状态码。

```rust
pub enum AppError {
    NotFound(String),      // 404
    BadRequest(String),    // 400
    Database(sqlx::Error), // 500 (自动 tracing::error!)
    Downloader(DownloaderError), // 400/401/500
    ExternalApi(String),   // 502
    Internal(String),      // 500
    // ...
}
```

**响应格式：**
```json
{
  "error": "错误描述",
  "details": "详细信息 (可选)"
}
```

## 使用示例

### 后台任务记录错误

```rust
// 简单日志 - message 包含所有信息
tracing::error!("RSS 订阅处理失败: {} - {}", rss.url, e);

// 结构化日志 - 额外字段会被提取到 details
tracing::error!(
    url = %rss.url,
    error = %e,
    "RSS 订阅处理失败"
);
// details: {"url": "https://...", "error": "connection refused"}
```

### Handler 中返回错误

```rust
pub async fn get_bangumi(
    State(state): State<AppState>,
    Path(id): Path<i64>,
) -> AppResult<Json<Bangumi>> {
    let bangumi = BangumiRepository::get_by_id(&state.db, id)
        .await?  // 数据库错误自动转换为 AppError
        .ok_or_else(|| AppError::not_found("Bangumi not found"))?;

    Ok(Json(bangumi))
}
```

## 配置

| 参数 | 值 | 说明 |
|------|------|------|
| 去重窗口 | 60s | 相同消息 60 秒内不重复 |
| 内存缓冲 | 100 条 | SSE 连接时推送最近事件 |
| 广播容量 | 256 | SSE broadcast channel 容量 |
| 清理周期 | 24h | 定时清理任务间隔 |
| 保留天数 | 30d | 事件保留时长 |
