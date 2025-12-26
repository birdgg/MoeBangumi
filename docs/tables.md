# 数据库表设计文档

本文档描述 `moe-rs` 项目的 SQLite 数据库表结构设计。

## 概览

```
┌─────────────┐      ┌─────────────┐      ┌─────────────┐      ┌──────────────────┐
│   bangumi   │◄────┤     rss     │      │   torrent   │─────►│  download_task   │
│   (番剧)    │      │  (RSS订阅)  │      │   (种子)    │      │    (下载任务)     │
└─────────────┘      └─────────────┘      └─────────────┘      └──────────────────┘
       ▲                    │                    ▲
       │                    │                    │
       └────────────────────┴────────────────────┘

┌─────────────┐      ┌─────────────┐
│    cache    │      │     log     │
│   (缓存)    │      │   (日志)    │
└─────────────┘      └─────────────┘
```

| 表名 | 描述 | 关联 |
|------|------|------|
| `bangumi` | 番剧主表，存储番剧基本信息 | 被 `rss`、`torrent` 引用 |
| `rss` | RSS 订阅表，每个番剧可有多个订阅源 | 引用 `bangumi` |
| `torrent` | 种子表，记录已下载或待下载的种子 | 引用 `bangumi`、`rss` |
| `download_task` | 下载任务表，跟踪种子下载状态 | 引用 `torrent` |
| `cache` | 通用缓存表，缓存外部 API 响应 | 无关联 |
| `log` | 系统日志表，记录操作日志 | 无关联 |

---

## bangumi (番剧表)

主表，存储所有追踪的番剧信息。

### 字段定义

| 字段名 | 类型 | 约束 | 默认值 | 描述 |
|--------|------|------|--------|------|
| `id` | INTEGER | PRIMARY KEY, AUTOINCREMENT | - | 主键 |
| `created_at` | DATETIME | NOT NULL | CURRENT_TIMESTAMP | 创建时间 |
| `updated_at` | DATETIME | NOT NULL | CURRENT_TIMESTAMP | 更新时间 (触发器自动维护) |
| `title_chinese` | TEXT | NOT NULL | - | 中文标题 (主显示名) |
| `title_japanese` | TEXT | - | NULL | 日文原名 |
| `title_original_chinese` | TEXT | NOT NULL, UNIQUE | '' | 中文原名 (用于去重) |
| `title_original_japanese` | TEXT | - | NULL | 日文原名 |
| `season` | INTEGER | NOT NULL | 1 | 季度编号 |
| `year` | INTEGER | NOT NULL | - | 年份 |
| `bgmtv_id` | INTEGER | UNIQUE (非零时) | NULL | Bangumi.tv ID |
| `tmdb_id` | INTEGER | - | NULL | TMDB ID |
| `poster_url` | TEXT | - | NULL | 海报 URL |
| `air_date` | DATE | - | NULL | 首播日期 |
| `air_week` | INTEGER | - | NULL | 播出星期 (0=周日, 1=周一, ..., 6=周六) |
| `total_episodes` | INTEGER | NOT NULL | 0 | 总集数 (0=未知) |
| `episode_offset` | INTEGER | NOT NULL | 0 | 集数偏移量 |
| `kind` | TEXT | - | 'TV' | 类型: TV, Movie, OVA 等 |
| `current_episode` | INTEGER | NOT NULL | 0 | 当前已下载集数 |
| `auto_download` | INTEGER | NOT NULL | 1 | 是否自动下载 (布尔值) |
| `finished` | INTEGER | NOT NULL | 0 | 是否已完结 (布尔值) |
| `save_path` | TEXT | - | NULL | 自定义保存路径 (空=使用默认) |
| `source_type` | TEXT | NOT NULL | 'webrip' | 来源类型: 'webrip' 或 'bdrip' |

### 索引

| 索引名 | 类型 | 字段 | 说明 |
|--------|------|------|------|
| `idx_bangumi_title_chinese` | INDEX | title_chinese | 中文标题查询 |
| `idx_bangumi_title_japanese` | INDEX | title_japanese | 日文标题查询 |
| `idx_bangumi_title_original_chinese` | INDEX | title_original_chinese | 中文原名查询 |
| `idx_bangumi_title_original_chinese_unique` | UNIQUE | title_original_chinese | 中文原名唯一约束 |
| `idx_bangumi_title_original_japanese` | INDEX | title_original_japanese | 日文原名查询 |
| `idx_bangumi_season` | INDEX | season | 季度筛选 |
| `idx_bangumi_year` | INDEX | year | 年份筛选 |
| `idx_bangumi_air_date` | INDEX | air_date | 播出日期排序 |
| `idx_bangumi_bgmtv_id` | UNIQUE (条件) | bgmtv_id | 非空非零时唯一 |

### 触发器

- `update_bangumi_timestamp`: UPDATE 后自动更新 `updated_at` 字段

---

## rss (RSS 订阅表)

存储番剧的 RSS 订阅源，每个番剧可关联多个订阅。

### 字段定义

| 字段名 | 类型 | 约束 | 默认值 | 描述 |
|--------|------|------|--------|------|
| `id` | INTEGER | PRIMARY KEY, AUTOINCREMENT | - | 主键 |
| `created_at` | DATETIME | NOT NULL | CURRENT_TIMESTAMP | 创建时间 |
| `updated_at` | DATETIME | NOT NULL | CURRENT_TIMESTAMP | 更新时间 |
| `bangumi_id` | INTEGER | NOT NULL, FK → bangumi(id) | - | 关联番剧 ID |
| `url` | TEXT | NOT NULL | - | RSS 订阅 URL |
| `enabled` | INTEGER | NOT NULL | 1 | 是否启用 (布尔值) |
| `exclude_filters` | TEXT | NOT NULL | '[]' | 排除规则 (JSON 数组，正则表达式) |
| `is_primary` | INTEGER | NOT NULL | 0 | 是否为主订阅 (每番剧仅一个) |

### 外键关系

| 外键 | 引用 | 删除行为 |
|------|------|----------|
| `bangumi_id` | `bangumi(id)` | CASCADE (级联删除) |

### 索引

| 索引名 | 类型 | 字段 | 说明 |
|--------|------|------|------|
| `idx_rss_bangumi_id` | INDEX | bangumi_id | 按番剧查询订阅 |
| `idx_rss_enabled` | INDEX | enabled | 筛选启用的订阅 |
| `idx_rss_is_primary` | INDEX | is_primary | 主订阅筛选 |
| `idx_rss_bangumi_primary` | UNIQUE (条件) | bangumi_id | 每番剧仅一个主订阅 (WHERE is_primary=1) |

### 触发器

- `update_rss_timestamp`: UPDATE 后自动更新 `updated_at` 字段

---

## torrent (种子表)

存储番剧剧集对应的种子信息，通过 info_hash 全局唯一标识。

### 字段定义

| 字段名 | 类型 | 约束 | 默认值 | 描述 |
|--------|------|------|--------|------|
| `id` | INTEGER | PRIMARY KEY, AUTOINCREMENT | - | 主键 |
| `created_at` | DATETIME | NOT NULL | CURRENT_TIMESTAMP | 创建时间 |
| `updated_at` | DATETIME | NOT NULL | CURRENT_TIMESTAMP | 更新时间 |
| `bangumi_id` | INTEGER | NOT NULL, FK → bangumi(id) | - | 关联番剧 ID |
| `rss_id` | INTEGER | FK → rss(id) | NULL | 来源 RSS ID (可选) |
| `info_hash` | TEXT | NOT NULL, UNIQUE | - | 种子 info_hash (v1: 40字符, v2: 64字符) |
| `episode_number` | INTEGER | NOT NULL | - | 剧集编号 |

### 外键关系

| 外键 | 引用 | 删除行为 |
|------|------|----------|
| `bangumi_id` | `bangumi(id)` | CASCADE (级联删除) |
| `rss_id` | `rss(id)` | SET NULL (置空) |

### 索引

| 索引名 | 类型 | 字段 | 说明 |
|--------|------|------|------|
| `idx_torrent_bangumi_id` | INDEX | bangumi_id | 按番剧查询种子 |
| `idx_torrent_rss_id` | INDEX | rss_id | 按 RSS 来源查询 |
| `idx_torrent_episode_number` | INDEX | episode_number | 按集数查询 |
| `idx_torrent_info_hash` | UNIQUE | info_hash | info_hash 全局唯一 |
| `idx_torrent_bangumi_episode` | INDEX | (bangumi_id, episode_number) | 复合索引：番剧+集数查询 |

### 触发器

- `update_torrent_timestamp`: UPDATE 后自动更新 `updated_at` 字段

---

## download_task (下载任务表)

跟踪种子的下载状态，保留完整下载历史。

### 字段定义

| 字段名 | 类型 | 约束 | 默认值 | 描述 |
|--------|------|------|--------|------|
| `id` | INTEGER | PRIMARY KEY, AUTOINCREMENT | - | 主键 |
| `created_at` | DATETIME | NOT NULL | CURRENT_TIMESTAMP | 创建时间 |
| `updated_at` | DATETIME | NOT NULL | CURRENT_TIMESTAMP | 更新时间 |
| `torrent_id` | INTEGER | NOT NULL, FK → torrent(id) | - | 关联种子 ID |
| `status` | TEXT | NOT NULL, CHECK | 'pending' | 任务状态 |
| `error_message` | TEXT | - | NULL | 失败时的错误信息 |

### 状态枚举

| 状态值 | 描述 |
|--------|------|
| `pending` | 等待下载 |
| `paused` | 已暂停 |
| `downloading` | 下载中 |
| `completed` | 已完成 |
| `failed` | 下载失败 |

### 外键关系

| 外键 | 引用 | 删除行为 |
|------|------|----------|
| `torrent_id` | `torrent(id)` | CASCADE (级联删除) |

### 索引

| 索引名 | 类型 | 字段 | 说明 |
|--------|------|------|------|
| `idx_download_task_torrent_id` | INDEX | torrent_id | 按种子查询任务 |
| `idx_download_task_status` | INDEX | status | 按状态筛选 |
| `idx_download_task_created_at` | INDEX | created_at | 按创建时间排序 |
| `idx_download_task_status_created` | INDEX | (status, created_at) | 复合索引：状态+时间查询 |

### 触发器

- `update_download_task_timestamp`: UPDATE 后自动更新 `updated_at` 字段

---

## cache (缓存表)

通用键值缓存，用于存储外部 API 响应。

### 字段定义

| 字段名 | 类型 | 约束 | 默认值 | 描述 |
|--------|------|------|--------|------|
| `cache_key` | TEXT | PRIMARY KEY | - | 缓存键 |
| `data` | TEXT | NOT NULL | - | 缓存数据 (JSON) |
| `fetched_at` | INTEGER | NOT NULL | - | 获取时间 (Unix 时间戳) |

### 索引

| 索引名 | 类型 | 字段 | 说明 |
|--------|------|------|------|
| `idx_cache_fetched_at` | INDEX | fetched_at | 用于过期清理 |

### 使用模式

通过 `CacheService` 提供 `get_or_fetch` 模式：

```rust
let data = cache.get_or_fetch(&key, TTL, || async {
    external_api.fetch().await
}).await?;
```

---

## log (日志表)

系统日志表，记录操作日志和用户通知。

### 字段定义

| 字段名 | 类型 | 约束 | 默认值 | 描述 |
|--------|------|------|--------|------|
| `id` | INTEGER | PRIMARY KEY, AUTOINCREMENT | - | 主键 |
| `created_at` | DATETIME | NOT NULL | CURRENT_TIMESTAMP | 创建时间 |
| `level` | TEXT | NOT NULL, CHECK | - | 日志级别 |
| `message` | TEXT | NOT NULL | - | 日志内容 |

### 级别枚举

| 级别 | 描述 |
|------|------|
| `info` | 信息 |
| `warning` | 警告 |
| `error` | 错误 |

### 索引

| 索引名 | 类型 | 字段 | 说明 |
|--------|------|------|------|
| `idx_log_created_at` | INDEX | created_at DESC | 按时间倒序查询 |
| `idx_log_level` | INDEX | level | 按级别筛选 |
| `idx_log_level_created` | INDEX | (level, created_at DESC) | 复合索引：级别+时间 |

---

## 实体关系图 (ER Diagram)

```
                              ┌─────────────────────────────────────────────┐
                              │                  bangumi                    │
                              ├─────────────────────────────────────────────┤
                              │ PK  id                    INTEGER           │
                              │     created_at            DATETIME          │
                              │     updated_at            DATETIME          │
                              │     title_chinese         TEXT              │
                              │     title_japanese        TEXT              │
                              │     title_original_chinese TEXT             │
                              │     title_original_japanese TEXT            │
                              │     season                INTEGER           │
                              │     year                  INTEGER           │
                              │     bgmtv_id              INTEGER           │
                              │     tmdb_id               INTEGER           │
                              │     poster_url            TEXT              │
                              │     air_date              DATE              │
                              │     air_week              INTEGER           │
                              │     total_episodes        INTEGER           │
                              │     episode_offset        INTEGER           │
                              │     kind                  TEXT              │
                              │     current_episode       INTEGER           │
                              │     auto_download         INTEGER           │
                              │     finished              INTEGER           │
                              │     save_path             TEXT              │
                              │     source_type           TEXT              │
                              └─────────────────────────────────────────────┘
                                           │                      │
                                           │ 1:N                  │ 1:N
                                           ▼                      ▼
              ┌──────────────────────────────────────┐    ┌──────────────────────────────────────┐
              │                 rss                  │    │               torrent                │
              ├──────────────────────────────────────┤    ├──────────────────────────────────────┤
              │ PK  id                INTEGER        │    │ PK  id                INTEGER        │
              │     created_at        DATETIME       │    │     created_at        DATETIME       │
              │     updated_at        DATETIME       │    │     updated_at        DATETIME       │
              │ FK  bangumi_id        INTEGER ───────│────│ FK  bangumi_id        INTEGER        │
              │     url               TEXT           │◄───│ FK  rss_id            INTEGER        │
              │     enabled           INTEGER        │    │     info_hash         TEXT           │
              │     exclude_filters   TEXT           │    │     episode_number    INTEGER        │
              │     is_primary        INTEGER        │    └──────────────────────────────────────┘
              └──────────────────────────────────────┘                      │
                                                                            │ 1:N
                                                                            ▼
                                                      ┌──────────────────────────────────────┐
                                                      │            download_task             │
                                                      ├──────────────────────────────────────┤
                                                      │ PK  id                INTEGER        │
                                                      │     created_at        DATETIME       │
                                                      │     updated_at        DATETIME       │
                                                      │ FK  torrent_id        INTEGER        │
                                                      │     status            TEXT           │
                                                      │     error_message     TEXT           │
                                                      └──────────────────────────────────────┘


┌──────────────────────────────────────┐    ┌──────────────────────────────────────┐
│               cache                  │    │                 log                  │
├──────────────────────────────────────┤    ├──────────────────────────────────────┤
│ PK  cache_key         TEXT           │    │ PK  id                INTEGER        │
│     data              TEXT           │    │     created_at        DATETIME       │
│     fetched_at        INTEGER        │    │     level             TEXT           │
└──────────────────────────────────────┘    │     message           TEXT           │
                                            └──────────────────────────────────────┘
```

---

## 设计说明

### 级联删除策略

1. **bangumi 删除时**:
   - 关联的 `rss` 记录级联删除
   - 关联的 `torrent` 记录级联删除
   - 进而关联的 `download_task` 级联删除

2. **rss 删除时**:
   - 关联的 `torrent.rss_id` 置为 NULL (保留种子记录)

3. **torrent 删除时**:
   - 关联的 `download_task` 级联删除

### 布尔值存储

SQLite 不支持原生布尔类型，使用 INTEGER 存储:
- `0` = false
- `1` = true

相关字段: `auto_download`, `finished`, `enabled`, `is_primary`

### 时间戳维护

所有主要表 (`bangumi`, `rss`, `torrent`, `download_task`) 都有:
- `created_at`: 记录创建时自动设置
- `updated_at`: 通过触发器在 UPDATE 时自动更新

### 唯一性约束

| 表 | 字段 | 条件 |
|----|------|------|
| `bangumi` | `title_original_chinese` | 全局唯一 |
| `bangumi` | `bgmtv_id` | 非空非零时唯一 |
| `rss` | `bangumi_id` | `is_primary=1` 时唯一 |
| `torrent` | `info_hash` | 全局唯一 |
