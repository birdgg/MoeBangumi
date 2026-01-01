# 番剧日历

## 概述

番剧日历功能提供当季新番的每周放送时间表。数据从 GitHub 预生成的 JSON 文件下载，避免频繁调用外部 API。

## 数据来源

```
┌─────────────────────────────────────┐
│           GitHub Repository         │
│  assets/seed/calendar.json          │
└─────────────────────────────────────┘
                  │
                  ▼
         ┌───────────────┐
         │   Download    │
         │   on startup  │
         └───────────────┘
                  │
                  ▼
         ┌───────────────┐
         │   Database    │
         │  (metadata +  │
         │   calendar)   │
         └───────────────┘
```

### 数据更新流程

1. 服务启动时检查 `calendar` 表是否为空
2. 如果为空，从 GitHub 下载种子数据
3. 解析 JSON 并写入 `metadata` 和 `calendar` 表

## 种子数据

**下载地址：**
```
https://raw.githubusercontent.com/birdgg/moe-bangumi/main/assets/seed/calendar.json
```

**数据格式：**
```json
{
  "seasons": [
    {
      "year": 2025,
      "season": "winter",
      "entries": [
        {
          "bgmtv_id": 123456,
          "mikan_id": "3456",
          "title_chinese": "异世界动画",
          "title_japanese": "異世界アニメ",
          "platform": "tv",
          "total_episodes": 12,
          "poster_url": "https://...",
          "air_date": "2025-01-06",
          "air_week": 1,
          "year": 2025
        }
      ]
    }
  ]
}
```

## 数据模型

### CalendarSubject

日历中的番剧条目，字段与 `Metadata` 保持一致：

```rust
pub struct CalendarSubject {
    /// BGM.tv subject ID
    pub bgmtv_id: Option<i64>,
    /// Mikan 番剧 ID
    pub mikan_id: Option<String>,
    /// 中文标题
    pub title_chinese: String,
    /// 日文标题
    pub title_japanese: Option<String>,
    /// 首播日期 (YYYY-MM-DD)
    pub air_date: Option<String>,
    /// 放送星期 (0=周日, 1-6=周一至周六)
    pub air_week: i32,
    /// 海报图片 URL
    pub poster_url: Option<String>,
    /// 平台类型 (tv, movie, ova)
    pub platform: Platform,
    /// 总集数
    pub total_episodes: i32,
}
```

### CalendarDay

按星期分组的日历数据：

```rust
pub struct CalendarDay {
    pub weekday: Weekday,
    pub items: Vec<CalendarSubject>,
}

pub struct Weekday {
    pub id: i32,      // 1-7
    pub en: String,   // "Mon", "Tue", ...
    pub cn: String,   // "星期一", "星期二", ...
    pub ja: String,   // "月曜日", "火曜日", ...
}
```

### Platform

番剧平台类型：

```rust
pub enum Platform {
    Tv,     // TV 动画
    Movie,  // 剧场版
    Ova,    // OVA/OAD
}
```

## API

### GET /api/calendar

获取当季番剧日历。

**查询参数：**
- `year` - 年份（可选，默认当前年份）
- `season` - 季节：`winter`/`spring`/`summer`/`fall`（可选，默认当前季节）

**响应：**
```json
[
  {
    "weekday": {
      "id": 1,
      "en": "Mon",
      "cn": "星期一",
      "ja": "月曜日"
    },
    "items": [
      {
        "bgmtv_id": 123456,
        "mikan_id": "3456",
        "title_chinese": "异世界动画",
        "title_japanese": "異世界アニメ",
        "air_date": "2025-01-06",
        "air_week": 1,
        "poster_url": "https://...",
        "platform": "tv",
        "total_episodes": 12
      }
    ]
  }
]
```

### POST /api/calendar/refresh

重新从 GitHub 下载种子数据并更新日历。

**查询参数：**
- `year` - 年份（可选）
- `season` - 季节（可选）

**响应：** 同 GET /api/calendar

## 数据库表

### calendar 表

```sql
CREATE TABLE calendar (
    id INTEGER PRIMARY KEY,
    metadata_id INTEGER NOT NULL,
    year INTEGER NOT NULL,
    season TEXT NOT NULL,        -- 'winter', 'spring', 'summer', 'fall'
    priority INTEGER DEFAULT 0,
    created_at TEXT DEFAULT CURRENT_TIMESTAMP,
    updated_at TEXT DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (metadata_id) REFERENCES metadata(id),
    UNIQUE(metadata_id, year, season)
);
```

### metadata 表（相关字段）

```sql
CREATE TABLE metadata (
    id INTEGER PRIMARY KEY,
    mikan_id TEXT,
    bgmtv_id INTEGER,
    title_chinese TEXT NOT NULL,
    title_japanese TEXT,
    platform TEXT NOT NULL DEFAULT 'tv',  -- 'tv', 'movie', 'ova'
    total_episodes INTEGER DEFAULT 0,
    poster_url TEXT,
    air_date TEXT,
    air_week INTEGER,  -- 0=周日, 1-6=周一至周六
    ...
);
```

## 季节划分

| 季节 | 月份 | season 值 |
|------|------|-----------|
| 冬季 | 1-3月 | `winter` |
| 春季 | 4-6月 | `spring` |
| 夏季 | 7-9月 | `summer` |
| 秋季 | 10-12月 | `fall` |

## 维护种子数据

种子数据文件位于 `assets/seed/calendar.json`，需要手动维护：

1. 每季度开始前更新新番列表
2. 数据可从 Mikan 或 BGM.tv 获取后整理
3. 提交到仓库后，新部署的实例会自动获取
