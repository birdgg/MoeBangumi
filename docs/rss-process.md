# RSS 处理流程

本文档描述 MoeBangumi 的 RSS 订阅处理流程，包括 RSS 获取、正则过滤、标题解析和洗版逻辑。

## 概述

RSS 处理系统负责从 RSS 源获取番剧资源，经过多层过滤和解析后，自动创建下载任务。系统支持：
- HTTP 缓存优化（ETag/Last-Modified）
- 多级正则过滤（Include/Exclude）
- 智能标题解析（提取集数、字幕组、分辨率等）
- 同批次优先级预过滤（同集多资源只保留最高优先级）
- 优先级洗版（自动替换更高优先级资源）

## 触发入口

### 1. 定时任务（每小时）

位置：`crates/server/src/services/scheduler/rss_fetch_job.rs`

```rust
// 获取所有启用的 RSS 订阅
RssRepository::get_enabled(&self.db)
// 批量并发处理
rss_processing.process_batch(rss_list, &global_exclude_filters)
```

### 2. 即时处理（创建/更新番剧时）

位置：`crates/server/src/services/bangumi.rs`

```rust
// 创建新 RSS 订阅后，立即在后台处理
self.rss_processing.spawn_background(new_rss_ids)
```

### 3. 用户手动触发刷新

## 处理流程

### 第一步：HTTP 条件请求

位置：`crates/rss/src/client.rs`

使用 ETag 和 Last-Modified 头实现条件请求，减少不必要的网络传输：

```rust
// 发送条件请求
fetch_conditional(source, context) {
    // 添加 If-None-Match (ETag)
    // 添加 If-Modified-Since
    // 如果返回 304，直接跳过处理
}
```

### 第二步：RSS 解析

位置：`crates/rss/src/parsers/mikan.rs`

从 RSS XML 中提取关键信息：

```rust
pub struct RssItem {
    pub title: String,           // 资源标题
    pub torrent_url: String,     // 种子文件 URL
    pub info_hash: String,       // BitTorrent info hash
    pub pub_date: Option<String> // 发布时间 (ISO 8601)
}
```

### 第三步：时间过滤

位置：`crates/server/src/services/rss_processing.rs:540-568`

只处理比上次记录的 `last_pub_date` 更新的条目：

```rust
fn filter_by_pub_date(items: Vec<RssItem>, last_pub_date: Option<&str>) -> Vec<RssItem> {
    items.filter(|item| {
        match &item.pub_date {
            Some(pub_date) => pub_date > last_pub_date,
            None => true,  // 没有日期的保守处理，全部包含
        }
    })
}
```

### 第四步：正则过滤

位置：`crates/server/src/services/rss_processing.rs:485-536`

#### 过滤器来源

1. **全局 Exclude 过滤器**：从 Settings 读取 `filter.global_rss_filters`
   - 默认值：`["720[Pp]", "\\d-\\d", "合集"]`
2. **RSS 级 Exclude 过滤器**：每个 RSS 订阅独立配置
3. **RSS 级 Include 过滤器**：每个 RSS 订阅独立配置

#### 过滤逻辑

```rust
fn filter_rss_items(
    items: Vec<RssItem>,
    include_patterns: &[String],
    exclude_patterns: &[String],
) -> Vec<RssItem> {
    // 编译正则（大小写不敏感）
    let include_filters = compile_filters(include_patterns);
    let exclude_filters = compile_filters(exclude_patterns);

    items.filter(|item| {
        let title = item.title();

        // 1. Include 过滤（OR 逻辑）
        // 如果 include 为空，则全部通过
        // 否则必须匹配任一 include 规则
        let include_ok = include_filters.is_empty()
            || matches_any_filter(title, &include_filters);

        // 2. Exclude 过滤
        // 不能匹配任何 exclude 规则
        let exclude_ok = !matches_any_filter(title, &exclude_filters);

        // 两个条件都满足才通过（AND 逻辑）
        include_ok && exclude_ok
    })
}
```

#### 过滤示例

配置：
```json
{
  "include_filters": ["1080[Pp]", "ANi"],
  "exclude_filters": ["合集", "720[Pp]"]
}
```

| 标题 | Include 匹配 | Exclude 匹配 | 结果 |
|------|-------------|-------------|------|
| `[ANi] 孤独摇滚 01 [1080P]` | ✅ | ❌ | ✅ 通过 |
| `[喵萌] 孤独摇滚 01 [720P]` | ❌ | ✅ | ❌ 过滤 |
| `[ANi] 孤独摇滚 合集 [1080P]` | ✅ | ✅ | ❌ 过滤 |
| `[喵萌] 孤独摇滚 01 [1080P]` | ✅ | ❌ | ✅ 通过 |

### 第五步：标题解析

位置：`crates/parser/src/parser.rs`

从 RSS 标题中提取结构化信息：

```rust
pub struct ParseResult {
    pub name_en: Option<String>,        // 英文名
    pub name_zh: Option<String>,        // 中文名
    pub name_jp: Option<String>,        // 日文名
    pub episode: Option<i32>,           // 集数
    pub season: Option<i32>,            // 季度
    pub subtitle_group: Option<String>, // 字幕组
    pub resolution: Option<String>,     // 分辨率 (1080P/720P/2160P)
    pub sub_type: Vec<String>,          // 字幕类型 (CHS/CHT/JPN/ENG)
}
```

解析器支持多种集数格式：
- `[01]`、`[第01话]`、`EP01`、`(01)` 等

### 第六步：优先级预过滤

位置：`crates/server/src/services/rss_processing.rs:236-286`

当同一批次 RSS 中包含多个同集资源（如不同字幕组的简中和繁中版本）时，预先按优先级过滤，只保留每集优先级最高的资源。这避免了不必要的数据库操作和下载任务。

```rust
fn filter_by_priority(
    &self,
    parsed_items: Vec<(RssItem, i32, ParseResult)>,
) -> Vec<(RssItem, i32, ParseResult)> {
    // 1. 按集数分组
    let mut episodes_map: HashMap<i32, Vec<...>> = HashMap::new();
    for item in parsed_items {
        episodes_map.entry(item.1).or_default().push(item);
    }

    // 2. 每组只保留优先级最高的 item
    for (episode, items) in episodes_map {
        let best_item = items
            .into_iter()
            .min_by_key(|item| calculator.calculate_score(&item.to_comparable()));
        result.push(best_item);
    }
}
```

#### 过滤示例

假设 RSS 返回以下资源（同一集）：

| 标题 | 字幕组 | 语言 | 优先级得分 |
|------|--------|------|-----------|
| `[ANi] 孤独摇滚 05 [简日]` | ANi (rank=0) | 简日 (rank=0) | (0, 0) |
| `[喵萌] 孤独摇滚 05 [简繁日]` | 喵萌 (rank=1) | 简繁日 (rank=1) | (1, 1) |
| `[桜都] 孤独摇滚 05 [繁体]` | 桜都 (rank=2) | 繁体 (rank=2) | (2, 2) |

结果：只有 `[ANi]` 版本会被处理，其他版本在预过滤阶段被跳过。

### 第七步：洗版判断

位置：`crates/server/src/services/washing.rs`

当同一集已有种子时，判断是否需要洗版（替换为更高优先级资源）。

详细的洗版逻辑请参考：[priority-washing.md](./priority-washing.md)

#### 简要流程

```rust
// 1. 检查该集是否已有种子
if let Some(existing_torrents) = lookup.episodes_map.get(&episode) {
    // 2. 判断是否应该洗版
    if !self.washing.should_wash(existing_torrents, parse_result) {
        // 现有种子优先级更高或相等，跳过
        return;
    }

    // 3. 执行洗版
    self.washing.wash_episode(params).await;
}
```

#### 优先级比较

优先级顺序（权重从高到低）：
1. **字幕组** - 精确匹配
2. **字幕语言** - 模糊匹配（包含关系）
3. **分辨率** - 大小写不敏感匹配

```rust
// 优先级得分（rank 越小优先级越高）
pub struct PriorityScore {
    pub group_rank: usize,      // 权重最高
    pub language_rank: usize,   // 权重中等
    pub resolution_rank: usize, // 权重最低
}
```

#### 洗版执行

```rust
pub async fn wash_episode(&self, params: WashParams<'_>) -> Result<Vec<String>, WashingError> {
    // 1. 使用事务确保原子性
    let mut tx = self.db.begin().await?;

    // 2. 删除所有旧种子记录
    for existing in params.existing_torrents {
        TorrentRepository::delete_with_executor(&mut *tx, existing.id).await?;
    }

    // 3. 创建新种子记录
    TorrentRepository::create_with_executor(&mut *tx, new_torrent).await?;

    // 4. 提交事务
    tx.commit().await?;

    // 5. 从下载器删除旧任务（best-effort，失败不影响数据库操作）
    for hash in &old_hashes {
        self.delete_from_downloader(hash, params.rss_title).await;
    }

    Ok(old_hashes)
}
```

### 第八步：创建下载任务

位置：`crates/server/src/services/rss_processing.rs:359-392`

```rust
async fn add_download_task(...) {
    // 1. 生成 Plex/Jellyfin 兼容的文件名
    let filename = pathgen::generate_filename(
        &ctx.bangumi.metadata.title_chinese,  // "孤独摇滚"
        ctx.bangumi.metadata.season,          // 1
        episode,                              // 5
        Some(ctx.bangumi.metadata.platform)   // "bilibili"
    );
    // 输出示例: "孤独摇滚 S01E05 (bilibili).mkv"

    // 2. 构建下载选项
    let options = AddTaskOptions::new(torrent_url)
        .save_path(&ctx.bangumi.bangumi.save_path)
        .rename(&filename)
        .add_tag("moe")      // 标记为 moe 管理
        .add_tag("rename");  // 标记需要重命名服务处理

    // 3. 添加到下载器
    self.downloader.add_task(options).await?;
}
```

### 第九步：更新缓存

处理完成后，更新 RSS 订阅的缓存信息：

```rust
RssRepository::update_cache(
    &self.db,
    rss.id,
    new_etag,          // 新的 ETag
    new_last_modified, // 新的 Last-Modified
    latest_pub_date,   // 最新的发布时间
).await;
```

## 完整流程图

```
┌─────────────────────────────────────────────────────────────┐
│ 入口: RssFetchJob (定时) / BangumiService (即时)             │
└─────────────────────────┬───────────────────────────────────┘
                          ▼
┌─────────────────────────────────────────────────────────────┐
│ 1. HTTP 条件请求 (ETag/Last-Modified)                       │
│    └─ 304 Not Modified → 跳过处理                           │
└─────────────────────────┬───────────────────────────────────┘
                          ▼
┌─────────────────────────────────────────────────────────────┐
│ 2. RSS XML 解析                                             │
│    └─ 提取 title, torrent_url, info_hash, pub_date         │
└─────────────────────────┬───────────────────────────────────┘
                          ▼
┌─────────────────────────────────────────────────────────────┐
│ 3. 时间过滤                                                  │
│    └─ 只保留 pub_date > last_pub_date 的条目                │
└─────────────────────────┬───────────────────────────────────┘
                          ▼
┌─────────────────────────────────────────────────────────────┐
│ 4. 正则过滤                                                  │
│    ├─ Include: 必须匹配任一规则 (OR)                         │
│    └─ Exclude: 不能匹配任何规则 (全局 + RSS级 合并)          │
└─────────────────────────┬───────────────────────────────────┘
                          ▼
┌─────────────────────────────────────────────────────────────┐
│ 5. 标题解析                                                  │
│    └─ 提取集数、字幕组、分辨率、语言等                       │
└─────────────────────────┬───────────────────────────────────┘
                          ▼
┌─────────────────────────────────────────────────────────────┐
│ 6. 优先级预过滤                                              │
│    └─ 同集多资源只保留优先级最高的一个                       │
└─────────────────────────┬───────────────────────────────────┘
                          ▼
┌─────────────────────────────────────────────────────────────┐
│ 7. 逐项处理                                                  │
│    ├─ info_hash 已存在 → 跳过                                │
│    ├─ 该集无种子 → 创建记录 + 添加下载                       │
│    └─ 该集有种子 → 洗版判断                                  │
│        ├─ 新种子优先级更高 → 执行洗版                        │
│        └─ 否则 → 跳过                                       │
└─────────────────────────┬───────────────────────────────────┘
                          ▼
┌─────────────────────────────────────────────────────────────┐
│ 8. 创建下载任务                                              │
│    ├─ 生成 Plex/Jellyfin 兼容文件名                         │
│    └─ 添加到下载器 (qBittorrent/Transmission)               │
└─────────────────────────┬───────────────────────────────────┘
                          ▼
┌─────────────────────────────────────────────────────────────┐
│ 9. 更新缓存                                                  │
│    └─ 更新 etag, last_modified, last_pub_date              │
└─────────────────────────────────────────────────────────────┘
```

## 核心文件列表

| 文件路径 | 说明 |
|---------|------|
| `crates/server/src/services/rss_processing.rs` | RSS 处理核心编排逻辑 |
| `crates/server/src/services/washing.rs` | 洗版服务层，数据库事务 |
| `crates/washing/src/lib.rs` | 洗版优先级算法 |
| `crates/parser/src/parser.rs` | 标题解析核心逻辑 |
| `crates/rss/src/client.rs` | RSS 获取和条件请求 |
| `crates/rss/src/parsers/mikan.rs` | Mikan XML 解析 |
| `crates/server/src/services/scheduler/rss_fetch_job.rs` | 定时任务入口 |

## 相关文档

- [洗版优先级系统](./priority-washing.md) - 详细的优先级配置和洗版算法说明
- [文件重命名](./file-rename.md) - 下载完成后的文件重命名逻辑
