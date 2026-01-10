-- ==================== 新表结构设计 ====================
-- 版本: v2
-- 主要变化:
--   1. 删除 metadata 表，合并到 bangumi
--   2. 新增 series 表，关联同系列不同季
--   3. 新增 torrent_bangumi 表，支持 BDRip 多季关联

-- ==================== Series 系列表 ====================
-- 代表一个动画系列（如 Re:Zero 从零开始的异世界生活）
-- 用于关联同系列的不同季
CREATE TABLE series (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    created_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,

    -- 外部 ID
    tmdb_id INTEGER UNIQUE,                 -- TMDB TV Show ID

    -- 基本信息
    title_chinese TEXT NOT NULL,            -- 中文标题
    title_japanese TEXT,                    -- 日文标题
    poster_url TEXT                         -- 海报 URL
);

CREATE INDEX idx_series_title_chinese ON series(title_chinese);

CREATE TRIGGER update_series_timestamp
AFTER UPDATE ON series
FOR EACH ROW
BEGIN
    UPDATE series SET updated_at = CURRENT_TIMESTAMP WHERE id = OLD.id;
END;

-- ==================== Bangumi 番剧表 ====================
-- 合并原 metadata + bangumi 表
-- 代表一个具体的季/作品（如 Re:Zero 第二季）
CREATE TABLE bangumi (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    created_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,

    -- 关联系列（必须，movie/OVA 也创建 series）
    series_id INTEGER NOT NULL REFERENCES series(id) ON DELETE CASCADE,

    -- 外部 ID
    mikan_id TEXT,                          -- Mikan ID
    bgmtv_id INTEGER,                       -- BGM.tv subject ID
    -- 注：tmdb_id 统一在 series 表，通过 series_id 关联获取

    -- 元数据
    title_chinese TEXT NOT NULL,            -- 中文标题
    title_japanese TEXT,                    -- 日文标题
    season INTEGER NOT NULL DEFAULT 1,      -- 季数
    year INTEGER NOT NULL,                  -- 年份
    total_episodes INTEGER NOT NULL DEFAULT 0,  -- 总集数
    poster_url TEXT,                        -- 海报 URL
    air_date DATE,                          -- 开播日期
    air_week INTEGER NOT NULL DEFAULT 0,    -- 播出星期 (0=周日, 1-6=周一至周六)
    platform TEXT NOT NULL DEFAULT 'tv' CHECK(platform IN ('tv', 'movie', 'ova')),

    -- 订阅配置
    current_episode INTEGER NOT NULL DEFAULT 0,   -- 当前已下载集数
    episode_offset INTEGER NOT NULL DEFAULT 0,    -- 集数偏移
    auto_complete INTEGER NOT NULL DEFAULT 1,     -- 自动完成（每次只下载一集）
    source_type TEXT NOT NULL DEFAULT 'webrip' CHECK(source_type IN ('webrip', 'bdrip'))
    -- 注：save_path 动态计算 = {base_path}/{series.title_chinese} ({series.year}) {tmdb-{series.tmdb_id}}/Season {season:02}/
);

CREATE INDEX idx_bangumi_series_id ON bangumi(series_id);
CREATE INDEX idx_bangumi_title_chinese ON bangumi(title_chinese);
CREATE INDEX idx_bangumi_year ON bangumi(year);
CREATE INDEX idx_bangumi_season ON bangumi(season);
CREATE INDEX idx_bangumi_air_week ON bangumi(air_week);
CREATE UNIQUE INDEX idx_bangumi_mikan_id ON bangumi(mikan_id) WHERE mikan_id IS NOT NULL;
CREATE UNIQUE INDEX idx_bangumi_bgmtv_id ON bangumi(bgmtv_id) WHERE bgmtv_id IS NOT NULL AND bgmtv_id != 0;

CREATE TRIGGER update_bangumi_timestamp
AFTER UPDATE ON bangumi
FOR EACH ROW
BEGIN
    UPDATE bangumi SET updated_at = CURRENT_TIMESTAMP WHERE id = OLD.id;
END;

-- ==================== RSS 订阅表 ====================
CREATE TABLE rss (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    created_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,

    -- 关联 bangumi
    bangumi_id INTEGER NOT NULL REFERENCES bangumi(id) ON DELETE CASCADE,

    -- RSS 信息
    title TEXT NOT NULL,                    -- RSS 标题
    url TEXT NOT NULL,                      -- RSS URL
    enabled INTEGER NOT NULL DEFAULT 1,     -- 是否启用
    exclude_filters TEXT NOT NULL DEFAULT '[]',  -- 排除过滤器 (JSON array)
    include_filters TEXT NOT NULL DEFAULT '[]',  -- 包含过滤器 (JSON array)
    subtitle_group TEXT,                    -- 字幕组

    -- HTTP 缓存
    etag TEXT,                              -- ETag
    last_modified TEXT,                     -- Last-Modified
    last_pub_date TEXT                      -- 最后处理的 pubDate
);

CREATE INDEX idx_rss_bangumi_id ON rss(bangumi_id);
CREATE INDEX idx_rss_enabled ON rss(enabled);

CREATE TRIGGER update_rss_timestamp
AFTER UPDATE ON rss
FOR EACH ROW
BEGIN
    UPDATE rss SET updated_at = CURRENT_TIMESTAMP WHERE id = OLD.id;
END;

-- ==================== Torrent 种子表 ====================
-- 所有元数据（episode, subtitle_group, subtitle_languages, resolution）
-- 都从 torrent_url 实时解析，不存储
CREATE TABLE torrent (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    created_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,

    rss_id INTEGER REFERENCES rss(id) ON DELETE SET NULL,  -- 来源 RSS（可选）

    info_hash TEXT NOT NULL,                -- Info hash (唯一标识)
    torrent_url TEXT NOT NULL               -- Torrent URL
);

CREATE UNIQUE INDEX idx_torrent_info_hash ON torrent(info_hash);
CREATE INDEX idx_torrent_rss_id ON torrent(rss_id);

CREATE TRIGGER update_torrent_timestamp
AFTER UPDATE ON torrent
FOR EACH ROW
BEGIN
    UPDATE torrent SET updated_at = CURRENT_TIMESTAMP WHERE id = OLD.id;
END;

-- ==================== Torrent-Bangumi 关联表 ====================
-- 统一管理 torrent 和 bangumi 的关系
-- WebRip: 一条记录 (torrent_id, bangumi_id)
-- BDRip: 多条记录 (torrent_id, bangumi_1), (torrent_id, bangumi_2), ...
CREATE TABLE torrent_bangumi (
    torrent_id INTEGER NOT NULL REFERENCES torrent(id) ON DELETE CASCADE,
    bangumi_id INTEGER NOT NULL REFERENCES bangumi(id) ON DELETE CASCADE,
    PRIMARY KEY (torrent_id, bangumi_id)
);

CREATE INDEX idx_torrent_bangumi_bangumi_id ON torrent_bangumi(bangumi_id);

-- ==================== Calendar 日历表 ====================
CREATE TABLE calendar (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    created_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,

    -- 关联 bangumi
    bangumi_id INTEGER NOT NULL REFERENCES bangumi(id) ON DELETE CASCADE,

    -- 季度信息
    year INTEGER NOT NULL,                  -- 年份
    season TEXT NOT NULL CHECK(season IN ('winter', 'spring', 'summer', 'fall')),
    priority INTEGER NOT NULL DEFAULT 0     -- 优先级（基于 BGM.tv 收藏数）
);

CREATE UNIQUE INDEX idx_calendar_bangumi_season ON calendar(bangumi_id, year, season);
CREATE INDEX idx_calendar_season ON calendar(year, season);
CREATE INDEX idx_calendar_priority ON calendar(priority DESC);

CREATE TRIGGER update_calendar_timestamp
AFTER UPDATE ON calendar
FOR EACH ROW
BEGIN
    UPDATE calendar SET updated_at = CURRENT_TIMESTAMP WHERE id = OLD.id;
END;

-- ==================== Cache 缓存表 ====================
CREATE TABLE cache (
    cache_key TEXT PRIMARY KEY,
    data TEXT NOT NULL,
    fetched_at INTEGER NOT NULL             -- Unix timestamp
);

CREATE INDEX idx_cache_fetched_at ON cache(fetched_at);

-- ==================== Log 日志表 ====================
CREATE TABLE log (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    created_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
    level TEXT NOT NULL CHECK(level IN ('info', 'warning', 'error')),
    message TEXT NOT NULL
);

CREATE INDEX idx_log_created_at ON log(created_at DESC);
CREATE INDEX idx_log_level ON log(level);
CREATE INDEX idx_log_level_created ON log(level, created_at DESC);
