-- Bangumi (番剧) subscription table
-- Stores subscription state and metadata (merged from former metadata table)
CREATE TABLE IF NOT EXISTS bangumi (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    created_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,

    -- External service IDs
    mikan_id TEXT,                                  -- Mikan bangumi ID (from mikanani.me)
    bgmtv_id INTEGER,                               -- BGM.tv subject ID
    tmdb_id INTEGER,                                -- TMDB ID

    -- Titles (bilingual support)
    title_chinese TEXT NOT NULL,                    -- Chinese title (primary display)
    title_japanese TEXT,                            -- Japanese original name

    -- Basic info
    season INTEGER NOT NULL DEFAULT 1,              -- Season number
    year INTEGER NOT NULL,                          -- Year
    platform TEXT NOT NULL DEFAULT 'tv' CHECK(platform IN ('tv', 'movie', 'ova')),

    -- Metadata
    total_episodes INTEGER NOT NULL DEFAULT 0,      -- Total episodes (0=unknown)
    poster_url TEXT,                                -- Poster image URL
    air_date DATE,                                  -- First air date
    air_week INTEGER NOT NULL DEFAULT 0,            -- Air weekday (0=Sunday ~ 6=Saturday)
    tmdb_lookup_at DATETIME,                        -- Last TMDB lookup attempt timestamp
    episode_offset INTEGER NOT NULL DEFAULT 0,      -- Episode offset for download

    -- Subscription state
    current_episode INTEGER NOT NULL DEFAULT 0,     -- Current downloaded episode
    auto_complete INTEGER NOT NULL DEFAULT 1,       -- Only download first matching episode per RSS check (boolean)
    save_path TEXT NOT NULL,                        -- Save path (required)
    source_type TEXT NOT NULL DEFAULT 'webrip'      -- Source type: 'webrip' or 'bdrip'
);

-- Bangumi indexes
CREATE INDEX IF NOT EXISTS idx_bangumi_title_chinese ON bangumi(title_chinese);
CREATE INDEX IF NOT EXISTS idx_bangumi_title_japanese ON bangumi(title_japanese);
CREATE INDEX IF NOT EXISTS idx_bangumi_year ON bangumi(year);
CREATE INDEX IF NOT EXISTS idx_bangumi_season ON bangumi(season);
CREATE INDEX IF NOT EXISTS idx_bangumi_air_week ON bangumi(air_week);

-- Unique indexes for external IDs (ensure uniqueness for non-null values)
CREATE UNIQUE INDEX IF NOT EXISTS idx_bangumi_mikan_id ON bangumi(mikan_id) WHERE mikan_id IS NOT NULL;
CREATE UNIQUE INDEX IF NOT EXISTS idx_bangumi_bgmtv_id ON bangumi(bgmtv_id) WHERE bgmtv_id IS NOT NULL AND bgmtv_id != 0;
CREATE INDEX IF NOT EXISTS idx_bangumi_tmdb_id ON bangumi(tmdb_id) WHERE tmdb_id IS NOT NULL AND tmdb_id != 0;

-- Trigger to update updated_at on row modification
CREATE TRIGGER IF NOT EXISTS update_bangumi_timestamp
AFTER UPDATE ON bangumi
FOR EACH ROW
BEGIN
    UPDATE bangumi SET updated_at = CURRENT_TIMESTAMP WHERE id = OLD.id;
END;

-- RSS subscription table
CREATE TABLE IF NOT EXISTS rss (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    created_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,

    -- Foreign key to bangumi
    bangumi_id INTEGER NOT NULL REFERENCES bangumi(id) ON DELETE CASCADE,

    -- RSS info
    title TEXT NOT NULL,                            -- RSS subscription title: [group] {bangumi} S{season}
    url TEXT NOT NULL,                              -- RSS feed URL
    enabled INTEGER NOT NULL DEFAULT 1,             -- Whether subscription is enabled (boolean)
    exclude_filters TEXT NOT NULL DEFAULT '[]',     -- JSON array of regex patterns to exclude
    include_filters TEXT NOT NULL DEFAULT '[]',     -- JSON array of regex patterns to include
    "subtitle_group" TEXT,                          -- Optional subtitle group name

    -- HTTP caching for incremental updates
    etag TEXT,                                      -- ETag from last HTTP response
    last_modified TEXT,                             -- Last-Modified from last HTTP response
    last_pub_date TEXT                              -- Last processed pubDate (ISO 8601)
);

-- RSS indexes
CREATE INDEX IF NOT EXISTS idx_rss_bangumi_id ON rss(bangumi_id);
CREATE INDEX IF NOT EXISTS idx_rss_title ON rss(title);
CREATE INDEX IF NOT EXISTS idx_rss_enabled ON rss(enabled);

-- Trigger to update updated_at on row modification
CREATE TRIGGER IF NOT EXISTS update_rss_timestamp
AFTER UPDATE ON rss
FOR EACH ROW
BEGIN
    UPDATE rss SET updated_at = CURRENT_TIMESTAMP WHERE id = OLD.id;
END;

-- Torrent table: stores torrent metadata for bangumi episodes
-- Each torrent is identified by its unique info_hash (globally unique)
CREATE TABLE IF NOT EXISTS torrent (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    created_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,

    -- Foreign key to bangumi (required)
    bangumi_id INTEGER NOT NULL REFERENCES bangumi(id) ON DELETE CASCADE,

    -- Optional reference to source RSS (SET NULL when RSS is deleted)
    rss_id INTEGER REFERENCES rss(id) ON DELETE SET NULL,

    -- Torrent identification
    info_hash TEXT NOT NULL,                        -- BitTorrent info hash (40-char hex for v1, 64-char for v2)
    torrent_url TEXT NOT NULL,                      -- Torrent URL (.torrent file URL or magnet link)

    -- Episode number (optional, can be parsed from filename during rename)
    episode_number INTEGER,

    -- Parsed metadata for priority comparison (washing)
    subtitle_group TEXT,                            -- Subtitle group name (e.g., "ANi", "喵萌奶茶屋")
    subtitle_languages TEXT,                        -- Subtitle languages JSON array (e.g., ["CHS", "JPN"])
    resolution TEXT                                 -- Video resolution (e.g., "1080P", "720P")
);

-- Torrent indexes
CREATE INDEX IF NOT EXISTS idx_torrent_bangumi_id ON torrent(bangumi_id);
CREATE INDEX IF NOT EXISTS idx_torrent_rss_id ON torrent(rss_id);
CREATE INDEX IF NOT EXISTS idx_torrent_episode_number ON torrent(episode_number);

-- Unique constraint: info_hash is globally unique (one torrent record per unique hash)
CREATE UNIQUE INDEX IF NOT EXISTS idx_torrent_info_hash ON torrent(info_hash);

-- Composite index for common query: find torrents for a specific bangumi episode
CREATE INDEX IF NOT EXISTS idx_torrent_bangumi_episode ON torrent(bangumi_id, episode_number);

-- Trigger to update updated_at on row modification
CREATE TRIGGER IF NOT EXISTS update_torrent_timestamp
AFTER UPDATE ON torrent
FOR EACH ROW
BEGIN
    UPDATE torrent SET updated_at = CURRENT_TIMESTAMP WHERE id = OLD.id;
END;

-- Generic cache table for external API responses
CREATE TABLE IF NOT EXISTS cache (
    cache_key TEXT PRIMARY KEY,
    data TEXT NOT NULL,
    fetched_at INTEGER NOT NULL  -- Unix timestamp
);

-- Index for cleanup queries
CREATE INDEX IF NOT EXISTS idx_cache_fetched_at ON cache(fetched_at);

-- System logs table for logging and user notifications
CREATE TABLE IF NOT EXISTS log (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    created_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,

    -- Log classification
    level TEXT NOT NULL CHECK(level IN ('info', 'warning', 'error')),

    -- Content
    message TEXT NOT NULL
);

-- Indexes for efficient querying
CREATE INDEX IF NOT EXISTS idx_log_created_at ON log(created_at DESC);
CREATE INDEX IF NOT EXISTS idx_log_level ON log(level);
CREATE INDEX IF NOT EXISTS idx_log_level_created ON log(level, created_at DESC);

-- Calendar table for seasonal anime schedule
-- Links to bangumi table, stores seasonal grouping and display priority
CREATE TABLE IF NOT EXISTS calendar (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    created_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,

    -- Foreign key to bangumi (required)
    bangumi_id INTEGER NOT NULL REFERENCES bangumi(id) ON DELETE CASCADE,

    -- Season identification
    year INTEGER NOT NULL,                      -- Year (e.g., 2024)
    season TEXT NOT NULL CHECK(season IN ('winter', 'spring', 'summer', 'fall')),

    -- Display priority (based on BGM.tv collection_doing)
    priority INTEGER NOT NULL DEFAULT 0
);

-- Unique index: one entry per bangumi per season
CREATE UNIQUE INDEX IF NOT EXISTS idx_calendar_bangumi_season ON calendar(bangumi_id, year, season);

-- Index: query by season
CREATE INDEX IF NOT EXISTS idx_calendar_season ON calendar(year, season);

-- Index: sort by priority
CREATE INDEX IF NOT EXISTS idx_calendar_priority ON calendar(priority DESC);

-- Trigger to update updated_at
CREATE TRIGGER IF NOT EXISTS update_calendar_timestamp
AFTER UPDATE ON calendar
FOR EACH ROW
BEGIN
    UPDATE calendar SET updated_at = CURRENT_TIMESTAMP WHERE id = OLD.id;
END;
