-- Bangumi (番剧) main table
CREATE TABLE IF NOT EXISTS bangumi (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    created_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,

    -- Basic info - bilingual title support
    title_chinese TEXT NOT NULL,                    -- Chinese title (primary display)
    title_japanese TEXT,                            -- Japanese original name
    title_original_chinese TEXT NOT NULL DEFAULT '',-- Original Chinese title (native language, required, unique)
    title_original_japanese TEXT,                   -- Original Japanese title
    season INTEGER NOT NULL DEFAULT 1,              -- Season number
    year INTEGER NOT NULL,                          -- Year

    -- External metadata IDs
    bgmtv_id INTEGER,                               -- Bangumi.tv ID
    tmdb_id INTEGER,                                -- TMDB ID

    -- Metadata
    poster_url TEXT,                                -- Poster URL
    air_date DATE NOT NULL,                         -- First air date (required)
    air_week INTEGER NOT NULL,                      -- Air weekday (0=Sunday, 1=Monday, ..., 6=Saturday, required)
    total_episodes INTEGER NOT NULL DEFAULT 0,      -- Total episodes (0=unknown)
    episode_offset INTEGER NOT NULL DEFAULT 0,      -- Episode offset
    platform TEXT NOT NULL DEFAULT 'tv' CHECK(platform IN ('tv', 'movie', 'ova')),  -- Platform type: tv, movie, ova

    -- Status management
    current_episode INTEGER NOT NULL DEFAULT 0,     -- Current downloaded episode
    auto_download INTEGER NOT NULL DEFAULT 1,       -- Auto download new episodes (boolean)
    finished INTEGER NOT NULL DEFAULT 0,            -- Whether bangumi has completed airing

    -- Path configuration
    save_path TEXT NOT NULL,                        -- Save path (required)

    -- Source type
    source_type TEXT NOT NULL DEFAULT 'webrip'      -- Source type: 'webrip' or 'bdrip'
);

-- Bangumi indexes
CREATE INDEX IF NOT EXISTS idx_bangumi_title_chinese ON bangumi(title_chinese);
CREATE INDEX IF NOT EXISTS idx_bangumi_title_japanese ON bangumi(title_japanese);
CREATE INDEX IF NOT EXISTS idx_bangumi_title_original_chinese ON bangumi(title_original_chinese);
CREATE UNIQUE INDEX IF NOT EXISTS idx_bangumi_title_original_chinese_unique ON bangumi(title_original_chinese);
CREATE INDEX IF NOT EXISTS idx_bangumi_title_original_japanese ON bangumi(title_original_japanese);
CREATE INDEX IF NOT EXISTS idx_bangumi_season ON bangumi(season);
CREATE INDEX IF NOT EXISTS idx_bangumi_year ON bangumi(year);
CREATE INDEX IF NOT EXISTS idx_bangumi_air_date ON bangumi(air_date);

-- Unique indexes for non-zero external IDs
CREATE UNIQUE INDEX IF NOT EXISTS idx_bangumi_bgmtv_id ON bangumi(bgmtv_id) WHERE bgmtv_id IS NOT NULL AND bgmtv_id != 0;

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
    url TEXT NOT NULL,                              -- RSS feed URL
    enabled INTEGER NOT NULL DEFAULT 1,             -- Whether subscription is enabled (boolean)
    exclude_filters TEXT NOT NULL DEFAULT '[]',     -- JSON array of regex patterns to exclude
    include_filters TEXT NOT NULL DEFAULT '[]',     -- JSON array of regex patterns to include (AND logic)
    is_primary INTEGER NOT NULL DEFAULT 0,          -- Primary RSS flag (only one per bangumi)
    "group" TEXT                                    -- Optional subtitle group name
);

-- RSS indexes
CREATE INDEX IF NOT EXISTS idx_rss_bangumi_id ON rss(bangumi_id);
CREATE INDEX IF NOT EXISTS idx_rss_enabled ON rss(enabled);
CREATE INDEX IF NOT EXISTS idx_rss_is_primary ON rss(is_primary);

-- Unique partial index to ensure only one primary RSS per bangumi
CREATE UNIQUE INDEX IF NOT EXISTS idx_rss_bangumi_primary
ON rss(bangumi_id) WHERE is_primary = 1;

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

    -- Torrent kind: 'episode' (single episode) or 'collection' (batch/season pack)
    kind TEXT NOT NULL DEFAULT 'episode' CHECK(kind IN ('episode', 'collection')),

    -- Episode number (required for 'episode' kind, NULL for 'collection')
    episode_number INTEGER,

    -- Constraint: episode kind must have episode_number
    CHECK(
        (kind = 'episode' AND episode_number IS NOT NULL) OR
        (kind = 'collection')
    )
);

-- Torrent indexes
CREATE INDEX IF NOT EXISTS idx_torrent_bangumi_id ON torrent(bangumi_id);
CREATE INDEX IF NOT EXISTS idx_torrent_rss_id ON torrent(rss_id);
CREATE INDEX IF NOT EXISTS idx_torrent_kind ON torrent(kind);
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

-- Download task table: tracks download jobs and their status
-- Keeps full history of downloads (completed/failed tasks are retained)
CREATE TABLE IF NOT EXISTS download_task (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    created_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,

    -- Foreign key to torrent (required)
    torrent_id INTEGER NOT NULL REFERENCES torrent(id) ON DELETE CASCADE,

    -- Task status: pending, paused, downloading, completed, failed
    status TEXT NOT NULL DEFAULT 'pending' CHECK(status IN ('pending', 'paused', 'downloading', 'completed', 'failed')),

    -- Error message for failed tasks
    error_message TEXT
);

-- Download task indexes
CREATE INDEX IF NOT EXISTS idx_download_task_torrent_id ON download_task(torrent_id);
CREATE INDEX IF NOT EXISTS idx_download_task_status ON download_task(status);
CREATE INDEX IF NOT EXISTS idx_download_task_created_at ON download_task(created_at);

-- Composite index for common query: find tasks by status ordered by creation time
CREATE INDEX IF NOT EXISTS idx_download_task_status_created ON download_task(status, created_at);

-- Trigger to update updated_at on row modification
CREATE TRIGGER IF NOT EXISTS update_download_task_timestamp
AFTER UPDATE ON download_task
FOR EACH ROW
BEGIN
    UPDATE download_task SET updated_at = CURRENT_TIMESTAMP WHERE id = OLD.id;
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
