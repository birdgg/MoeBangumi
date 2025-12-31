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
    auto_complete INTEGER NOT NULL DEFAULT 1,       -- Only download first matching episode per RSS check (boolean)
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
    title TEXT NOT NULL,                            -- RSS subscription title: [group] {bangumi} S{season}
    url TEXT NOT NULL,                              -- RSS feed URL
    enabled INTEGER NOT NULL DEFAULT 1,             -- Whether subscription is enabled (boolean)
    exclude_filters TEXT NOT NULL DEFAULT '[]',     -- JSON array of regex patterns to exclude
    is_primary INTEGER NOT NULL DEFAULT 0,          -- Primary RSS flag (only one per bangumi)
    "group" TEXT,                                   -- Optional subtitle group name

    -- HTTP caching for incremental updates
    etag TEXT,                                      -- ETag from last HTTP response
    last_modified TEXT,                             -- Last-Modified from last HTTP response
    last_pub_date TEXT                              -- Last processed pubDate (ISO 8601)
);

-- RSS indexes
CREATE INDEX IF NOT EXISTS idx_rss_bangumi_id ON rss(bangumi_id);
CREATE INDEX IF NOT EXISTS idx_rss_title ON rss(title);
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

    -- Episode number (optional, can be parsed from filename during rename)
    episode_number INTEGER,

    -- Parsed metadata for priority comparison (washing)
    subtitle_group TEXT,                            -- Subtitle group name (e.g., "ANi", "喵萌奶茶屋")
    subtitle_language TEXT,                         -- Subtitle language/type (e.g., "简日", "繁体")
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

-- BGM.tv Calendar table for weekly anime schedule
-- Stores the latest snapshot, refreshed daily by CalendarRefreshJob
CREATE TABLE IF NOT EXISTS calendar_subject (
    -- Primary key: BGM.tv subject ID (unique identifier)
    bgmtv_id INTEGER PRIMARY KEY,

    -- Basic info
    subject_type INTEGER NOT NULL,              -- Subject type: 1=Book, 2=Anime, 3=Music, 4=Game, 6=Real
    name TEXT NOT NULL,                         -- Original name
    name_cn TEXT NOT NULL,                      -- Chinese name
    summary TEXT NOT NULL DEFAULT '',           -- Summary description

    -- Air info
    air_date TEXT NOT NULL,                     -- First air date (YYYY-MM-DD)
    air_weekday INTEGER NOT NULL,               -- Air weekday: 1=Monday ~ 7=Sunday

    -- Rating stats
    rating_total INTEGER,                       -- Total ratings count
    rating_score REAL,                          -- Rating score (0-10)
    rank INTEGER,                               -- Ranking

    -- Collection stats
    collection_doing INTEGER NOT NULL DEFAULT 0,-- Number of users currently watching

    -- Images (JSON string of SubjectImages struct)
    images_json TEXT NOT NULL,                  -- {"small":"...","grid":"...","large":"...","medium":"...","common":"..."}

    -- Last update time
    updated_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP
);

-- Index: query by weekday (main query scenario)
CREATE INDEX IF NOT EXISTS idx_calendar_air_weekday ON calendar_subject(air_weekday);

-- Index: sort by collection count
CREATE INDEX IF NOT EXISTS idx_calendar_collection ON calendar_subject(collection_doing DESC);

-- Trigger to update updated_at
CREATE TRIGGER IF NOT EXISTS update_calendar_subject_timestamp
AFTER UPDATE ON calendar_subject
FOR EACH ROW
BEGIN
    UPDATE calendar_subject SET updated_at = CURRENT_TIMESTAMP WHERE bgmtv_id = OLD.bgmtv_id;
END;

-- Mikan to BGM.tv/TMDB ID mapping table
-- Maps Mikan bangumi IDs to external service IDs for quick lookup
CREATE TABLE IF NOT EXISTS mikan_bgmtv_mapping (
    -- Primary key: Mikan bangumi ID (unique identifier on mikanani.me)
    mikan_id TEXT PRIMARY KEY,

    -- External service IDs
    bgmtv_id INTEGER NOT NULL,          -- BGM.tv subject ID (required, scraped from Mikan)
    tmdb_id INTEGER,                    -- TMDB ID (optional, updated when adding bangumi)

    -- Timestamp
    created_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP
);

-- Index: query by BGM.tv ID (main use case)
CREATE INDEX IF NOT EXISTS idx_mikan_bgmtv_mapping_bgmtv_id
    ON mikan_bgmtv_mapping(bgmtv_id);

-- Index: query by TMDB ID
CREATE INDEX IF NOT EXISTS idx_mikan_bgmtv_mapping_tmdb_id
    ON mikan_bgmtv_mapping(tmdb_id);
