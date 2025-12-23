-- Bangumi (番剧) main table
CREATE TABLE IF NOT EXISTS bangumi (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    created_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,

    -- Basic info - bilingual title support
    title_chinese TEXT NOT NULL,                    -- Chinese title (primary display)
    title_japanese TEXT,                            -- Japanese original name
    season INTEGER NOT NULL DEFAULT 1,              -- Season number
    year INTEGER NOT NULL,                          -- Year

    -- External metadata IDs
    bgmtv_id INTEGER,                               -- Bangumi.tv ID
    tmdb_id INTEGER,                                -- TMDB ID

    -- Metadata
    poster_url TEXT,                                -- Poster URL
    air_date DATE,                                  -- First air date
    air_week INTEGER,                               -- Air weekday (0=Sunday, 1=Monday, ..., 6=Saturday)
    total_episodes INTEGER NOT NULL DEFAULT 0,      -- Total episodes (0=unknown)
    episode_offset INTEGER NOT NULL DEFAULT 0,      -- Episode offset
    kind TEXT DEFAULT 'TV',                         -- Type: TV, Movie, OVA, etc.

    -- Status management
    current_episode INTEGER NOT NULL DEFAULT 0,     -- Current downloaded episode
    auto_download INTEGER NOT NULL DEFAULT 1,       -- Auto download new episodes (boolean)
    finished INTEGER NOT NULL DEFAULT 0,            -- Whether bangumi has completed airing

    -- Path configuration
    save_path TEXT,                                 -- Custom save path (empty=use default)

    -- Source type
    source_type TEXT NOT NULL DEFAULT 'webrip'      -- Source type: 'webrip' or 'bdrip'
);

-- Bangumi indexes
CREATE INDEX IF NOT EXISTS idx_bangumi_title_chinese ON bangumi(title_chinese);
CREATE INDEX IF NOT EXISTS idx_bangumi_title_japanese ON bangumi(title_japanese);
CREATE INDEX IF NOT EXISTS idx_bangumi_season ON bangumi(season);
CREATE INDEX IF NOT EXISTS idx_bangumi_year ON bangumi(year);
CREATE INDEX IF NOT EXISTS idx_bangumi_air_date ON bangumi(air_date);

-- Unique indexes for non-zero external IDs
CREATE UNIQUE INDEX IF NOT EXISTS idx_bangumi_bgmtv_id ON bangumi(bgmtv_id) WHERE bgmtv_id IS NOT NULL AND bgmtv_id != 0;
CREATE UNIQUE INDEX IF NOT EXISTS idx_bangumi_tmdb_id ON bangumi(tmdb_id) WHERE tmdb_id IS NOT NULL AND tmdb_id != 0;

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
    title TEXT NOT NULL,                            -- RSS subscription title
    url TEXT NOT NULL,                              -- RSS feed URL
    save_path TEXT NOT NULL,                        -- Download save path
    enabled INTEGER NOT NULL DEFAULT 1,             -- Whether subscription is enabled (boolean)
    exclude_filters TEXT NOT NULL DEFAULT '[]'      -- JSON array of regex patterns to exclude
);

-- RSS indexes
CREATE INDEX IF NOT EXISTS idx_rss_bangumi_id ON rss(bangumi_id);
CREATE INDEX IF NOT EXISTS idx_rss_enabled ON rss(enabled);

-- Trigger to update updated_at on row modification
CREATE TRIGGER IF NOT EXISTS update_rss_timestamp
AFTER UPDATE ON rss
FOR EACH ROW
BEGIN
    UPDATE rss SET updated_at = CURRENT_TIMESTAMP WHERE id = OLD.id;
END;
