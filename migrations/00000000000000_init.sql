-- Initial database schema for Moe Bangumi
-- Creates all core tables: series, bangumi, subscription, rss, torrent, calendar

--------------------------------------------------------------------------------
-- Tables
--------------------------------------------------------------------------------

-- Series: Groups related bangumi (e.g., Attack on Titan S1/S2/S3/Movie)
CREATE TABLE series (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
  title_chinese TEXT NOT NULL,
  title_japanese TEXT,
  poster_url TEXT
);

-- Bangumi: Anime metadata (what an anime IS)
CREATE TABLE bangumi (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
  -- External IDs
  mikan_id TEXT,
  bgmtv_id INTEGER,
  tmdb_id INTEGER,
  -- Titles
  title_chinese TEXT NOT NULL,
  title_japanese TEXT,
  -- Metadata
  season INTEGER NOT NULL DEFAULT 1,
  platform TEXT NOT NULL DEFAULT 'tv',  -- 'tv', 'movie', 'ova'
  total_episodes INTEGER NOT NULL DEFAULT 0,
  poster_url TEXT,
  air_date TEXT,  -- YYYY-MM-DD format
  air_week INTEGER NOT NULL DEFAULT 0,  -- 0=Sunday ~ 6=Saturday
  tmdb_lookup_at TIMESTAMP,
  -- Relations
  series_id INTEGER,
  FOREIGN KEY (series_id) REFERENCES series(id) ON DELETE SET NULL
);

-- Subscription: User's tracking state for a bangumi (MY tracking progress)
CREATE TABLE subscription (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
  bangumi_id INTEGER NOT NULL UNIQUE,  -- One subscription per bangumi
  -- Tracking
  current_episode INTEGER NOT NULL DEFAULT 0,
  auto_complete INTEGER NOT NULL DEFAULT 1,  -- Boolean: 0/1, default to download all episodes
  save_path TEXT,
  source_type TEXT NOT NULL DEFAULT 'other',  -- 'bdrip', 'other'
  episode_offset INTEGER NOT NULL DEFAULT 0,
  emby_id TEXT,
  FOREIGN KEY (bangumi_id) REFERENCES bangumi(id) ON DELETE CASCADE
);

-- RSS: RSS feed subscriptions linked to subscription (tracking logic)
CREATE TABLE rss (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
  subscription_id INTEGER NOT NULL,
  url TEXT NOT NULL,
  enabled INTEGER NOT NULL DEFAULT 1,  -- Boolean: 0/1
  last_processed_pub_date TEXT,  -- RFC 822 date string for time-based filtering
  FOREIGN KEY (subscription_id) REFERENCES subscription(id) ON DELETE CASCADE
);

-- Torrent: Downloaded torrent metadata (single-episode from RSS)
CREATE TABLE torrent (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
  subscription_id INTEGER NOT NULL,  -- Single FK to subscription
  rss_id INTEGER,
  info_hash TEXT NOT NULL,
  torrent_url TEXT NOT NULL,
  episode_number INTEGER,
  "group" TEXT,  -- Subtitle group name (quoted: SQL reserved word)
  languages TEXT NOT NULL DEFAULT '[]',  -- JSON array: ["chs","cht","jap"]
  FOREIGN KEY (subscription_id) REFERENCES subscription(id) ON DELETE CASCADE,
  FOREIGN KEY (rss_id) REFERENCES rss(id) ON DELETE SET NULL
);

-- Calendar: Seasonal bangumi from Mikan
CREATE TABLE calendar (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
  year INTEGER NOT NULL,  -- e.g., 2025
  season TEXT NOT NULL,   -- 'winter', 'spring', 'summer', 'fall'
  bangumi_id INTEGER NOT NULL,
  FOREIGN KEY (bangumi_id) REFERENCES bangumi(id) ON DELETE CASCADE
);

--------------------------------------------------------------------------------
-- Indexes
--------------------------------------------------------------------------------

-- Bangumi indexes
CREATE INDEX idx_bangumi_series_id ON bangumi(series_id);
CREATE INDEX idx_bangumi_air_week ON bangumi(air_week);
CREATE UNIQUE INDEX idx_bangumi_mikan_id ON bangumi(mikan_id) WHERE mikan_id IS NOT NULL;

-- Subscription indexes
CREATE UNIQUE INDEX idx_subscription_bangumi_id ON subscription(bangumi_id);

-- RSS indexes
CREATE INDEX idx_rss_subscription_id ON rss(subscription_id);
CREATE INDEX idx_rss_enabled ON rss(enabled);

-- Torrent indexes
CREATE INDEX idx_torrent_subscription_id ON torrent(subscription_id);
CREATE INDEX idx_torrent_rss_id ON torrent(rss_id);
CREATE UNIQUE INDEX idx_torrent_info_hash ON torrent(info_hash);

-- Calendar indexes
CREATE INDEX idx_calendar_year_season ON calendar(year, season);
CREATE UNIQUE INDEX idx_calendar_unique ON calendar(year, season, bangumi_id);
