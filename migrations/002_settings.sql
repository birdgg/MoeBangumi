-- Settings table (singleton pattern - only one row with id=1)
CREATE TABLE IF NOT EXISTS settings (
    id INTEGER PRIMARY KEY CHECK (id = 1),  -- Enforce singleton
    created_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,

    -- qBittorrent downloader configuration
    qb_url TEXT,                                -- qBittorrent Web UI URL (e.g., http://localhost:8080)
    qb_username TEXT,                           -- qBittorrent username
    qb_password TEXT,                           -- qBittorrent password (plain text)

    -- Global RSS filters (stored as JSON array of regex patterns)
    global_rss_filters TEXT NOT NULL DEFAULT '[]'
);

-- Initialize with default row
INSERT OR IGNORE INTO settings (id) VALUES (1);

-- Trigger to update updated_at on row modification
CREATE TRIGGER IF NOT EXISTS update_settings_timestamp
AFTER UPDATE ON settings
FOR EACH ROW
BEGIN
    UPDATE settings SET updated_at = CURRENT_TIMESTAMP WHERE id = OLD.id;
END;
