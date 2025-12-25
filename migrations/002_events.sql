-- System events table for logging and user notifications
CREATE TABLE IF NOT EXISTS event (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    created_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,

    -- Event classification
    level TEXT NOT NULL CHECK(level IN ('info', 'warning', 'error')),

    -- Content
    message TEXT NOT NULL,
    details TEXT              -- Optional details (e.g., error stack trace)
);

-- Indexes for efficient querying
CREATE INDEX IF NOT EXISTS idx_event_created_at ON event(created_at DESC);
CREATE INDEX IF NOT EXISTS idx_event_level ON event(level);
CREATE INDEX IF NOT EXISTS idx_event_level_created ON event(level, created_at DESC);
