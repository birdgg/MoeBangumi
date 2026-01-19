# Database Schema

This document describes the database table design for moe-bangumi.

## Core Tables

### series

Groups related bangumi (e.g., Attack on Titan S1/S2/S3/Movie).

| Column | Type | Description |
|--------|------|-------------|
| id | INTEGER | Primary key |
| created_at | TIMESTAMP | Creation timestamp |
| updated_at | TIMESTAMP | Last update timestamp |
| title_chinese | TEXT | Chinese title (required) |
| title_japanese | TEXT | Japanese title |
| poster_url | TEXT | Poster image URL |

### bangumi

Anime metadata - what an anime IS.

| Column | Type | Description |
|--------|------|-------------|
| id | INTEGER | Primary key |
| created_at | TIMESTAMP | Creation timestamp |
| updated_at | TIMESTAMP | Last update timestamp |
| mikan_id | TEXT | Mikan ID |
| bgmtv_id | INTEGER | BGM.tv subject ID |
| tmdb_id | INTEGER | TMDB ID |
| title_chinese | TEXT | Chinese title (required) |
| title_japanese | TEXT | Japanese title |
| season | INTEGER | Season number (default: 1) |
| platform | TEXT | 'tv', 'movie', or 'ova' |
| total_episodes | INTEGER | Total episode count (default: 0) |
| poster_url | TEXT | Poster image URL |
| air_date | TEXT | First air date (YYYY-MM-DD) |
| air_week | INTEGER | Day of week (0=Sun, 6=Sat, default: 0) |
| tmdb_lookup_at | TIMESTAMP | Last TMDB lookup timestamp |
| series_id | INTEGER | FK to series (nullable) |

### subscription

User's tracking state - MY tracking progress.

| Column | Type | Description |
|--------|------|-------------|
| id | INTEGER | Primary key |
| created_at | TIMESTAMP | Creation timestamp |
| updated_at | TIMESTAMP | Last update timestamp |
| bangumi_id | INTEGER | FK to bangumi (unique, required) |
| current_episode | INTEGER | Current downloaded episode (default: 0) |
| auto_complete | INTEGER | Download all episodes when true (0/1, default: 1) |
| save_path | TEXT | Download save path |
| source_type | TEXT | 'bdrip' or 'other' (default: 'other') |
| episode_offset | INTEGER | Episode number offset (default: 0) |
| emby_id | TEXT | Emby item ID |

### rss

RSS feed subscriptions.

| Column | Type | Description |
|--------|------|-------------|
| id | INTEGER | Primary key |
| created_at | TIMESTAMP | Creation timestamp |
| updated_at | TIMESTAMP | Last update timestamp |
| subscription_id | INTEGER | FK to subscription (required) |
| url | TEXT | RSS feed URL (required) |
| enabled | INTEGER | Active status (0/1, default: 1) |
| last_processed_pub_date | TEXT | RFC 822 date string for time-based filtering |

### torrent

Downloaded torrent records.

| Column | Type | Description |
|--------|------|-------------|
| id | INTEGER | Primary key |
| created_at | TIMESTAMP | Creation timestamp |
| updated_at | TIMESTAMP | Last update timestamp |
| subscription_id | INTEGER | FK to subscription (required) |
| rss_id | INTEGER | FK to rss (nullable) |
| info_hash | TEXT | Torrent info hash (unique, required) |
| torrent_url | TEXT | Magnet link or torrent URL (required) |
| episode_number | INTEGER | Episode number |
| group | TEXT | Subtitle group name |
| languages | TEXT | JSON array of subtitles (e.g., ["chs","cht","jap"]) |

### calendar

Seasonal bangumi from Mikan.

| Column | Type | Description |
|--------|------|-------------|
| id | INTEGER | Primary key |
| created_at | TIMESTAMP | Creation timestamp |
| updated_at | TIMESTAMP | Last update timestamp |
| year | INTEGER | Year (e.g., 2025) |
| season | TEXT | 'winter', 'spring', 'summer', 'fall' |
| bangumi_id | INTEGER | FK to bangumi (required) |

## Relationships

```
series (optional)
    │
    ▼
bangumi ◄──── calendar
    │
    ▼
subscription
    │
    ├──► rss
    │
    └──► torrent
```

## Indexes

| Table | Index | Columns | Type |
|-------|-------|---------|------|
| bangumi | idx_bangumi_series_id | series_id | Index |
| bangumi | idx_bangumi_air_week | air_week | Index |
| bangumi | idx_bangumi_mikan_id | mikan_id | Unique (where not null) |
| subscription | idx_subscription_bangumi_id | bangumi_id | Unique |
| rss | idx_rss_subscription_id | subscription_id | Index |
| rss | idx_rss_enabled | enabled | Index |
| torrent | idx_torrent_subscription_id | subscription_id | Index |
| torrent | idx_torrent_rss_id | rss_id | Index |
| torrent | idx_torrent_info_hash | info_hash | Unique |
| calendar | idx_calendar_year_season | year, season | Index |
| calendar | idx_calendar_unique | year, season, bangumi_id | Unique |

## Design Rationale

**Why separate bangumi and subscription?**

- Calendar sync creates bangumi as metadata only
- Users explicitly subscribe to track anime
- Allows browsing anime without tracking them
- Clean separation of "what it is" vs "my progress"
