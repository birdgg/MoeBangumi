# File Rename Workflow

This document describes the automatic file renaming system for downloaded torrents.

## Overview

The rename job automatically renames video files in completed torrents. It supports three content types:

1. **Single Episode** - One video file, renamed to torrent name
2. **Collection (合集)** - Multiple episodes in one directory, batch renamed with TMDB metadata
3. **Multi-Season Collection** - Multiple season subdirectories

This creates clean, consistent filenames that work well with media servers like Plex and Jellyfin.

## Content Types

### Single Episode
Standard single-video torrents downloaded via RSS subscription.
```
[SubGroup] Anime - 01.mkv  →  葬送的芙莉莲 S01E01.mkv
```

### Collection (合集)
Batch torrents containing multiple episodes, e.g. BDRip releases.
```
[hyakuhuyu][Re Zero][51-66][BDRip 1080p]/
├── EP51.mkv  →  Re:ゼロから始める異世界生活 S02E51.mkv
├── EP52.mkv  →  Re:ゼロから始める異世界生活 S02E52.mkv
├── SP/
│   └── SP01.mkv  →  Re:ゼロから始める異世界生活 S00E01.mkv  (specials)
```

### Multi-Season Collection
Top-level directory with multiple season subdirectories.
```
[Group][Title]/
├── [Group][Title][S1]/EP01.mkv
├── [Group][Title][S2]/EP01.mkv
```

## Workflow Diagram

```
┌─────────────────────────────────────────────────────────────────┐
│                    File Rename Job (every 30 min)               │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│  1. Query all torrents from qBittorrent                         │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│  2. Filter: has "rename" tag AND progress >= 1.0 (completed)    │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
                    ┌─────────┴─────────┐
                    │  For each torrent │
                    └─────────┬─────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│  3. Get file list and detect content type                       │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
              ┌───────────────┼───────────────┐
              │               │               │
              ▼               ▼               ▼
       ┌──────────┐    ┌──────────┐    ┌──────────────┐
       │  Single  │    │Collection│    │ Multi-Season │
       │ Episode  │    │  (合集)  │    │  Collection  │
       └────┬─────┘    └────┬─────┘    └──────┬───────┘
            │               │                 │
            ▼               ▼                 ▼
┌────────────────┐  ┌────────────────┐  ┌────────────────┐
│ Rename to      │  │ 1. Parse title │  │ Process each   │
│ torrent name   │  │ 2. Search TMDB │  │ season subdir  │
│ + extension    │  │ 3. Create DB   │  │ as Collection  │
└────────────────┘  │ 4. Batch rename│  └────────────────┘
                    └────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│  Remove "rename" tag from torrent                               │
└─────────────────────────────────────────────────────────────────┘
```

## Tag-Based Triggering

The rename system uses qBittorrent tags to identify torrents that need renaming:

### How Tags Are Set

When the RSS fetch job downloads a torrent, it adds two tags:
- `moe` - Identifies torrents managed by moe-bangumi
- `rename` - Marks the torrent for file renaming

```haskell
-- From App.Jobs.RssFetch.Download
options = defaultAddOptions
  { savePath = Just savePath
  , tags = ["moe", "rename"]  -- Tags added on download
  , rename = Just renameTo
  }
```

### Tag Lifecycle

```
Download starts    →  Tags: [moe, rename]
Download completes →  Tags: [moe, rename]  (rename job picks it up)
Rename succeeds    →  Tags: [moe]          (rename tag removed)
```

## Supported Video Extensions

The following file extensions are recognized as video files:

| Extension | Format |
|-----------|--------|
| `.mkv`    | Matroska |
| `.mp4`    | MPEG-4 |
| `.avi`    | AVI |
| `.ts`     | MPEG Transport Stream |
| `.m2ts`   | Blu-ray MPEG-2 TS |
| `.flv`    | Flash Video |
| `.wmv`    | Windows Media Video |
| `.webm`   | WebM |
| `.mov`    | QuickTime |

Files with other extensions (`.srt`, `.ass`, `.nfo`, etc.) are ignored when counting video files.

## Rename Logic

### Path Preservation

The rename operation preserves the directory structure within the torrent:

```
Original: "Season 1/[SubGroup] Episode 05.mkv"
Result:   "Season 1/葬送的芙莉莲 s01e05.mkv"

Original: "[SubGroup] Episode 05.mkv"  (no directory)
Result:   "葬送的芙莉莲 s01e05.mkv"
```

### Naming Convention

The new filename follows this pattern:
```
{torrent_name}.{original_extension}
```

The torrent name is set during download using `formatRename`:
```haskell
formatRename :: Text -> Int -> Int -> Text
formatRename titleChinese season episode =
  titleChinese <> " " <> printf "s%02de%02d" season episode

-- Example: formatRename "葬送的芙莉莲" 1 5 → "葬送的芙莉莲 s01e05"
```

## Special Directory Detection

The following directories are recognized as "specials" and renamed to Season 0 (S00):

| Directory | Description |
|-----------|-------------|
| `SP`, `Special`, `Specials` | Special episodes |
| `OVA`, `OVAs` | Original Video Animation |
| `OAD`, `OADs` | Original Animation DVD |
| `Extra`, `Extras`, `Bonus` | Bonus content |
| `PV`, `PVs`, `CM` | Promotional videos |
| `NC`, `NCOP`, `NCED` | Creditless OP/ED |
| `Menu` | DVD/BD menus |

## Skip Conditions

The rename job skips a torrent if any of these conditions are true:

### Single Episode Mode
| Condition | Reason |
|-----------|--------|
| No "rename" tag | Not marked for renaming |
| progress < 1.0 | Download not complete |
| 0 video files | Nothing to rename |
| Already renamed | `oldPath == newPath` |

### Collection Mode (Batch)
| Condition | Reason |
|-----------|--------|
| Failed to parse title | Cannot extract title from torrent name |
| No TMDB match | Cannot find metadata for title |
| Failed to create Bangumi | Database operation failed |
| Cannot extract episode number | Filename doesn't contain episode info |

## Error Handling

### Rename Failure

If the rename operation fails:
- Error is logged with torrent hash and error message
- "rename" tag is NOT removed (will retry next cycle)
- Other torrents continue to be processed

### Tag Removal Failure

If rename succeeds but tag removal fails:
- Warning is logged
- Next cycle will skip rename (already renamed check)
- Will retry tag removal each cycle

## Code Structure

```
src/App/Jobs/Rename/
├── Job.hs      # Job definition, effect stack setup
├── Rename.hs   # Core rename logic
└── ../Rename.hs  # Module re-exports

src/Moe/Parsing/
└── Collection.hs  # Collection parsing (content type detection, season/episode extraction)
```

### Key Functions

| Function | Location | Description |
|----------|----------|-------------|
| `renameJob` | `Job.hs` | Job entry point, runs every 30 min |
| `renameAllTaggedTorrents` | `Rename.hs` | Main orchestration logic |
| `renameSingleTorrent` | `Rename.hs` | Process one torrent (routes to single or batch) |
| `performRename` | `Rename.hs` | Single episode rename + remove tag |
| `performBatchRename` | `Rename.hs` | Collection batch rename (TMDB + DB) |
| `searchTmdbForTitle` | `Rename.hs` | Search TMDB for metadata |
| `createBangumiFromTmdb` | `Rename.hs` | Create Bangumi record in database |
| `renameCollectionFiles` | `Rename.hs` | Rename all files in collection |
| `buildEpisodePath` | `Rename.hs` | Format `{Title} S{season}E{episode}` |
| `detectContentType` | `Collection.hs` | Detect Single/Collection/MultiSeason |
| `extractSeasonFromPath` | `Collection.hs` | Extract season from path (S1, Season 1, etc.) |
| `isSpecialDirectory` | `Collection.hs` | Check if directory is SP/OVA/etc. |

## Effect Dependencies

The rename job uses these effects:

```haskell
renameAllTaggedTorrents
  :: ( Downloader :> es       -- qBittorrent operations
     , Log :> es              -- Logging
     , IOE :> es              -- IO operations
     , Reader MoeEnv :> es    -- Environment (for TMDB access)
     , DB :> es               -- Database (for Bangumi creation)
     , Concurrent :> es       -- Concurrent operations
     )
  => Eff es ()
```

Effect stack assembled in `runRenameAction`:
```haskell
renameAllTaggedTorrents
  & Reader.runReader env                              -- Environment
  & runDBPool env.pool                                -- Database
  & runDownloader manager settings                    -- qBittorrent
  & runLog environment logger                         -- Logging
  & runConcurrent                                     -- Concurrency
  & runEff
```

## Batch Rename (Collection) Workflow

When multiple video files are detected, the batch rename workflow is triggered:

### 1. Parse Collection Info
```haskell
-- Extract from torrent name like "[hyakuhuyu][Re Zero][51-66][BDRip]"
parseCollectionInfo :: Text -> [Text] -> CollectionInfo
-- Returns: title, season, episode range, content type
```

### 2. Search TMDB
```haskell
searchTmdbForTitle :: Text -> Eff es (Maybe TmdbSearchItem)
-- Searches TMDB TV shows API for metadata
```

### 3. Create Bangumi Record
```haskell
createBangumiFromTmdb :: Text -> TmdbSearchItem -> Int -> Text -> Eff es (Maybe Bangumi)
-- Creates database entry with:
--   - Title from parsed name
--   - TMDB ID, poster URL, air date from TMDB
--   - Season from parsed directory
--   - subscribed = False (batch imports don't auto-subscribe)
```

### 4. Rename Each File
For each video file:
1. Detect if in SP/OVA directory → use Season 0
2. Extract episode number from filename
3. Build new path: `{Title} S{season}E{episode}.{ext}`
4. Rename video file
5. Rename associated subtitle files (same directory)

### Example
```
Torrent: [hyakuhuyu][Re Zero][51-66][BDRip 1080p AVC AAC][CHS]

Before:
├── EP51.mkv
├── EP51.chs.ass
├── EP52.mkv
└── SP/
    └── SP01.mkv

After:
├── Re:ゼロから始める異世界生活 S02E51.mkv
├── Re:ゼロから始める異世界生活 S02E51.chs.ass
├── Re:ゼロから始める異世界生活 S02E52.mkv
└── SP/
    └── Re:ゼロから始める異世界生活 S00E01.mkv
```

## Configuration

Currently, the rename job uses hardcoded values:

| Setting | Value | Location |
|---------|-------|----------|
| Interval | 30 minutes | `Job.hs` |
| Target tag | "rename" | `Rename.hs` |
| Video extensions | See table above | `Rename.hs` |

## Logging

The job produces logs at various levels:

| Level | Event |
|-------|-------|
| INFO | Job start, torrents found, rename success, Bangumi created, collection completed |
| TRACE | Processing details, skip reasons, parsed collection info |
| ATTENTION | Failures (get files, rename, tag removal, TMDB search, parse failure) |

Example log output (single episode):
```
[file-rename] Starting file rename job
[file-rename] Found torrents for renaming {"tagged_count":3,"completed_count":2}
[file-rename] Renaming file {"hash":"abc123","old_path":"video.mkv","new_path":"葬送的芙莉莲 S01E05.mkv"}
[file-rename] Video rename succeeded {"hash":"abc123"}
```

Example log output (collection):
```
[file-rename] Multiple video files found, attempting batch rename {"hash":"def456","count":16}
[file-rename] Parsed collection info {"content_type":"Collection","title":"Re Zero","season":2}
[file-rename] Created Bangumi from TMDB {"bangumi_id":42,"title":"Re:ゼロから始める異世界生活","season":2}
[file-rename] Renaming collection file {"old_path":"EP51.mkv","new_path":"Re:ゼロから始める異世界生活 S02E51.mkv"}
[file-rename] Collection rename completed {"total_files":16,"success_count":16}
```

## Troubleshooting

### Torrent not being renamed

1. Check if torrent has "rename" tag in qBittorrent
2. Check if download is complete (100%)
3. Check logs for skip reasons

### Rename fails repeatedly

1. Check qBittorrent connectivity
2. Verify file permissions
3. Check if target filename already exists
4. Review error message in logs

### Tag not being removed

1. Check qBittorrent API connectivity
2. Verify user has permission to modify tags
3. Check if job completed successfully (not interrupted)

### Collection not being renamed

1. **"Failed to parse title"** - Torrent name doesn't match expected format
   - Ensure torrent name contains title in brackets, e.g. `[Group][Title][01-12]`
2. **"No TMDB match found"** - Title not found in TMDB
   - Check TMDB API key is configured
   - Try searching manually on themoviedb.org to verify title
3. **"Could not extract episode number"** - Filename doesn't contain episode info
   - Files should contain episode numbers like `EP01`, `01`, `E01`, etc.
4. **Files in SP directory not renamed** - Season detection issue
   - SP/OVA directories should use standard names (SP, Special, OVA, etc.)

### Subtitle files not renamed

1. Check if subtitle extension is supported (.ass, .srt, .ssa, .sub, .vtt)
2. Verify subtitle is in same directory as video file
3. Check if language tag is recognized (.chs, .cht, .en, etc.)
