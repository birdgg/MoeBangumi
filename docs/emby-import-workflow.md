# Emby Import Workflow

This document describes the Emby media library import system for moe-bangumi.

## Overview

The Emby import feature allows you to import existing anime from your Emby media server into moe-bangumi. This is useful for:

1. **Initial Setup** - Quickly populate moe-bangumi with your existing anime collection
2. **Syncing** - Keep moe-bangumi aware of anime added through other means
3. **Metadata Reuse** - Leverage Emby's TMDB metadata for poster URLs and air dates

Imported anime are created with `subscribed = False` and won't trigger RSS downloads automatically.

## Import Strategy

The import process follows this strategy:

1. **Get TV Libraries** - Query Emby for all virtual folders (libraries)
2. **Filter TV Shows** - Keep only libraries with `collectionType = "tvshows"` or `"mixed"`
3. **Get Series Items** - For each TV library, get all Series items
4. **Deduplication** - For each Series, check if it already exists (by `embyId`)
5. **Create Bangumi** - If not exists, create a new Bangumi record

## Workflow Diagram

```
┌─────────────────────────────────────────────────────────────────┐
│                    Emby Import Workflow                          │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│  1. GET /Library/VirtualFolders (Emby API)                      │
│     - Retrieves all media libraries                             │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│  2. Filter libraries: collectionType = "tvshows" or "mixed"     │
│     - If libraryIds provided: use only specified libraries      │
│     - Otherwise: use all TV libraries                           │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
                    ┌─────────┴─────────┐
                    │ For each TV library│
                    └─────────┬─────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│  3. GET /Items (Emby API)                                       │
│     - ParentId = library.itemId                                 │
│     - IncludeItemTypes = "Series"                               │
│     - Recursive = true                                          │
│     - Fields = ProviderIds,Overview,PremiereDate,ChildCount...  │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
                    ┌─────────┴─────────┐
                    │ For each Series   │
                    └─────────┬─────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│  4. Check: SELECT * FROM bangumi WHERE emby_id = ?              │
└─────────────────────────────────────────────────────────────────┘
                              │
              ┌───────────────┴───────────────┐
              │                               │
              ▼                               ▼
       ┌──────────┐                    ┌──────────┐
       │  Exists  │                    │ Not Found│
       └────┬─────┘                    └────┬─────┘
            │                               │
            ▼                               ▼
     ┌────────────┐              ┌────────────────────┐
     │   SKIP     │              │ Create Bangumi     │
     │ (already   │              │ - title = name     │
     │  imported) │              │ - embyId = id      │
     └────────────┘              │ - tmdbId (if avail)│
                                 │ - posterUrl        │
                                 │ - airDate          │
                                 │ - totalEpisodes    │
                                 └────────────────────┘
                                          │
                                          ▼
┌─────────────────────────────────────────────────────────────────┐
│  5. Return ImportResult with stats and records                  │
└─────────────────────────────────────────────────────────────────┘
```

## Trigger Methods

### 1. Automatic (on startup)

When the server starts, if Emby is enabled in settings, an import job runs automatically:

```
Server starts
    ↓
Read settings.emby.enabled
    ↓
If enabled → Run import in background (forkIO)
If disabled → Skip (log message)
```

### 2. Manual (via API)

```bash
POST /api/emby/import
```

Returns `ImportResult` with detailed statistics.

### 3. Manual (via UI)

In Settings → General → Emby section, click the "Import Media Library" button.

A dialog will appear showing all available TV libraries. Select the libraries you want to import from and click "Import".

## Data Mapping

### Emby Item → Bangumi

| Emby Field | Bangumi Field | Notes |
|------------|---------------|-------|
| `id` | `embyId` | Used for deduplication |
| `name` | `titleChinese` | Series title |
| `providerIds.Tmdb` | `tmdbId` | Parsed as Int |
| `childCount` | `totalEpisodes` | Number of episodes |
| `premiereDate` | `airDate` | Extracted YYYY-MM-DD |
| (computed) | `posterUrl` | `{emby_url}/Items/{id}/Images/Primary` |

### Default Values

Fields not available from Emby use these defaults:

| Field | Default Value | Notes |
|-------|---------------|-------|
| `season` | 1 | Single season assumed |
| `platform` | TV | Standard TV anime |
| `airWeek` | 0 | Unknown air day |
| `episodeOffset` | 0 | No offset |
| `subscribed` | False | No auto-download |
| `currentEpisode` | 0 | No episodes watched |
| `autoComplete` | False | Manual completion |
| `sourceType` | Other | Not from RSS |

## Poster URL Construction

The poster URL is built from:

```
{emby_url}/Items/{item_id}/Images/Primary
```

Example:
```
http://localhost:8096/Items/12345/Images/Primary
```

Requirements:
- Item must have `imageTags.Primary` in metadata
- No authentication required (public images)

## API Reference

### Get Libraries

```bash
GET /api/emby/libraries
```

Response:
```json
[
  {
    "id": "12345",
    "name": "Anime",
    "collectionType": "tvshows"
  },
  {
    "id": "67890",
    "name": "Movies & TV",
    "collectionType": "mixed"
  }
]
```

Only returns libraries with `collectionType` of `tvshows` or `mixed`.

### Test Connection

```bash
POST /api/emby/test
Content-Type: application/json

{
  "settings": {
    "url": "http://localhost:8096",
    "apiKey": "your-api-key",
    "enabled": true
  }
}
```

Response:
```json
{
  "success": true,
  "message": "连接成功",
  "libraryCount": 3
}
```

### Import

```bash
POST /api/emby/import
Content-Type: application/json

{
  "libraryIds": ["12345", "67890"]
}
```

Request body:
- `libraryIds` (optional): Array of library IDs to import from. If empty or null, imports from all TV libraries.

Response:
```json
{
  "stats": {
    "total": 50,
    "imported": 10,
    "skipped": 40,
    "failed": 0
  },
  "records": [
    {
      "embyId": "12345",
      "title": "葬送的芙莉莲",
      "status": "Imported",
      "bangumiId": 42,
      "error": null
    },
    {
      "embyId": "12346",
      "title": "咒术回战",
      "status": "Skipped",
      "bangumiId": null,
      "error": null
    }
  ]
}
```

### Import Status Values

| Status | Description |
|--------|-------------|
| `Imported` | Successfully created new Bangumi |
| `Skipped` | Already exists (same embyId) |
| `Failed` | Error during import |

## Configuration

### Settings Schema

```json
{
  "emby": {
    "url": "http://localhost:8096",
    "apiKey": "your-emby-api-key",
    "enabled": true
  }
}
```

### Getting Emby API Key

1. Open Emby Server web interface
2. Go to **Settings** → **API Keys**
3. Click **New API Key**
4. Enter an application name (e.g., "moe-bangumi")
5. Copy the generated API key

## Code Structure

```
src/App/Emby/
├── Sync.hs     # Core import logic
└── Job.hs      # Startup import job

src/Infra/External/Emby/
├── API.hs      # Servant API client definition
├── Client.hs   # Generated client functions
├── Effect.hs   # Emby effect (operations + handler)
└── Types.hs    # EmbyItem, EmbyLibrary, EmbyError

src/Web/API/
├── Routes/Emby.hs   # API route definitions
└── Server/Emby.hs   # API handlers
```

### Key Functions

| Function | Location | Description |
|----------|----------|-------------|
| `importFromEmby` | `Sync.hs` | Main import orchestration |
| `getAllEmbySeriesItems` | `Sync.hs` | Get all Series from TV libraries |
| `importOneItem` | `Sync.hs` | Import single Emby item |
| `isTvLibrary` | `Sync.hs` | Filter for TV/mixed libraries |
| `buildPosterUrl` | `Sync.hs` | Construct poster URL |
| `extractTmdbId` | `Sync.hs` | Parse TMDB ID from providerIds |
| `runEmbyImportOnStartup` | `Job.hs` | Startup import job |

## Effect Dependencies

```haskell
importFromEmby
  :: ( Emby :> es    -- Emby API operations
     , DB :> es      -- Database operations
     , Log :> es     -- Logging
     )
  => EmbySettings
  -> Eff es ImportResult
```

## Logging

| Level | Event |
|-------|-------|
| INFO | Import started, items found, import completed, item imported |
| TRACE | Item skipped (already exists) |
| ATTENTION | Failed to get Emby items |

Example log output:
```
[emby-import] Starting Emby import
[emby-import] Got Emby items {"count":50}
[emby-import] Imported Bangumi from Emby {"bangumi_id":42,"title":"葬送的芙莉莲","emby_id":"12345"}
[emby-import] Skipping existing item {"emby_id":"12346","title":"咒术回战"}
[emby-import] Emby import completed {"total":50,"imported":10,"skipped":40,"failed":0}
```

## Troubleshooting

### Connection Failed

1. **Check URL format** - Include protocol (http:// or https://)
2. **Verify API Key** - Ensure key is valid and not expired
3. **Network access** - Confirm moe-bangumi can reach Emby server
4. **Firewall** - Check if port (default 8096) is accessible

### No Items Imported

1. **Check library type** - Only `tvshows` and `mixed` libraries are scanned
2. **Verify content** - Ensure libraries contain Series (not Movies)
3. **Check logs** - Look for error messages in import log

### All Items Skipped

This is normal if items were previously imported. The `embyId` field prevents duplicates.

To re-import:
1. Delete the Bangumi record from moe-bangumi
2. Run import again

### Missing Poster

Posters require:
1. Emby item has Primary image tag
2. Emby server URL is correctly configured
3. Image is publicly accessible (no auth required for images)

### Missing TMDB ID

TMDB ID is extracted from Emby's provider IDs. Ensure:
1. Emby has TMDB metadata plugin enabled
2. Item has been matched/identified in Emby
3. Provider IDs are refreshed

## Limitations

1. **No Sync** - Import is one-way; changes in Emby won't update existing Bangumi
2. **Single Season** - Each Series is imported as season 1; multi-season handling requires manual adjustment
3. **No Episode Data** - Only Series metadata is imported; individual episode info is not synced
4. **No Watch Progress** - Emby watch status is not imported
