# metadata

Metadata management crate for moe-bangumi. Provides unified access to anime metadata from BGM.tv and TMDB, with automatic poster downloading and background synchronization.

## Features

- **MetadataService** - Query and CRUD operations for metadata (BGM.tv, TMDB)
- **PosterService** - Poster image downloading with URL deduplication
- **MetadataActor** - Background actor with internal scheduling for automatic sync
- **Fire-and-forget API** - Non-blocking operations for poster downloads

## Architecture

```
┌─────────────────────────────────────────────────────────┐
│                    MetadataHandle                        │
│              (Fire-and-forget interface)                 │
└─────────────────────┬───────────────────────────────────┘
                      │ mpsc::channel
                      ▼
┌─────────────────────────────────────────────────────────┐
│                    MetadataActor                         │
│  ┌─────────────────┐  ┌──────────────────────────────┐  │
│  │ Message Handler │  │ Internal Interval (1 hour)   │  │
│  └────────┬────────┘  └──────────────┬───────────────┘  │
│           │                          │                   │
│           ▼                          ▼                   │
│  ┌─────────────────────────────────────────────────┐    │
│  │              sync_all_metadata()                 │    │
│  │  - Download missing posters (Semaphore: 5)      │    │
│  │  - Sync TMDB IDs (Concurrency: 5)               │    │
│  └─────────────────────────────────────────────────┘    │
└─────────────────────────────────────────────────────────┘
                      │
        ┌─────────────┼─────────────┐
        ▼             ▼             ▼
┌───────────┐  ┌───────────┐  ┌───────────┐
│  SQLite   │  │  BGM.tv   │  │   TMDB    │
│    DB     │  │   API     │  │   API     │
└───────────┘  └───────────┘  └───────────┘
```

## Usage

### Basic Setup

```rust
use std::sync::Arc;
use metadata::{create_metadata_actor, MetadataService, PosterService};

// Create services
let metadata_service = Arc::new(MetadataService::new(
    db.clone(),
    bgmtv_client,
    tmdb_client,
));

let poster_service = Arc::new(PosterService::with_client_provider(
    client_provider,
    posters_path,
));

// Create actor with internal scheduling (syncs every hour)
let metadata_handle = create_metadata_actor(
    db.clone(),
    metadata_service.clone(),
    poster_service,
);
```

### Fire-and-forget Poster Download

```rust
// Download poster asynchronously (returns immediately)
metadata_handle.download(metadata_id, poster_url);

// Batch download
metadata_handle.download_batch(vec![
    (metadata_id_1, url_1),
    (metadata_id_2, url_2),
]);
```

### Manual Sync Trigger

```rust
// Trigger metadata sync manually
metadata_handle.trigger_sync();
```

### Custom Sync Interval

```rust
use std::time::Duration;
use metadata::create_metadata_actor_with_interval;

// Create actor with custom interval (e.g., 30 minutes)
let metadata_handle = create_metadata_actor_with_interval(
    db.clone(),
    metadata_service,
    poster_service,
    Duration::from_secs(30 * 60),
);
```

### Direct Service Usage

```rust
use metadata::MetadataService;

// Query metadata by BGM.tv ID
let metadata = metadata_service.get_by_bgmtv_id(12345).await?;

// Search TMDB for matching ID
let tmdb_id = metadata_service.find_tmdb_id("anime title").await?;

// Create or update metadata
let metadata = metadata_service.find_or_update(create_metadata).await?;
```

## Module Structure

```
src/
├── lib.rs           # Public exports
├── error.rs         # MetadataError, PosterError
├── poster.rs        # PosterService - image downloading
├── service.rs       # MetadataService - BGM.tv/TMDB queries
├── actor.rs         # Actor creation functions
└── actor/
    ├── messages.rs  # MetadataMessage enum
    ├── handle.rs    # MetadataHandle (client interface)
    └── runner.rs    # MetadataActor (background worker)
```

## Public API

### Types

| Type | Description |
|------|-------------|
| `MetadataHandle` | Fire-and-forget interface for actor operations |
| `MetadataService` | Synchronous metadata queries (BGM.tv, TMDB) |
| `PosterService` | Poster downloading with deduplication |
| `MetadataError` | Error type for metadata operations |
| `PosterError` | Error type for poster operations |
| `SyncStats` | Statistics from sync operations |

### Functions

| Function | Description |
|----------|-------------|
| `create_metadata_actor` | Create actor with default 1-hour sync interval |
| `create_metadata_actor_with_interval` | Create actor with custom sync interval |

### Re-exports from `model`

- `Metadata`, `CreateMetadata`, `UpdateMetadata`
- `MetadataRepository`, `MetadataToSync`
- `Platform`

## Features

| Feature | Description |
|---------|-------------|
| `openapi` | Enable OpenAPI schema generation (utoipa) |

## Concurrency Control

- **Poster downloads**: Limited to 5 concurrent downloads via `Semaphore`
- **TMDB sync**: Limited to 5 concurrent API calls via `buffer_unordered`
- **Chunk processing**: Syncs in batches of 100 records to limit memory usage
- **URL deduplication**: Prevents duplicate downloads of the same URL

## Graceful Shutdown

The actor supports graceful shutdown via the `shutdown()` method:

```rust
// In your application shutdown sequence
metadata_handle.shutdown().await;

// Or check if the actor is still running
if metadata_handle.is_running() {
    metadata_handle.shutdown().await;
}
```

**Important**: Ensure `MetadataHandle` is properly shut down during application termination to allow the actor to:
- Stop accepting new messages
- Cancel the internal sync timer
- Log shutdown completion
