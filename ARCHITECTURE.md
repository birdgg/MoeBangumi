# Architecture

This document describes the architecture of moe-bangumi, a Rust workspace for anime tracking and download management.

## Workspace Structure

```
moe-bangumi/
├── app/                    # Application layer (runtime components)
│   ├── server/             # Web server (Axum API, state management)
│   └── jobs/               # Background job actors (RSS fetch, rename, log cleanup)
│
├── cli/                    # Binary crates (entry points)
│   ├── cli/                # Main CLI binary (moe)
│   └── calendar-seed/      # Calendar seed data generator tool
│
├── core/                   # Core library crates (reusable business logic)
│   ├── domain/             # Domain layer (models, repositories, services)
│   ├── pathgen/            # Media file path generation
│   ├── downloader/         # Downloader abstraction (qBittorrent, Transmission)
│   ├── metadata/           # Metadata provider abstraction (BGM.tv, TMDB)
│   ├── parser/             # Anime filename parser
│   ├── washing/            # Torrent priority/washing algorithm
│   └── updater/            # Self-update service
│
├── libs/                   # External API clients
│   ├── bgmtv/              # BGM.tv API client
│   ├── mikan/              # Mikan (蜜柑计划) API client
│   ├── tmdb/               # TMDB API client
│   ├── qbittorrent/        # qBittorrent Web API client
│   └── rss/                # RSS feed fetching and parsing
│
└── web/                    # Frontend (React + Vite)
```

## Dependency Graph

```
                    ┌─────────────┐
                    │   cli/cli   │
                    └──────┬──────┘
                           │
                    ┌──────▼──────┐
                    │ app/server  │
                    └──────┬──────┘
                           │
              ┌────────────┼────────────┐
              │            │            │
       ┌──────▼──────┐ ┌───▼───┐ ┌──────▼──────┐
       │  app/jobs   │ │pathgen│ │     rss     │
       └──────┬──────┘ └───────┘ └─────────────┘
              │
       ┌──────▼──────┐
       │ core/domain │
       └──────┬──────┘
              │
    ┌─────────┼─────────┬─────────┬─────────┐
    │         │         │         │         │
┌───▼───┐ ┌───▼───┐ ┌───▼───┐ ┌───▼───┐ ┌───▼───┐
│parser │ │washing│ │metadata│ │downldr│ │ libs/ │
└───────┘ └───────┘ └───┬───┘ └───┬───┘ └───────┘
                        │         │
                   ┌────┴────┐ ┌──┴──┐
                   │bgmtv/   │ │qbit │
                   │tmdb     │ │     │
                   └─────────┘ └─────┘
```

## Crate Descriptions

### Application Layer (`app/`)

| Crate | Description |
|-------|-------------|
| `server` | Axum-based HTTP API server. Handles routing, request/response, and application state. |
| `jobs` | Background job actors for periodic tasks: RSS fetching, file renaming, log cleanup. |

### CLI Layer (`cli/`)

| Crate | Description |
|-------|-------------|
| `cli` | Main entry point. Reads config from `.env`, initializes logging, starts server. |
| `calendar-seed` | Tool to generate calendar seed data from Mikan. |

### Core Layer (`core/`)

| Crate | Description |
|-------|-------------|
| `domain` | Core business logic: models, repositories (SQLite), services, and core actors. |
| `pathgen` | Generates Plex/Jellyfin-compatible paths for downloaded media files. |
| `downloader` | Abstraction layer for download clients (qBittorrent, Transmission). |
| `metadata` | Unified metadata provider abstraction with BGM.tv and TMDB adapters. |
| `parser` | Parses anime filenames to extract episode, season, subtitle group, resolution. |
| `washing` | Torrent priority algorithm for replacing lower-quality downloads. |
| `updater` | Self-update service for checking and applying updates from GitHub. |

### Libraries Layer (`libs/`)

| Crate | Description |
|-------|-------------|
| `bgmtv` | BGM.tv (Bangumi) API client for anime metadata. |
| `mikan` | Mikan (蜜柑计划) client for anime resource discovery. |
| `tmdb` | TMDB API client for movie/TV metadata. |
| `qbittorrent` | qBittorrent Web API client. |
| `rss` | RSS feed fetcher with conditional request support (ETag/Last-Modified). |

## Key Patterns

### Layered Architecture

```
HTTP Request
     │
     ▼
┌─────────────────────────────────┐
│  API Handlers (app/server/api)  │  ← Request validation, response formatting
└─────────────────────────────────┘
     │
     ▼
┌─────────────────────────────────┐
│  Services (core/domain/services)│  ← Business logic, orchestration
└─────────────────────────────────┘
     │
     ▼
┌─────────────────────────────────┐
│  Repositories (core/domain/repo)│  ← Data access, SQLite queries
└─────────────────────────────────┘
     │
     ▼
┌─────────────────────────────────┐
│  Database (SQLite)              │  ← Persistence
└─────────────────────────────────┘
```

### Actor Model

Background tasks use an actor pattern with message passing:

```rust
// Actor structure
struct RssFetchActor {
    service: Arc<RssProcessingService>,
    receiver: mpsc::Receiver<Message>,
}

// Handle for external communication
pub struct RssFetchHandle {
    sender: mpsc::Sender<Message>,
}

impl RssFetchHandle {
    pub async fn shutdown(&self) { ... }
}
```

Actors in `app/jobs/`: Timer-based (periodic execution)
Actors in `core/domain/services/actors/`: Message-driven (on-demand)

### State Sharing

Application state is shared via Axum's state extractor:

```rust
// app/server/src/infra/state.rs
pub struct AppState {
    pub db: SqlitePool,
    pub bangumi: Arc<BangumiService>,
    pub downloader: Arc<DownloaderHandle>,
    // ... other services
}
```

## Module Conventions

### No `mod.rs`

This project follows the modern Rust convention of avoiding `mod.rs`:

```
// Good
src/models.rs           # Module declaration
src/models/             # Submodules directory
    bangumi.rs
    calendar.rs

// Avoid
src/models/mod.rs       # Don't use this pattern
```

### Feature Flags

OpenAPI documentation is optional:

```toml
[features]
default = []           # OpenAPI disabled by default
openapi = [...]        # Enable for development/docs
```

```bash
# Development (with OpenAPI)
just dev               # Enables openapi feature

# Production build (minimal binary)
just build-release     # No openapi, smaller binary
```

## Adding a New Crate

1. Create directory under appropriate layer:
   - `core/` for reusable business logic
   - `libs/` for external API clients
   - `app/` for application-specific code

2. Create `Cargo.toml` with workspace inheritance:
   ```toml
   [package]
   name = "your-crate"
   version.workspace = true
   edition.workspace = true
   license.workspace = true

   [dependencies]
   # Use workspace dependencies
   tokio.workspace = true
   ```

3. The crate is auto-discovered via workspace glob:
   ```toml
   # Root Cargo.toml
   members = ["core/*", "app/*", "cli/*", "libs/*"]
   ```

4. Follow "no mod.rs" convention for modules.

## Development Workflow

```bash
# Start development server (with OpenAPI docs at /docs)
just dev

# Run both backend and frontend
just dev-all

# Check compilation
just check

# Run tests
just test

# Build production binary
just build-release
```
