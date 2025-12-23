# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build and Run Commands

### Backend (Rust)

```bash
# Build
cargo build
cargo build --release

# Run the server (defaults: port 3000, database todos.db)
cargo run -p cli

# Check compilation without building
cargo check

# Enable debug logging
RUST_LOG=debug cargo run -p cli
```

### Frontend (Web)

```bash
cd web

# Install dependencies
bun install

# Development server
bun run dev

# Production build
bun run build

# Preview production build
bun run preview

# Lint
bun run lint
```

## Configuration

Configuration is read from environment variables. Create a `.env` file in the project root:

```env
PORT=3000        # Server port (default: 3000)
DATABASE=todos.db # SQLite database file (default: todos.db)
```

## Architecture

This is a Rust workspace containing a TODO REST API built with Axum and SQLite.

### Workspace Structure

- **crates/cli** - CLI entry point, reads config from .env and starts server
- **crates/server** - Core library with web server logic
- **crates/bgmtv** - BGM.tv API client crate
- **crates/qbittorrent** - qBittorrent Web API client crate
- **web/** - Frontend application (React + Vite)

### Server Module Organization

```
server/src/
├── lib.rs          # Exports modules, run_server() entry function
├── models.rs       # Data models (Bangumi, CreateBangumi, etc.)
├── repositories.rs # Database CRUD operations
├── db.rs           # SQLite pool creation
├── router.rs       # Axum route definitions
├── handlers.rs     # Request handlers, returns JSON responses
├── state.rs        # AppState with shared resources
└── config.rs       # Configuration struct
```

### Key Patterns

- **State sharing**: SQLite connection pool passed via Axum router state
- **Async**: All database operations and handlers are async (Tokio runtime)
- **Error handling**: Handlers log errors with tracing and return appropriate HTTP status codes

### API Endpoints

- `GET /todos` - List all todos
- `POST /todos` - Create todo (JSON body: `{"title": "..."}`)
- `DELETE /todos/{id}` - Delete todo by ID

## Frontend Architecture

The `web/` directory contains a React frontend application.

### Tech Stack

- **React 19** - UI framework
- **Vite** - Build tool and dev server
- **TailwindCSS 4** - Utility-first CSS
- **Tanstack Router** - File-based routing
- **shadcn/ui** - UI component library (base-ui based)
- **Tabler Icons** - Icon library
- **Bun** - Package manager and runtime

### Frontend Structure

```
web/src/
├── main.tsx              # App entry point
├── index.css             # Global styles and Tailwind config
├── routeTree.gen.ts      # Auto-generated route tree
├── lib/
│   └── utils.ts          # Utility functions (cn helper)
├── components/
│   ├── ui/               # shadcn UI components
│   ├── app-layout.tsx    # Main layout with sidebar
│   └── bangumi-card.tsx  # Bangumi card component
└── routes/
    ├── __root.tsx        # Root layout
    └── index.tsx         # Home page
```

### Key Frontend Patterns

- **File-based routing**: Routes defined in `src/routes/` directory
- **CSS Variables**: Theme colors defined in `index.css` using oklch
- **Dark mode**: Toggle via `.dark` class on document element
- **Component styling**: Uses `cn()` helper for conditional class merging

## Coding Conventions

### Rust Module Style

- **Do NOT use `mod.rs`** for module definitions. Use `module_name.rs` + `module_name/` directory instead of `module_name/mod.rs`.
- Example: Use `models.rs` + `models/bangumi.rs` instead of `models/mod.rs` + `models/bangumi.rs`

## Git Commit Style

Use [Conventional Commits](https://www.conventionalcommits.org/) format:

```
<type>(<scope>): <description>

[optional body]

[optional footer(s)]
```

### Types

- **feat**: New feature
- **fix**: Bug fix
- **docs**: Documentation only
- **style**: Code style (formatting, semicolons, etc.)
- **refactor**: Code refactoring (no feature or bug fix)
- **perf**: Performance improvement
- **test**: Adding or updating tests
- **chore**: Build process, dependencies, tooling

### Examples

```
feat(api): add user authentication endpoint
fix(db): resolve connection pool timeout issue
docs: update README with new configuration options
refactor(handlers): simplify error handling logic
chore(deps): update axum to 0.8
```
