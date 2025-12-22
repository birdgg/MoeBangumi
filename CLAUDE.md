# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build and Run Commands

```bash
# Build
cargo build
cargo build --release

# Run the server (defaults: port 3000, host 127.0.0.1, database todos.db)
cargo run -p cli

# Run with custom options
cargo run -p cli -- --port 8080 --host 0.0.0.0 --database mydata.db

# Check compilation without building
cargo check

# Enable debug logging
RUST_LOG=debug cargo run -p cli
```

## Architecture

This is a Rust workspace containing a TODO REST API built with Axum and SQLite.

### Workspace Structure

- **crates/cli** - CLI entry point, parses args (clap) and starts server
- **crates/server** - Core library with web server logic

### Server Module Organization

```
server/src/
├── lib.rs      # Exports modules, run_server() entry function
├── models.rs   # Todo and CreateTodo structs
├── db.rs       # SQLite pool creation and CRUD operations
├── router.rs   # Axum route definitions (GET/POST/DELETE /todos)
└── handlers.rs # Request handlers, returns JSON responses
```

### Key Patterns

- **State sharing**: SQLite connection pool passed via Axum router state
- **Async**: All database operations and handlers are async (Tokio runtime)
- **Error handling**: Handlers log errors with tracing and return appropriate HTTP status codes

### API Endpoints

- `GET /todos` - List all todos
- `POST /todos` - Create todo (JSON body: `{"title": "..."}`)
- `DELETE /todos/{id}` - Delete todo by ID
