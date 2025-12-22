# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build and Run Commands

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
