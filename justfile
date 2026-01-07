# moe-bangumi justfile
# Run `just` to start the backend dev server
# Run `just --list` to see all available commands

# Default: start backend dev server
default: dev-all

# === Rust Commands ===

# Check compilation without building
check:
    cargo check

# Run tests
test:
    cargo test

# Run the server (dev mode)
dev:
    cargo run -p cli --bin moe --features openapi

# Generate calendar seed data (all seasons from 2013)
seed:
    cargo run -p cli --bin calendar_seed

# Generate calendar seed data (recent N seasons)
seed-recent count:
    cargo run -p cli --bin calendar_seed -- --recent {{count}}

# === Frontend Commands ===

# Install frontend dependencies
web-install:
    cd web && bun install

# Start frontend dev server
web-dev:
    cd web && bun run dev

# Run frontend linter
web-lint:
    cd web && bun run lint

# Generate API client from OpenAPI spec
web-gen-api:
    cd web && bun run gen:api

# === Combined Commands ===

# Run both backend and frontend dev servers (requires: just dev-all)
dev-all:
    #!/usr/bin/env bash
    trap 'kill 0' EXIT
    cargo run -p cli --bin moe --features openapi &
    cd web && bun run dev &
    wait

# === Docker Commands ===

# Start Transmission in Docker
transmission:
    docker compose -f docker/docker-compose.transmission.yaml up -d

# === Release Commands ===

# Generate changelog (default: unreleased, use "full" to generate all)
changelog mode="unreleased":
    #!/usr/bin/env bash
    if [ "{{mode}}" = "full" ]; then
        git-cliff -o CHANGELOG.md
    else
        git-cliff --unreleased
    fi

# Release with version bump: just release patch|minor|major|X.Y.Z
# Examples:
#   just release patch   # 0.2.4 -> 0.2.5
#   just release minor   # 0.2.4 -> 0.3.0
#   just release major   # 0.2.4 -> 1.0.0
#   just release 0.3.0   # explicit version
release level:
    cargo release {{level}} --execute

# Preview release (dry-run)
release-dry level:
    cargo release {{level}}

