# moe-bangumi justfile
# Run `just` to start the backend dev server
# Run `just --list` to see all available commands

# Default: start backend dev server
default: dev

# === Rust Commands ===

# Build the project (debug)
build:
    cargo build

# Build the project (release)
build-release:
    cargo build --release

# Run the server
run:
    cargo run -p cli --bin moe

# Check compilation without building
check:
    cargo check

# Run tests
test:
    cargo test

# Watch mode (requires cargo-watch: cargo install cargo-watch)
watch:
    cargo watch -x 'run -p cli --bin moe'

# Alias for run
dev: run

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

# Build frontend for production
web-build:
    cd web && bun run build

# Run frontend linter
web-lint:
    cd web && bun run lint

# Generate API client from OpenAPI spec
web-gen-api:
    cd web && bun run gen:api

# Preview production build
web-preview:
    cd web && bun run preview

# === Combined Commands ===

# Run both backend and frontend dev servers (requires: just dev-all)
dev-all:
    #!/usr/bin/env bash
    trap 'kill 0' EXIT
    cargo run -p cli --bin moe &
    cd web && bun run dev &
    wait

# Build both backend and frontend
build-all: build web-build

# === Docker Commands ===

# Start Transmission in Docker
transmission:
    docker compose -f docker/docker-compose.transmission.yaml up -d

# === Release Commands ===

# Generate changelog for unreleased changes
changelog:
    git-cliff --unreleased

# Generate full changelog
changelog-full:
    git-cliff -o CHANGELOG.md

# Bump version in all Cargo.toml files
bump-version version:
    #!/usr/bin/env bash
    set -euo pipefail
    VERSION="{{version}}"

    # Validate version format
    if [[ ! "$VERSION" =~ ^[0-9]+\.[0-9]+\.[0-9]+(-[a-zA-Z0-9]+)?$ ]]; then
        echo "Error: Invalid version format. Expected: X.Y.Z or X.Y.Z-suffix"
        exit 1
    fi

    # Update workspace version
    sed -i '' "s/^version = \".*\"/version = \"$VERSION\"/" Cargo.toml

    echo "Version bumped to $VERSION"
    echo "Run 'cargo check' to verify the changes"

# Trigger release workflow on GitHub (requires gh CLI)
release version:
    #!/usr/bin/env bash
    set -euo pipefail
    VERSION="{{version}}"

    # Validate version format
    if [[ ! "$VERSION" =~ ^[0-9]+\.[0-9]+\.[0-9]+(-[a-zA-Z0-9]+)?$ ]]; then
        echo "Error: Invalid version format. Expected: X.Y.Z or X.Y.Z-suffix"
        exit 1
    fi

    echo "Triggering release workflow for version $VERSION..."
    gh workflow run release.yml -f version="$VERSION"
    echo "Release workflow triggered! Check GitHub Actions for progress."

# Dry-run release (builds but doesn't publish)
release-dry version:
    #!/usr/bin/env bash
    set -euo pipefail
    VERSION="{{version}}"

    echo "Triggering dry-run release workflow for version $VERSION..."
    gh workflow run release.yml -f version="$VERSION" -f dry_run=true
    echo "Dry-run release workflow triggered! Check GitHub Actions for progress."

# Local release build (for testing)
release-local:
    #!/usr/bin/env bash
    set -euo pipefail

    echo "Building release binary..."
    cargo build --release -p cli

    echo "Building frontend..."
    cd web && bun install && bun run build

    echo "Release build complete!"
    echo "Binary: target/release/moe"
    echo "Frontend: web/dist/"
