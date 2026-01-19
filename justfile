# moe-bangumi justfile
# Run `just` to start the backend dev server
# Run `just --list` to see all available commands

# Default: start backend dev server
default: dev

# === Haskell Commands ===

# Build the project
build:
    cabal build

# Run tests
test:
    cabal test

# Run the server
dev:
    MOE_LOG_LEVEL=trace cabal run moe-bangumi

# Start REPL
repl:
    cabal repl

# Run HLint
lint:
    hlint src/ app/

# Auto-fix HLint suggestions
lint-fix:
    hlint --refactor --refactor-options="--inplace" src/ app/

# Clean build artifacts
clean:
    cabal clean

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

# Run both backend and frontend dev servers
dev-all:
    #!/usr/bin/env bash
    trap 'kill 0' EXIT
    cabal run moe-bangumi &
    cd web && bun run dev &
    wait

# === Docker Commands ===

# Build Docker image (current platform only)
docker-build:
    docker build -t moe-bangumi:latest .

# Build multi-arch Docker image (requires buildx)
docker-build-multi:
    docker buildx build --platform linux/amd64,linux/arm64 -t moe-bangumi:latest .

# Build and push multi-arch Docker image to GHCR
docker-push:
    docker buildx build --platform linux/amd64,linux/arm64 -t ghcr.io/birdgg/moe-bangumi:latest --push .

# Run Docker container locally
docker-run:
    docker run --rm -p 3000:3000 -u $(id -u):$(id -g) -v $(pwd)/data:/app/data moe-bangumi:latest

# Start qBittorrent in Docker
qb:
    docker compose -f docker/docker-compose.qbittorrent.yaml up -d

# === Release Commands ===

# Generate changelog (default: unreleased, use "full" to generate all)
changelog mode="unreleased":
    #!/usr/bin/env bash
    if [ "{{mode}}" = "full" ]; then
        git-cliff -o CHANGELOG.md
    else
        git-cliff --unreleased
    fi
