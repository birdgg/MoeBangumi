# moe-rs justfile
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
    cargo run -p cli

# Check compilation without building
check:
    cargo check

# Run tests
test:
    cargo test

# Watch mode (requires cargo-watch: cargo install cargo-watch)
watch:
    cargo watch -x 'run -p cli'

# Alias for run
dev: run

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
    cargo run -p cli &
    cd web && bun run dev &
    wait

# Build both backend and frontend
build-all: build web-build
