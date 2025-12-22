# Default recipe
default:
    @just --list

# Build the project
build:
    cargo build

# Build in release mode
build-release:
    cargo build --release

# Run the server
run:
    cargo run -p cli

# Run with custom port
run-port port="8080":
    cargo run -p cli -- --port {{port}}

# Run with debug logging
run-debug:
    RUST_LOG=debug cargo run -p cli

# Check compilation
check:
    cargo check

# Run clippy lints
lint:
    cargo clippy --all-targets --all-features -- -D warnings

# Format code
fmt:
    cargo fmt

# Check formatting
fmt-check:
    cargo fmt -- --check

# Run tests
test:
    cargo test

# Run tests with output
test-verbose:
    cargo test -- --nocapture

# Clean build artifacts
clean:
    cargo clean

# Watch and run (requires cargo-watch)
watch:
    cargo watch -x 'run -p cli'

# Run database migrations
migrate:
    cargo run -p cli -- --migrate

# Full CI check
ci: fmt-check lint test build
