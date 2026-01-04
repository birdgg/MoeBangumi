# Stage 1: Build frontend
FROM oven/bun:1 AS frontend
WORKDIR /app/web
COPY web/package.json web/bun.lock* ./
RUN bun install --frozen-lockfile
COPY web .
RUN bun run build

# Stage 2: Prepare dependency recipe
FROM rust:1-alpine AS planner
WORKDIR /app
RUN cargo install cargo-chef
COPY . .
RUN cargo chef prepare --recipe-path recipe.json

# Stage 3: Build Rust binary (native Alpine build = musl linked)
FROM rust:1-alpine AS builder
WORKDIR /app
RUN apk add --no-cache musl-dev
RUN cargo install cargo-chef
COPY --from=planner /app/recipe.json recipe.json
RUN cargo chef cook --release --recipe-path recipe.json
COPY . .
RUN cargo build --release -p cli

# Stage 4: Runtime image
FROM alpine:3.21 AS runtime
WORKDIR /app

# Install runtime dependencies
RUN apk add --no-cache ca-certificates curl tzdata

# Create non-root user
RUN addgroup -S moe && adduser -S moe -G moe

# Copy binary from builder
COPY --from=builder /app/target/release/moe /app/moe

# Copy frontend dist
COPY --from=frontend /app/web/dist /app/dist

# Create data directory and set permissions
RUN mkdir -p /data && chown -R moe:moe /app /data

USER moe

# Set environment variables
ENV APP_ENV=prod
ENV PORT=3000

EXPOSE 3000

VOLUME ["/data"]

CMD ["/app/moe"]
