# Dockerfile for GitHub Actions release builds
# Uses pre-built frontend artifact and cargo-chef for dependency caching

ARG FRONTEND_DIST=./frontend-dist

# Stage 1: Prepare recipe with cargo-chef
FROM rust:1-alpine AS chef
RUN apk add --no-cache musl-dev && \
    cargo install cargo-chef --locked
WORKDIR /app

# Stage 2: Generate dependency recipe
FROM chef AS planner
COPY . .
RUN cargo chef prepare --recipe-path recipe.json

# Stage 3: Build dependencies (cached layer)
FROM chef AS deps
COPY --from=planner /app/recipe.json recipe.json
# Build dependencies only - this layer is cached
RUN cargo chef cook --release --recipe-path recipe.json

# Stage 4: Build application
FROM chef AS builder
COPY --from=deps /app/target target
COPY --from=deps /usr/local/cargo /usr/local/cargo
COPY . .
RUN cargo build --release -p cli

# Stage 5: Runtime image
FROM alpine:3.21 AS runtime
ARG FRONTEND_DIST
WORKDIR /app

# Install runtime dependencies and create user
RUN apk add --no-cache ca-certificates curl tzdata \
    && addgroup -S moe && adduser -S moe -G moe

# Copy binary from builder
COPY --from=builder --chown=moe:moe /app/target/release/moe /app/moe

# Copy pre-built frontend dist
COPY --chown=moe:moe ${FRONTEND_DIST} /app/dist

# Create data directory and set permissions
RUN mkdir -p /data && chown moe:moe /data

# Set environment variables
ENV APP_ENV=prod
ENV PORT=3000

USER moe

EXPOSE 3000

VOLUME ["/data"]

CMD ["/app/moe"]
