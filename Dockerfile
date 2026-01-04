# Stage 1: Build frontend
FROM oven/bun:1 AS frontend
WORKDIR /app/web
COPY web/package.json web/bun.lock* ./
RUN bun install --frozen-lockfile
COPY web .
RUN bun run build

# Stage 2: Chef base image with musl support
FROM clux/muslrust:stable AS chef
USER root
RUN cargo install cargo-chef
WORKDIR /app

# Stage 3: Prepare recipe
FROM chef AS planner
COPY . .
RUN cargo chef prepare --recipe-path recipe.json

# Stage 4: Build Rust binary
FROM chef AS builder
ARG TARGETARCH
COPY --from=planner /app/recipe.json recipe.json
RUN if [ "$TARGETARCH" = "arm64" ]; then \
        rustup target add aarch64-unknown-linux-musl; \
    fi
RUN RUST_TARGET=$([ "$TARGETARCH" = "arm64" ] && echo "aarch64-unknown-linux-musl" || echo "x86_64-unknown-linux-musl") && \
    cargo chef cook --release --target $RUST_TARGET --recipe-path recipe.json
COPY . .
RUN RUST_TARGET=$([ "$TARGETARCH" = "arm64" ] && echo "aarch64-unknown-linux-musl" || echo "x86_64-unknown-linux-musl") && \
    cargo build --release --target $RUST_TARGET -p cli && \
    cp /app/target/$RUST_TARGET/release/moe /app/moe

# Stage 5: Runtime image
FROM alpine:3.21 AS runtime
WORKDIR /app

# Install runtime dependencies and create user
RUN apk add --no-cache ca-certificates curl tzdata \
    && addgroup -S moe && adduser -S moe -G moe

# Copy binary from builder
COPY --from=builder --chown=moe:moe /app/moe /app/moe

# Copy frontend dist
COPY --from=frontend --chown=moe:moe /app/web/dist /app/dist

# Create data directory and set permissions
RUN mkdir -p /data && chown moe:moe /data

# Set environment variables
ENV APP_ENV=prod
ENV PORT=3000

USER moe

EXPOSE 3000

VOLUME ["/data"]

CMD ["/app/moe"]
