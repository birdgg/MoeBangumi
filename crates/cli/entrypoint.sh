#!/bin/sh
# Entrypoint script for moe-bangumi
# Supports self-update by preferring the updated binary in /data/bin/

UPDATED_BINARY="/data/bin/moe"
ORIGINAL_BINARY="/app/moe"

# Check if updated binary exists and is executable
if [ -x "$UPDATED_BINARY" ]; then
    echo "Using updated binary from $UPDATED_BINARY"
    exec "$UPDATED_BINARY"
else
    echo "Using original binary from $ORIGINAL_BINARY"
    exec "$ORIGINAL_BINARY"
fi
