#!/bin/sh
# Entrypoint script for moe-bangumi
# Supports self-update by preferring the updated binary in /data/bin/

UPDATED_BINARY="/data/bin/moe"
ORIGINAL_BINARY="/app/moe"

# Check if updated binary exists
if [ -f "$UPDATED_BINARY" ]; then
    # Verify it's executable (could fail if update was interrupted)
    if [ -x "$UPDATED_BINARY" ]; then
        echo "Using updated binary from $UPDATED_BINARY"
        exec "$UPDATED_BINARY"
    else
        echo "WARNING: Updated binary exists but is not executable, using original"
    fi
fi

echo "Using original binary from $ORIGINAL_BINARY"
exec "$ORIGINAL_BINARY"
