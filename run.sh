#!/bin/sh
# Start from any directory; native ICE resolves its own data paths.
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
PROGRAM="$ROOT/build/ice"
BUILD_OPTION=""
if [ "${1:-}" = "--deltat2026" ]; then
    PROGRAM="$ROOT/build/ice-dt2026"
    BUILD_OPTION=" --deltat2026"
    shift
fi
if [ ! -x "$PROGRAM" ]; then
    printf 'Build ICE first: %s/build.sh%s\n' "$ROOT" "$BUILD_OPTION" >&2
    exit 1
fi
exec "$PROGRAM" "$@"
