#!/usr/bin/env bash
# Fast single-figure preview for formatting work. See tools/preview-figure.R.
#   tools/preview-figure.sh timeline-tree
set -euo pipefail
REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
OUT="$(cd "$REPO/.." && pwd)/cev-render-review/preview"
IMAGE="${CEV_DEV_IMAGE:-cev-dev:local}"
mkdir -p "$OUT"
docker run --rm -v "$REPO":/pkg -v "$OUT":/out "$IMAGE" \
    Rscript /pkg/tools/preview-figure.R "$@" 2>&1 | grep -vE '^WARNING: The requested image'
echo "==> $OUT"
