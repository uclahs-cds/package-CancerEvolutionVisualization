#!/usr/bin/env bash
#
# Render vignettes/ManuscriptFigures.Rmd for qualitative review before opening a
# PR, and diff the figures against another ref.
#
#   tools/render-manuscript-figures.sh            # current worktree vs origin/main
#   tools/render-manuscript-figures.sh main       # explicit baseline ref
#   tools/render-manuscript-figures.sh --no-diff  # render current worktree only
#
# Output goes OUTSIDE the repo, to keep it untracked:
#   ../cev-render-review/manuscript/{current,baseline}/figures/*.png
#   ../cev-render-review/manuscript/REVIEW.html    <- open this
#
# Requires the cev-dev:local image (see tools/Dockerfile.dev).
set -euo pipefail

REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
OUTROOT="$(cd "$REPO/.." && pwd)/cev-render-review/manuscript"
IMAGE="${CEV_DEV_IMAGE:-cev-dev:local}"
BASELINE="${1:-origin/main}"

render () {  # render <src-dir> <out-dir>
    mkdir -p "$2"
    docker run --rm -v "$1":/pkg -v "$2":/out "$IMAGE" bash -c '
        set -e
        cd /tmp && rm -rf mf && mkdir mf && cd mf
        cp -r /pkg/vignettes ./vignettes && cd vignettes
        Rscript -e "suppressMessages(pkgload::load_all(\"/pkg\", quiet = TRUE));
                    rmarkdown::render(\"ManuscriptFigures.Rmd\", quiet = TRUE, output_dir = \".\")"
        cp -f ManuscriptFigures.html /out/ 2>/dev/null || true
        cp -rf figures /out/ 2>/dev/null || true
    '
}

echo "==> rendering current worktree"
rm -rf "$OUTROOT/current"; render "$REPO" "$OUTROOT/current"

if [[ "$BASELINE" == "--no-diff" ]]; then
    echo "==> done: $OUTROOT/current"
    exit 0
fi

echo "==> rendering baseline ($BASELINE)"
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
git -C "$REPO" archive "$BASELINE" | tar -x -C "$TMP"
rm -rf "$OUTROOT/baseline"; render "$TMP" "$OUTROOT/baseline"

echo "==> building REVIEW.html"
OUTROOT="$OUTROOT" BASELINE="$BASELINE" python3 "$REPO/tools/compare-figures.py"
echo "==> open $OUTROOT/REVIEW.html"
