#!/usr/bin/env bash
# Starts the explorer, runs every browser check and writes validation/e2e-<version>.log.
# Usage: tools/e2e/run-all.sh [chromium_path] [port]
set -u
CHROME=${1:-}
PORT=${2:-8790}
VER=$(Rscript -e 'cat(as.character(packageVersion("herminR")))')
SHOTS=$(mktemp -d)
LOG=validation/e2e-$VER.log
Rscript -e "herminR::run_explorer(port = $PORT, launch.browser = FALSE)" >/dev/null 2>&1 &
APP=$!
trap 'kill $APP 2>/dev/null' EXIT
for i in $(seq 1 60); do curl -s "http://127.0.0.1:$PORT" >/dev/null && break; sleep 1; done
{
  echo "herminR $VER browser checks, $(date -u +%Y-%m-%dT%H:%MZ)"
  Rscript -e 'cat("R", as.character(getRversion()), "| shiny", as.character(packageVersion("shiny")), "| ggplot2", as.character(packageVersion("ggplot2")), "\n")'
  status=0
  for s in happy_path interaction flows viewports; do
    echo "== $s"
    python3 tools/e2e/$s.py "http://127.0.0.1:$PORT" "$SHOTS" $CHROME || status=1
  done
  echo "screenshots: $SHOTS"
  exit $status
} 2>&1 | tee "$LOG"
exit ${PIPESTATUS[0]}
