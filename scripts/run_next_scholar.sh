#!/usr/bin/env bash
set -euo pipefail

# Run from repo root regardless of current working directory.
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_ROOT"

POSTED_FILE="posted.tsv"
TODO_FILE="to_do.tsv"
PRIORITY_FILE="prioritize.tsv"
LOG_FILE="logs/cron_scholar_bluesky.log"
DRY_RUN=0

for arg in "$@"; do
  case "$arg" in
    --dry-run)
      DRY_RUN=1
      ;;
    -h|--help)
      cat <<'EOF'
Usage: run_next_scholar.sh [--dry-run]

--dry-run   Print the python command that would run, but do not execute it.
EOF
      exit 0
      ;;
  esac
done

mkdir -p "$(dirname "$LOG_FILE")"

# Ensure posted file exists so grep -f doesn't error (without bumping mtime if it already exists).
if [[ ! -f "$POSTED_FILE" ]]; then
  : > "$POSTED_FILE"
fi

# Get first ID in to_do.tsv that is not in posted.tsv.
# -F: fixed strings, -x: full-line match, -v: invert, -f: patterns from posted.tsv
NEXTID=""
if [[ -f "$TODO_FILE" ]]; then
  NEXTID="$(grep -Fvx -f "$POSTED_FILE" "$TODO_FILE" | awk 'BEGIN{srand()} {if (rand() < 1/NR) line=$0} END{print line}' | tr -d '\r' | xargs)"
fi

# Fallback: prioritize.tsv, first column only.
if [[ -z "$NEXTID" && -f "$PRIORITY_FILE" ]]; then
  NEXTID="$(grep -Fv -f "$POSTED_FILE" "$PRIORITY_FILE" | cut -f 1 | awk 'BEGIN{srand()} {if (rand() < 1/NR) line=$0} END{print line}' | tr -d '\r' | xargs)"
fi

if [[ -z "$NEXTID" ]]; then
  echo "$(date -u +"%Y-%m-%dT%H:%M:%SZ") no pending IDs; exiting" >> "$LOG_FILE"
  exit 0
fi

echo "$(date -u +"%Y-%m-%dT%H:%M:%SZ") posting $NEXTID" >> "$LOG_FILE"
if [[ "$DRY_RUN" -eq 1 ]]; then
  echo "python3 scripts/scholar_bluesky.py --scholar-id \"$NEXTID\" --single-post"
  exit 0
fi

python3 scripts/scholar_bluesky.py --scholar-id "$NEXTID" --single-post >> "$LOG_FILE" 2>&1
