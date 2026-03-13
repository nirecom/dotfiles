#!/bin/bash
# Update the private repo whitelist from GitHub
# Usage: ./bin/update-private-repos.sh
# Output: .context-private/private-repos.txt

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
DOTFILES_DIR="$(dirname "$SCRIPT_DIR")"
OUTPUT_DIR="$DOTFILES_DIR/.context-private"
OUTPUT_FILE="$OUTPUT_DIR/private-repos.txt"

if ! command -v gh >/dev/null 2>&1; then
    echo "Error: gh (GitHub CLI) is not installed" >&2
    exit 1
fi

if ! gh auth status >/dev/null 2>&1; then
    echo "Error: gh is not authenticated. Run 'gh auth login' first" >&2
    exit 1
fi

mkdir -p "$OUTPUT_DIR"

gh repo list --json nameWithOwner,isPrivate --limit 1000 \
    --jq '.[] | select(.isPrivate) | .nameWithOwner' \
    | sort > "$OUTPUT_FILE"

COUNT=$(wc -l < "$OUTPUT_FILE")
echo "Updated $OUTPUT_FILE ($COUNT private repos)"
