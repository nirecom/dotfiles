#!/bin/bash
# session-sync.sh - Sync Claude Code session history across machines
# Usage:
#   session-sync.sh push    # Commit and push session data
#   session-sync.sh pull    # Pull latest session data
#   session-sync.sh status  # Show sync status

set -euo pipefail

ACTION="${1:-}"
CLAUDE_DIR="$HOME/.claude"

# Parse --claude-dir for testing
shift || true
while [ $# -gt 0 ]; do
    case "$1" in
        --claude-dir) CLAUDE_DIR="$2"; shift 2 ;;
        *) shift ;;
    esac
done

PROJECTS_DIR="$CLAUDE_DIR/projects"

if [ ! -d "$PROJECTS_DIR/.git" ]; then
    echo "Session sync not initialized. Run install.sh or install/linux/session-sync-init.sh first." >&2
    exit 1
fi

case "$ACTION" in
    push)
        # Warn if Claude Code is running
        if pgrep -x "claude" >/dev/null 2>&1; then
            echo "WARNING: Claude Code is running. Close all sessions before push to ensure latest data is saved." >&2
        fi
        git -C "$PROJECTS_DIR" add .
        if [ -z "$(git -C "$PROJECTS_DIR" status --porcelain)" ]; then
            echo "No changes to push."
            exit 0
        fi
        timestamp=$(date "+%Y-%m-%d %H:%M")
        git -C "$PROJECTS_DIR" commit -m "sync: $(hostname -s) $timestamp"
        git -C "$PROJECTS_DIR" push
        echo "Pushed session data."
        ;;
    pull)
        git -C "$PROJECTS_DIR" pull --rebase
        echo "Pulled session data."
        ;;
    status)
        git -C "$PROJECTS_DIR" status
        ;;
    *)
        echo "Usage: session-sync.sh {push|pull|status}" >&2
        exit 1
        ;;
esac
