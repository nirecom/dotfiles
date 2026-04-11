#!/bin/bash
# session-sync.sh - Sync Claude Code session history across machines
# Usage:
#   session-sync.sh push    # Commit and push session data
#   session-sync.sh pull    # Pull latest session data
#   session-sync.sh status  # Show sync status
#   session-sync.sh reset   # Force-sync local to remote (for initial setup or recovery)

set -euo pipefail

ACTION="${1:-}"
CLAUDE_DIR="$HOME/.claude"
_QUIET=0
_TOAST=0

# Parse options
shift || true
while [ $# -gt 0 ]; do
    case "$1" in
        --claude-dir) CLAUDE_DIR="$2"; shift 2 ;;
        --quiet) _QUIET=1; shift ;;
        --toast) _TOAST=1; shift ;;
        *) shift ;;
    esac
done

PROJECTS_DIR="$CLAUDE_DIR/projects"

if [ ! -d "$PROJECTS_DIR/.git" ]; then
    echo "Session sync not initialized. Run install.sh or install/linux/session-sync-init.sh first." >&2
    exit 1
fi

_toast() {
    local msg="$1"
    if command -v powershell.exe >/dev/null 2>&1; then
        powershell.exe -NoProfile -Command "[void][Windows.UI.Notifications.ToastNotificationManager,Windows.UI.Notifications,ContentType=WindowsRuntime];[void][Windows.Data.Xml.Dom.XmlDocument,Windows.Data.Xml.Dom.XmlDocument,ContentType=WindowsRuntime];\$x=[Windows.UI.Notifications.ToastNotificationManager]::GetTemplateContent(1);\$t=\$x.GetElementsByTagName('text');\$t.Item(0).AppendChild(\$x.CreateTextNode('session-sync'))|Out-Null;\$t.Item(1).AppendChild(\$x.CreateTextNode('$msg'))|Out-Null;[Windows.UI.Notifications.ToastNotificationManager]::CreateToastNotifier('PowerShell').Show([Windows.UI.Notifications.ToastNotification]::new(\$x))" 2>/dev/null
    elif command -v osascript >/dev/null 2>&1; then
        osascript -e "display notification \"$msg\" with title \"session-sync\""
    elif command -v notify-send >/dev/null 2>&1; then
        notify-send "session-sync" "$msg"
    fi
}


case "$ACTION" in
    push)
        # Warn if Claude Code is running
        if [ "$_QUIET" = "0" ] && pgrep -x "claude" >/dev/null 2>&1; then
            echo "WARNING: Claude Code is running. Close all sessions before push to ensure latest data is saved." >&2
        fi
        # Copy history.jsonl into sync area
        cp "$CLAUDE_DIR/history.jsonl" "$PROJECTS_DIR/.history.jsonl" 2>/dev/null || true
        git -C "$PROJECTS_DIR" add .
        if [ -z "$(git -C "$PROJECTS_DIR" status --porcelain)" ]; then
            echo "No changes to push."
            exit 0
        fi
        timestamp=$(date "+%Y-%m-%d %H:%M")
        git -C "$PROJECTS_DIR" commit -q -m "sync: $(hostname -s) $timestamp"
        git -C "$PROJECTS_DIR" pull --rebase origin main 2>/dev/null || true
        if [ "$_QUIET" = "1" ]; then
            if git -C "$PROJECTS_DIR" push -u origin main 2>&1; then
                [ "$_TOAST" = "1" ] && _toast "push complete"
            else
                [ "$_TOAST" = "1" ] && _toast "push failed"
            fi
        else
            git -C "$PROJECTS_DIR" push -u origin main
            echo "Pushed session data."
        fi
        ;;
    pull)
        git -C "$PROJECTS_DIR" pull --rebase
        # Merge remote history with local (dedup, preserve order)
        if [ -f "$PROJECTS_DIR/.history.jsonl" ]; then
            if [ -f "$CLAUDE_DIR/history.jsonl" ]; then
                cat "$PROJECTS_DIR/.history.jsonl" "$CLAUDE_DIR/history.jsonl"
            else
                cat "$PROJECTS_DIR/.history.jsonl"
            fi | awk '!seen[$0]++' > "$CLAUDE_DIR/history.jsonl.tmp"
            mv "$CLAUDE_DIR/history.jsonl.tmp" "$CLAUDE_DIR/history.jsonl"
        fi
        echo "Pulled session data."
        ;;
    status)
        git -C "$PROJECTS_DIR" status
        ;;
    reset)
        git -C "$PROJECTS_DIR" fetch origin main
        git -C "$PROJECTS_DIR" reset --hard origin/main
        # Restore mtime from JSONL timestamps (git doesn't preserve mtime)
        find "$PROJECTS_DIR" -name "*.jsonl" ! -name ".history.jsonl" | while read -r f; do
            ts=$(tail -1 "$f" 2>/dev/null | grep -o '"timestamp":"[^"]*"' | head -1 | cut -d'"' -f4) || true
            [ -z "$ts" ] && ts=$(head -1 "$f" 2>/dev/null | grep -o '"timestamp":"[^"]*"' | head -1 | cut -d'"' -f4) || true
            [ -n "$ts" ] && touch -d "$ts" "$f" 2>/dev/null || true
        done
        # Merge remote history with local (dedup, preserve order)
        if [ -f "$PROJECTS_DIR/.history.jsonl" ]; then
            if [ -f "$CLAUDE_DIR/history.jsonl" ]; then
                cat "$PROJECTS_DIR/.history.jsonl" "$CLAUDE_DIR/history.jsonl"
            else
                cat "$PROJECTS_DIR/.history.jsonl"
            fi | awk '!seen[$0]++' > "$CLAUDE_DIR/history.jsonl.tmp"
            mv "$CLAUDE_DIR/history.jsonl.tmp" "$CLAUDE_DIR/history.jsonl"
        fi
        echo "Reset to remote state."
        ;;
    *)
        echo "Usage: session-sync.sh {push|pull|status|reset}" >&2
        exit 1
        ;;
esac
