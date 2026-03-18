#!/bin/bash
# Scan content for private information patterns
# Usage:
#   check-private-info.sh [--stdin [label]] [file ...]
#   --stdin: read from stdin (optional label for output)
#   file args: scan named files
# Exit: 0 = clean, 1 = violations found, 2 = usage error

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
DOTFILES_DIR="$(dirname "$SCRIPT_DIR")"
ALLOWLIST="$DOTFILES_DIR/.private-info-allowlist"
BLOCKLIST="$DOTFILES_DIR/.private-info-blocklist"

VIOLATIONS=0
MODE=""
LABEL="stdin"

# Parse arguments
if [ $# -eq 0 ]; then
    echo "Usage: check-private-info.sh [--stdin [label]] [file ...]" >&2
    exit 2
fi

if [ "$1" = "--stdin" ]; then
    MODE="stdin"
    shift
    if [ $# -gt 0 ]; then
        LABEL="$1"
        shift
    fi
else
    MODE="files"
fi

# Load allowlist patterns (skip comments and empty lines)
ALLOW_PATTERNS=()
if [ -f "$ALLOWLIST" ]; then
    while IFS= read -r line; do
        line="${line%$'\r'}"
        [[ -z "$line" || "$line" =~ ^# ]] && continue
        ALLOW_PATTERNS+=("$line")
    done < "$ALLOWLIST"
fi

# Load blocklist patterns
BLOCK_PATTERNS=()
if [ -f "$BLOCKLIST" ]; then
    while IFS= read -r line; do
        line="${line%$'\r'}"
        [[ -z "$line" || "$line" =~ ^# ]] && continue
        BLOCK_PATTERNS+=("$line")
    done < "$BLOCKLIST"
fi

# Check if a match is allowlisted
is_allowed() {
    local file="$1"
    local matched="$2"
    for pattern in "${ALLOW_PATTERNS[@]+"${ALLOW_PATTERNS[@]}"}"; do
        if [[ "$pattern" == *:* ]]; then
            local file_pat="${pattern%%:*}"
            local val_pat="${pattern#*:}"
            if [[ "$file" == $file_pat ]] && [[ "$matched" =~ $val_pat ]]; then
                return 0
            fi
        else
            if [[ "$matched" =~ $pattern ]]; then
                return 0
            fi
        fi
    done
    return 1
}

# Scan a single line
scan_line() {
    local file="$1"
    local lineno="$2"
    local line="$3"

    # Private IPv4 ranges (RFC 1918)
    local ip_patterns=(
        '10\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}'
        '172\.(1[6-9]|2[0-9]|3[01])\.[0-9]{1,3}\.[0-9]{1,3}'
        '192\.168\.[0-9]{1,3}\.[0-9]{1,3}'
    )
    for ip_pat in "${ip_patterns[@]}"; do
        local tmpline="$line"
        while [[ "$tmpline" =~ ($ip_pat) ]]; do
            local ip="${BASH_REMATCH[1]}"
            if ! is_allowed "$file" "$ip"; then
                echo "$file:$lineno: [IPv4] $ip"
                VIOLATIONS=$((VIOLATIONS + 1))
            fi
            tmpline="${tmpline/"$ip"/}"
        done
    done

    # Email addresses
    if [[ "$line" =~ [a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,} ]]; then
        local email="${BASH_REMATCH[0]}"
        if ! is_allowed "$file" "$email"; then
            echo "$file:$lineno: [email] $email"
            VIOLATIONS=$((VIOLATIONS + 1))
        fi
    fi

    # MAC addresses
    if [[ "$line" =~ ([0-9A-Fa-f]{2}[:-]){5}[0-9A-Fa-f]{2} ]]; then
        local mac="${BASH_REMATCH[0]}"
        if ! is_allowed "$file" "$mac"; then
            echo "$file:$lineno: [MAC] $mac"
            VIOLATIONS=$((VIOLATIONS + 1))
        fi
    fi

    # Absolute local paths
    if [[ "$line" =~ /Users/[a-zA-Z] ]] || [[ "$line" =~ /home/[a-zA-Z] ]] || \
       [[ "$line" =~ [A-Z]:\\Users\\[a-zA-Z] ]] || [[ "$line" =~ [A-Z]:/Users/[a-zA-Z] ]]; then
        local path="${BASH_REMATCH[0]}"
        if [[ ! "$line" =~ /home/linuxbrew ]] && ! is_allowed "$file" "$path"; then
            echo "$file:$lineno: [path] $path"
            VIOLATIONS=$((VIOLATIONS + 1))
        fi
    fi

    # MSYS absolute paths: /<drive>/<name> (e.g. /c/..., /d/...)
    if [[ "$line" =~ (^|[^[:alnum:]_/])/[a-z]/[[:alnum:]_.-] ]]; then
        local mpath="${BASH_REMATCH[0]}"
        if ! is_allowed "$file" "$line"; then
            echo "$file:$lineno: [msys-path] $mpath"
            VIOLATIONS=$((VIOLATIONS + 1))
        fi
    fi

    # WSL absolute paths: /mnt/<drive>/<name>
    if [[ "$line" =~ (^|[^[:alnum:]_/])/mnt/[a-z]/[[:alnum:]_.-] ]]; then
        local wpath="${BASH_REMATCH[0]}"
        if ! is_allowed "$file" "$line"; then
            echo "$file:$lineno: [wsl-path] $wpath"
            VIOLATIONS=$((VIOLATIONS + 1))
        fi
    fi

    # Blocklist patterns
    for pattern in "${BLOCK_PATTERNS[@]+"${BLOCK_PATTERNS[@]}"}"; do
        if [[ "$line" =~ $pattern ]]; then
            local match="${BASH_REMATCH[0]}"
            if ! is_allowed "$file" "$match"; then
                echo "$file:$lineno: [blocklist] $match"
                VIOLATIONS=$((VIOLATIONS + 1))
            fi
        fi
    done
}

# Main: scan content
scan_content() {
    local file="$1"
    local lineno=0
    while IFS= read -r line || [ -n "$line" ]; do
        lineno=$((lineno + 1))
        scan_line "$file" "$lineno" "$line"
    done
}

if [ "$MODE" = "stdin" ]; then
    scan_content "$LABEL"
else
    for f in "$@"; do
        if [ -f "$f" ]; then
            scan_content "$f" < "$f"
        else
            echo "Warning: $f not found, skipping" >&2
        fi
    done
fi

if [ "$VIOLATIONS" -gt 0 ]; then
    echo ""
    echo "Found $VIOLATIONS private information violation(s)"
    exit 1
fi
exit 0
