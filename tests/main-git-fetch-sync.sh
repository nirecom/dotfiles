#!/usr/bin/env bash
# Test: git sync uses fetch+merge pattern on both platforms
set -euo pipefail

DOTFILES_DIR="$(cd "$(dirname "$0")/.." && pwd)"
ERRORS=0

pass() { echo "  PASS: $1"; }
fail() { echo "  FAIL: $1"; ERRORS=$((ERRORS + 1)); }

echo "=== git fetch+merge sync tests ==="

# --- .profile_common (macOS/Linux) ---
echo ""
echo "--- Normal: .profile_common uses fetch+merge pattern ---"

# Check sync block only (between "if git is installed" and next "fi" block), excluding aliases
SYNC_BLOCK=$(sed -n '/if type git/,/^fi$/p' "$DOTFILES_DIR/.profile_common")
if echo "$SYNC_BLOCK" | grep -q 'git pull\|git.*pull'; then
    fail ".profile_common sync block still uses 'git pull'"
else
    pass ".profile_common: sync block has no 'git pull'"
fi

if grep -qE 'git -C "\$DOTFILES_DIR" fetch' "$DOTFILES_DIR/.profile_common"; then
    pass ".profile_common: uses 'git fetch' for dotfiles"
else
    fail ".profile_common: missing 'git fetch' for dotfiles"
fi

if grep -q 'merge --ff-only' "$DOTFILES_DIR/.profile_common"; then
    pass ".profile_common: uses 'merge --ff-only'"
else
    fail ".profile_common: missing 'merge --ff-only'"
fi

echo ""
echo "--- Normal: .profile_common has timeout ---"

if grep -q 'timeout' "$DOTFILES_DIR/.profile_common"; then
    pass ".profile_common: has timeout"
else
    fail ".profile_common: missing timeout"
fi

echo ""
echo "--- Normal: .profile_common checks fetch exit code before merge ---"

# Parallel pattern: fetch exit code captured in _rc_df, merge gated on it
if grep -q '_rc_df' "$DOTFILES_DIR/.profile_common"; then
    pass ".profile_common: merge is gated on fetch exit code (_rc_df)"
else
    fail ".profile_common: merge not gated on fetch exit code"
fi

echo ""
echo "--- Normal: .profile_common fetches agents ---"

if grep -qE 'dirname "\$DOTFILES_DIR"\)/agents|/agents"' "$DOTFILES_DIR/.profile_common"; then
    pass ".profile_common: fetches agents repo (sibling)"
else
    fail ".profile_common: missing agents fetch"
fi

# --- profile.ps1 (Windows) ---
echo ""
echo "--- Normal: profile.ps1 uses fetch+merge pattern ---"

if grep -q 'git fetch' "$DOTFILES_DIR/install/win/profile.ps1"; then
    pass "profile.ps1: uses 'git fetch'"
else
    fail "profile.ps1: missing 'git fetch'"
fi

if grep -q 'merge --ff-only' "$DOTFILES_DIR/install/win/profile.ps1"; then
    pass "profile.ps1: uses 'merge --ff-only'"
else
    fail "profile.ps1: missing 'merge --ff-only'"
fi

echo ""
echo "--- Normal: profile.ps1 checks fetch exit code ---"

if grep -q 'ExitCode' "$DOTFILES_DIR/install/win/profile.ps1"; then
    pass "profile.ps1: checks ExitCode"
else
    fail "profile.ps1: missing ExitCode check"
fi

echo ""
echo "--- Normal: profile.ps1 fetches dotfiles-private and agents ---"

if grep -q 'dotfiles-private' "$DOTFILES_DIR/install/win/profile.ps1"; then
    pass "profile.ps1: fetches dotfiles-private"
else
    fail "profile.ps1: missing dotfiles-private fetch"
fi

if grep -q 'AgentsDir' "$DOTFILES_DIR/install/win/profile.ps1"; then
    pass "profile.ps1: fetches agents repo"
else
    fail "profile.ps1: missing agents fetch"
fi

# --- Edge: no bare 'git pull' in sync blocks ---
echo ""
echo "--- Edge: no git pull in either platform's sync block ---"

if echo "$SYNC_BLOCK" | grep -q 'git pull'; then
    fail ".profile_common: sync block still contains 'git pull'"
else
    pass ".profile_common: sync block has no 'git pull'"
fi

if grep -q 'git pull' "$DOTFILES_DIR/install/win/profile.ps1"; then
    fail "profile.ps1: contains 'git pull'"
else
    pass "profile.ps1: no 'git pull'"
fi

# --- Cross-platform: both use same timeout value ---
echo ""
echo "--- Cross-platform: both have 3s timeout ---"

if grep -q 'timeout 3\|timeout=3' "$DOTFILES_DIR/.profile_common"; then
    pass ".profile_common: 3s timeout"
else
    fail ".profile_common: missing 3s timeout"
fi

if grep -q '3000' "$DOTFILES_DIR/install/win/profile.ps1"; then
    pass "profile.ps1: 3s timeout (3000ms)"
else
    fail "profile.ps1: missing 3s timeout"
fi

echo ""
echo "=== Results ==="
if [ $ERRORS -eq 0 ]; then
    echo "All tests passed."
else
    echo "$ERRORS test(s) failed."
    exit 1
fi
