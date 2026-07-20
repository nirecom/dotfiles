#!/usr/bin/env bash
# Test: git sync uses fetch+merge pattern on both platforms
# Tests: .profile_common, install/win/profile.ps1
# Tags: git-fetch, ssh, batchmode, zsh-guard, pwsh-not-required, scope:common
# L3 gap (what this test does NOT catch):
# - Real ssh reading /dev/tty for a passphrase during interactive WSL login
# - The terminal actually staying responsive (no hang on `wait`) after fetch
# Closest-to-action mitigation: gap checked at WORKFLOW_USER_VERIFIED preflight
# via bin/check-verification-gate.sh category: installer.
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
echo "--- Normal: profile.ps1 fetches agents repo ---"

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
echo "--- Security: each fetch target has BatchMode=yes subshell guard ---"

# Each grep matches guard env vars AND the specific git -C target on the same line,
# so comments or unrelated lines cannot satisfy the assertion.
if grep -qE '\(\s*GIT_TERMINAL_PROMPT=0\s+GIT_SSH_COMMAND=.ssh -o BatchMode=yes[^)]*git -C "\$DOTFILES_DIR" fetch' "$DOTFILES_DIR/.profile_common"; then
    pass ".profile_common: dotfiles fetch has BatchMode=yes subshell guard"
else
    fail ".profile_common: dotfiles fetch missing BatchMode=yes subshell guard"
fi

if grep -qE '\(\s*GIT_TERMINAL_PROMPT=0\s+GIT_SSH_COMMAND=.ssh -o BatchMode=yes[^)]*git -C "\$_xrepo" fetch' "$DOTFILES_DIR/.profile_common"; then
    pass ".profile_common: extra repos fetch has BatchMode=yes subshell guard"
else
    fail ".profile_common: extra repos fetch missing BatchMode=yes subshell guard"
fi

if grep -qE '\(\s*GIT_TERMINAL_PROMPT=0\s+GIT_SSH_COMMAND=.ssh -o BatchMode=yes[^)]*git -C "\$_agents_dir" fetch' "$DOTFILES_DIR/.profile_common"; then
    pass ".profile_common: agents fetch has BatchMode=yes subshell guard"
else
    fail ".profile_common: agents fetch missing BatchMode=yes subshell guard"
fi

echo ""
echo "--- Normal: shell guard handles zsh sessions (pgrep bash fix) ---"

if grep -q 'ZSH_VERSION' "$DOTFILES_DIR/.profile_common"; then
    pass ".profile_common: fetch guard includes ZSH_VERSION branch for zsh sessions"
else
    fail ".profile_common: fetch guard missing ZSH_VERSION branch (zsh sessions will skip fetch)"
fi

# NOTE (codex round 2, C1): the pgrep -fo bash guard is RETAINED by design (bash path
# unchanged); only the ZSH_VERSION fallback is added. Do NOT assert pgrep absence — that
# would contradict the implementation. Assert instead that the zsh fallback co-exists on
# the same guard line as the retained pgrep-derived PID condition.
if grep -qE 'if \{ \[ -n "\$PID" \].*\}\s*\|\|\s*\[ -n "\$\{ZSH_VERSION-\}" \]' "$DOTFILES_DIR/.profile_common"; then
    pass ".profile_common: guard combines retained bash pgrep condition with ZSH_VERSION fallback"
else
    fail ".profile_common: guard line does not combine bash condition with ZSH_VERSION fallback"
fi

echo ""
echo "=== Results ==="
if [ $ERRORS -eq 0 ]; then
    echo "All tests passed."
else
    echo "$ERRORS test(s) failed."
    exit 1
fi
