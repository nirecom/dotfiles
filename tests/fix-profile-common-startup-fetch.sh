#!/usr/bin/env bash
# Test dispatcher: .profile_common startup fetch on Windows Git Bash (issue #333)
# Tests: .profile_common
# Tags: git-fetch, ssh, mingw, stdout-pollution, pgrep-guard, source-order, table-driven, pwsh-not-required, scope:common
# TL3 gap (what this test does NOT catch):
# - Real PATH resolution of `ssh` to Windows OpenSSH at source time in a live Git Bash session
# - Real ssh.exe/network prompt-blocking behavior (subprocess coverage stubs `git`, not `ssh`)
# - Real multi-session dedup: the guard matrix pins OSDIST/PID/ZSH_VERSION instead of racing shells
# - Real TTY/pty behaviour for the #335 Claude Code guard: `-t 1` is symbolically rewritten to a pinned predicate, so neither a real terminal nor a real Claude Code Bash-tool shell is exercised. This also leaves the "guard is not overbroad" direction (CLAUDECODE set + stdout IS a terminal => still fetches) unprovable end-to-end: Git Bash here has no script/expect/socat, winpty demands a tty on its own stdin, and /dev/ptmx|/dev/ttyS*|/dev/windows all report `[ -t 1 ]` false — so only the symbolic rows tty-with-claudecode-set-fetch / cc-value-metachar-tty-fetch / cc-value-with-space-tty-fetch cover it
# - #335 WSL-bridge scenario: Windows-native Claude Code does not propagate CLAUDECODE into a WSL shell (agents repo docs/ops.md:244-250, rules/test/claude-e2e.md:14-27), so the skip is unexercised there; WSL-native installs propagate it and are out of scope for this note
# Closest-to-action mitigation: gap checked at WORKFLOW_USER_VERIFIED preflight via bin/check-verification-gate.sh category: installer.

set -euo pipefail

DOTFILES_DIR="$(cd "$(dirname "$0")/.." && pwd)"
PROFILE="$DOTFILES_DIR/.profile_common"
PART_DIR="$(dirname "$0")/fix-profile-common-startup-fetch"
ERRORS=0

pass() { echo "  PASS: $1"; }
fail() { echo "  FAIL: $1"; ERRORS=$((ERRORS + 1)); }

# Table-driven comparison helper (skills/_shared/test-design/parser-regex-tests.md).
# Routed through fail() so ERRORS stays the single source of the exit code.
assert_eq() {
    local name="$1" want="$2" got="$3"
    if [ "$want" = "$got" ]; then
        pass "$name"
    else
        fail "$name — want=$(printf '%q' "$want") got=$(printf '%q' "$got")"
    fi
}

# The fetch guard lives in the `if type git ... fi` block at column 0. Scoping every
# Change-4 assertion to this slice is mandatory: `.profile_common` already carries an
# `if [ "$OSDIST" = "mingw" ]; then` line for the Windows OpenSSH PATH block (~line 190),
# so a whole-file grep for that string would be a false-green.
GUARD_REGION=$(sed -n '/^if type git/,/^fi$/p' "$PROFILE")

# Anchors shared by the static and behavioural parts.
SRC_LINE=$(grep -n '\. "\$_agents_dir/profile-snippet\.sh"' "$PROFILE" | head -n 1 | cut -d: -f1 || true)
SSH_CLEANUP_LINE=$(grep -n '^[[:space:]]*unset _ssh_keys' "$PROFILE" | head -n 1 | cut -d: -f1 || true)

# Change 1 order-check predicate (CPR-SSOT): shared by static-structure.sh
# (against the real PROFILE) and mutation-and-subprocess.sh (against mutated
# copies), so the "is it really after the whole SSH block" logic never forks.
# `unset _ssh_keys` alone is not enough (codex round-2 C1): a source line
# placed at column 0 but still textually between `unset _ssh_keys` and the
# SSH-setup block's own closing `fi` remains trapped inside the elif branch,
# so this also locates that `fi` and requires the source line to be after it.
# Prints "pass" or "fail:<reason>"; never exits non-zero.
change1_order_verdict() {
    local file="$1" src_line ssh_cleanup_line fi_line src_text
    src_line=$(grep -n '\. "\$_agents_dir/profile-snippet\.sh"' "$file" | head -n 1 | cut -d: -f1 || true)
    ssh_cleanup_line=$(grep -n '^[[:space:]]*unset _ssh_keys' "$file" | head -n 1 | cut -d: -f1 || true)
    if [ -z "$src_line" ] || [ -z "$ssh_cleanup_line" ]; then
        echo "fail:anchors-missing"; return
    fi
    src_text=$(sed -n "${src_line}p" "$file")
    if printf '%s\n' "$src_text" | grep -q '^[[:space:]]'; then
        echo "fail:indented"; return
    fi
    fi_line=$(awk -v start="$ssh_cleanup_line" 'NR>start && $0=="fi"{print NR; exit}' "$file")
    if [ -z "$fi_line" ]; then
        echo "fail:fi-not-found"; return
    fi
    if [ "$src_line" -gt "$fi_line" ]; then
        echo "pass"
    else
        echo "fail:before-block-end"
    fi
}

echo "=== #333: .profile_common startup fetch on Windows Git Bash ==="

# shellcheck source=./fix-profile-common-startup-fetch/static-structure.sh
. "$PART_DIR/static-structure.sh"
# shellcheck source=./fix-profile-common-startup-fetch/guard-behaviour.sh
. "$PART_DIR/guard-behaviour.sh"
# shellcheck source=./fix-profile-common-startup-fetch/mutation-and-subprocess.sh
. "$PART_DIR/mutation-and-subprocess.sh"
# shellcheck source=./fix-profile-common-startup-fetch/full-pipeline-integration.sh
. "$PART_DIR/full-pipeline-integration.sh"

echo ""
echo "=== Results ==="
if [ $ERRORS -eq 0 ]; then
    echo "All tests passed."
else
    echo "$ERRORS test(s) failed."
    exit 1
fi
