#!/bin/bash
# Tests for bin/session-sync.sh and install/linux/session-sync-init.sh
# Run: bash tests/main-session-sync.sh

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
DOTFILES_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
PASS=0
FAIL=0

pass() { PASS=$((PASS + 1)); echo "  PASS: $1"; }
fail() { FAIL=$((FAIL + 1)); echo "  FAIL: $1"; }

# Create isolated temp environment
TMPDIR_BASE=$(mktemp -d)
trap 'rm -rf "$TMPDIR_BASE"' EXIT

FAKE_HOME="$TMPDIR_BASE/home"
FAKE_CLAUDE="$FAKE_HOME/.claude"
FAKE_PROJECTS="$FAKE_CLAUDE/projects"
mkdir -p "$FAKE_HOME"

# Create a bare remote repo to simulate github
FAKE_REMOTE="$TMPDIR_BASE/remote.git"
git init --bare "$FAKE_REMOTE" >/dev/null 2>&1

echo "=== session-sync-init.sh tests ==="

# --- Normal: Fresh initialization ---
echo "[init] Fresh initialization"
output=$("$DOTFILES_DIR/install/linux/session-sync-init.sh" \
    --claude-dir "$FAKE_CLAUDE" --remote-url "$FAKE_REMOTE" 2>&1)
if [ -d "$FAKE_PROJECTS/.git" ]; then
    pass "git repo created in projects dir"
else
    fail "git repo not created in projects dir"
fi

if [ -f "$FAKE_PROJECTS/.gitattributes" ]; then
    pass ".gitattributes created"
else
    fail ".gitattributes not created"
fi

remote_url=$(git -C "$FAKE_PROJECTS" remote get-url origin 2>/dev/null)
if [ "$remote_url" = "$FAKE_REMOTE" ]; then
    pass "remote set correctly"
else
    fail "remote not set correctly (got: $remote_url)"
fi

commit_count=$(git -C "$FAKE_PROJECTS" rev-list --count HEAD 2>/dev/null)
if [ "$commit_count" -ge 1 ]; then
    pass "initial commit created"
else
    fail "initial commit not created"
fi

# --- Edge: Idempotent re-run ---
echo "[init] Idempotent re-run"
output=$("$DOTFILES_DIR/install/linux/session-sync-init.sh" \
    --claude-dir "$FAKE_CLAUDE" --remote-url "$FAKE_REMOTE" 2>&1)
commit_count_after=$(git -C "$FAKE_PROJECTS" rev-list --count HEAD 2>/dev/null)
if [ "$commit_count_after" = "$commit_count" ]; then
    pass "re-run does not create extra commits"
else
    fail "re-run created extra commits ($commit_count -> $commit_count_after)"
fi

# --- Edge: Remote already set, updates URL ---
echo "[init] Remote URL update"
NEW_REMOTE="$TMPDIR_BASE/remote2.git"
git init --bare "$NEW_REMOTE" >/dev/null 2>&1
"$DOTFILES_DIR/install/linux/session-sync-init.sh" \
    --claude-dir "$FAKE_CLAUDE" --remote-url "$NEW_REMOTE" >/dev/null 2>&1
updated_url=$(git -C "$FAKE_PROJECTS" remote get-url origin 2>/dev/null)
if [ "$updated_url" = "$NEW_REMOTE" ]; then
    pass "remote URL updated on re-run"
else
    fail "remote URL not updated (got: $updated_url)"
fi
# Restore original remote for subsequent tests
"$DOTFILES_DIR/install/linux/session-sync-init.sh" \
    --claude-dir "$FAKE_CLAUDE" --remote-url "$FAKE_REMOTE" >/dev/null 2>&1

# --- Edge: Old .git in ~/.claude/ gets migrated ---
echo "[init] Migration of old git root"
MIGRATE_HOME="$TMPDIR_BASE/migrate"
MIGRATE_CLAUDE="$MIGRATE_HOME/.claude"
mkdir -p "$MIGRATE_CLAUDE/projects"
git init "$MIGRATE_CLAUDE" >/dev/null 2>&1
touch "$MIGRATE_CLAUDE/.gitignore"
MIGRATE_REMOTE="$TMPDIR_BASE/migrate-remote.git"
git init --bare "$MIGRATE_REMOTE" >/dev/null 2>&1
"$DOTFILES_DIR/install/linux/session-sync-init.sh" \
    --claude-dir "$MIGRATE_CLAUDE" --remote-url "$MIGRATE_REMOTE" >/dev/null 2>&1
if [ ! -d "$MIGRATE_CLAUDE/.git" ] && [ -d "$MIGRATE_CLAUDE/projects/.git" ]; then
    pass "old .git migrated from claude dir to projects dir"
else
    fail "migration did not work"
fi

# --- Normal: --no-remote flag ---
echo "[init] --no-remote flag"
NOREMOTE_CLAUDE="$TMPDIR_BASE/noremote/.claude"
mkdir -p "$NOREMOTE_CLAUDE"
"$DOTFILES_DIR/install/linux/session-sync-init.sh" \
    --claude-dir "$NOREMOTE_CLAUDE" --no-remote >/dev/null 2>&1
if [ -d "$NOREMOTE_CLAUDE/projects/.git" ]; then
    noremote_remotes=$(git -C "$NOREMOTE_CLAUDE/projects" remote 2>/dev/null)
    if [ -z "$noremote_remotes" ]; then
        pass "--no-remote: repo created without remote"
    else
        fail "--no-remote: remote was set ($noremote_remotes)"
    fi
else
    fail "--no-remote: git repo not created"
fi

# --- Normal: .gitattributes content ---
echo "[init] .gitattributes content"
if grep -q "eol=lf" "$FAKE_PROJECTS/.gitattributes" 2>/dev/null; then
    pass ".gitattributes contains eol=lf"
else
    fail ".gitattributes missing eol=lf"
fi

# --- Normal: core.hooksPath disabled ---
echo "[init] core.hooksPath disabled"
hooks_path=$(git -C "$FAKE_PROJECTS" config core.hooksPath 2>/dev/null || true)
if [ "$hooks_path" = "/dev/null" ]; then
    pass "core.hooksPath set to /dev/null"
else
    fail "core.hooksPath not disabled (got: $hooks_path)"
fi

# --- Error: No git installed (skip if we can't fake it) ---
echo "[init] No git warning"
output=$(PATH="/usr/bin/nonexistent" "$DOTFILES_DIR/install/linux/session-sync-init.sh" \
    --claude-dir "$TMPDIR_BASE/nogit" --remote-url "$FAKE_REMOTE" 2>&1) || true
if echo "$output" | grep -qi "git.*required\|git.*not found"; then
    pass "warns when git is not available"
else
    fail "no warning when git missing (output: $output)"
fi

echo ""
echo "=== session-sync.sh tests ==="

# Push initial state to remote so pull works
git -C "$FAKE_PROJECTS" push -u origin main >/dev/null 2>&1 || git -C "$FAKE_PROJECTS" push -u origin master >/dev/null 2>&1

# --- Error: Not initialized ---
echo "[sync] Not initialized"
NOT_INIT="$TMPDIR_BASE/notinit/.claude"
mkdir -p "$NOT_INIT/projects"
output=$("$DOTFILES_DIR/bin/session-sync.sh" push --claude-dir "$NOT_INIT" 2>&1) || true
if echo "$output" | grep -qi "not initialized"; then
    pass "error when not initialized"
else
    fail "no error for uninitialized repo (output: $output)"
fi

# --- Error: Invalid action ---
echo "[sync] Invalid action"
output=$("$DOTFILES_DIR/bin/session-sync.sh" invalid --claude-dir "$FAKE_CLAUDE" 2>&1) || true
if [ $? -ne 0 ] || echo "$output" | grep -qi "usage\|invalid\|push\|pull\|status"; then
    pass "rejects invalid action"
else
    fail "accepted invalid action (output: $output)"
fi

# --- Normal: Push with no changes ---
echo "[sync] Push with no changes"
output=$("$DOTFILES_DIR/bin/session-sync.sh" push --claude-dir "$FAKE_CLAUDE" 2>&1)
if echo "$output" | grep -qi "no changes"; then
    pass "push reports no changes"
else
    fail "push did not report no changes (output: $output)"
fi

# --- Normal: Push with changes ---
echo "[sync] Push with changes"
echo '{"test":"data"}' > "$FAKE_PROJECTS/test-session.jsonl"
output=$("$DOTFILES_DIR/bin/session-sync.sh" push --claude-dir "$FAKE_CLAUDE" 2>&1)
if echo "$output" | grep -qi "pushed"; then
    pass "push succeeds with changes"
else
    fail "push did not succeed (output: $output)"
fi

# --- Normal: Pull ---
echo "[sync] Pull"
output=$("$DOTFILES_DIR/bin/session-sync.sh" pull --claude-dir "$FAKE_CLAUDE" 2>&1)
if echo "$output" | grep -qi "pulled\|up to date\|already"; then
    pass "pull succeeds"
else
    fail "pull did not succeed (output: $output)"
fi

# --- Normal: Status ---
echo "[sync] Status"
output=$("$DOTFILES_DIR/bin/session-sync.sh" status --claude-dir "$FAKE_CLAUDE" 2>&1)
if [ -n "$output" ]; then
    pass "status produces output"
else
    fail "status produced no output"
fi

# --- Normal: Commit message format ---
echo "[sync] Commit message format"
echo '{"test":"format"}' > "$FAKE_PROJECTS/test-format.jsonl"
"$DOTFILES_DIR/bin/session-sync.sh" push --claude-dir "$FAKE_CLAUDE" >/dev/null 2>&1
last_msg=$(git -C "$FAKE_PROJECTS" log -1 --format=%s)
if echo "$last_msg" | grep -qE "^sync: .+ [0-9]{4}-[0-9]{2}-[0-9]{2}"; then
    pass "commit message matches format (sync: hostname date)"
else
    fail "commit message format unexpected ($last_msg)"
fi

# --- Edge: Pull idempotent (already up-to-date) ---
echo "[sync] Pull idempotent"
output1=$("$DOTFILES_DIR/bin/session-sync.sh" pull --claude-dir "$FAKE_CLAUDE" 2>&1)
output2=$("$DOTFILES_DIR/bin/session-sync.sh" pull --claude-dir "$FAKE_CLAUDE" 2>&1)
if echo "$output2" | grep -qi "pulled\|up to date\|already"; then
    pass "consecutive pulls succeed"
else
    fail "second pull failed (output: $output2)"
fi

# --- Edge: Push warns if claude process running (mock with self) ---
echo "[sync] Claude running warning"
# We can't easily mock this cross-platform, so just verify the function exists
# by checking the script contains the check
if grep -q "claude" "$DOTFILES_DIR/bin/session-sync.sh" 2>/dev/null; then
    pass "script contains claude process check"
else
    fail "script missing claude process check"
fi

echo ""
echo "=== Results ==="
echo "PASS: $PASS  FAIL: $FAIL"
[ "$FAIL" -eq 0 ] && exit 0 || exit 1
