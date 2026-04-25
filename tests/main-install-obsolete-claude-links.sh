#!/usr/bin/env bash
# Test: install-obsolete cleanup for stale claude-global symlinks
# Verifies the remove_claude_global_symlinks logic intended for install-obsolete scripts.

set -uo pipefail

# --- Portable timeout wrapper (macOS has no `timeout`) ---
run_with_timeout() {
    if command -v timeout >/dev/null 2>&1; then
        timeout 120 "$@"
    else
        perl -e 'alarm 120; exec @ARGV' -- "$@"
    fi
}

# --- Function under test (mirrors logic to be added to install-obsolete scripts) ---
remove_claude_global_symlinks() {
    local claude_dir="$1"
    [ -d "$claude_dir" ] || return 0
    for _name in CLAUDE.md settings.json skills rules agents; do
        _link="$claude_dir/$_name"
        if [ -L "$_link" ]; then
            _target=$(readlink "$_link")
            case "$_target" in
                */dotfiles/claude-global/*)
                    rm "$_link"
                    ;;
            esac
        fi
    done
}

# Removes leftover dotfiles/claude-global/ directory only when HEAD confirms
# claude-global/ is no longer tracked (post-agents-split).
remove_claude_global_dir() {
    local home_dir="$1"
    local cg_dir="$home_dir/dotfiles/claude-global"
    if [ -d "$cg_dir" ] && [ -d "$home_dir/dotfiles/.git" ]; then
        if ! git -C "$home_dir/dotfiles" ls-tree -r HEAD claude-global 2>/dev/null | grep -q .; then
            rm -rf "$cg_dir"
        fi
    fi
}

# --- Test harness ---
PASS=0
FAIL=0
SKIP=0
FAIL_NAMES=()
SKIP_NAMES=()

assert() {
    local name="$1"
    local cond="$2"
    if [ "$cond" = "0" ]; then
        PASS=$((PASS + 1))
        echo "PASS: $name"
    else
        FAIL=$((FAIL + 1))
        FAIL_NAMES+=("$name")
        echo "FAIL: $name"
    fi
}

skip() {
    local name="$1"
    local reason="$2"
    SKIP=$((SKIP + 1))
    SKIP_NAMES+=("$name ($reason)")
    echo "SKIP: $name — $reason"
}

make_tmpdir() {
    mktemp -d "${TMPDIR:-/tmp}/claude-links-test-XXXXXX"
}

# Detect whether the current environment can create real symlinks.
# On MSYS/MinGW without MSYS=winsymlinks:nativestrict, ln -s silently falls
# back to copying the target — which would invalidate every test below.
SYMLINKS_SUPPORTED=1
__symlink_probe_dir=$(make_tmpdir)
__probe_target="$__symlink_probe_dir/target.txt"
echo "x" > "$__probe_target"
ln -s "$__probe_target" "$__symlink_probe_dir/link" 2>/dev/null
if [ ! -L "$__symlink_probe_dir/link" ]; then
    SYMLINKS_SUPPORTED=0
fi
rm -rf "$__symlink_probe_dir"

# --- Test 1: symlink to */dotfiles/claude-global/CLAUDE.md is removed ---
test_dotfiles_symlink_removed() {
    if [ "$SYMLINKS_SUPPORTED" = "0" ]; then
        skip "dotfiles symlink removed" "symlinks not supported in this shell"
        return
    fi
    local tmp claude_dir target rc
    tmp=$(make_tmpdir)
    claude_dir="$tmp/.claude"
    mkdir -p "$claude_dir"
    target="$tmp/dotfiles/claude-global/CLAUDE.md"
    mkdir -p "$(dirname "$target")"
    echo "real" > "$target"
    ln -s "$target" "$claude_dir/CLAUDE.md"

    remove_claude_global_symlinks "$claude_dir"
    rc=0
    [ -e "$claude_dir/CLAUDE.md" ] || [ -L "$claude_dir/CLAUDE.md" ] && rc=1
    assert "dotfiles symlink removed" "$rc"
    rm -rf "$tmp"
}

# --- Test 2: all 5 names symlinked, all removed ---
test_all_five_removed() {
    if [ "$SYMLINKS_SUPPORTED" = "0" ]; then
        skip "all 5 names removed" "symlinks not supported in this shell"
        return
    fi
    local tmp claude_dir base rc=0
    tmp=$(make_tmpdir)
    claude_dir="$tmp/.claude"
    mkdir -p "$claude_dir"
    base="$tmp/dotfiles/claude-global"
    mkdir -p "$base/skills" "$base/rules" "$base/agents"
    echo "x" > "$base/CLAUDE.md"
    echo "{}" > "$base/settings.json"
    ln -s "$base/CLAUDE.md" "$claude_dir/CLAUDE.md"
    ln -s "$base/settings.json" "$claude_dir/settings.json"
    ln -s "$base/skills" "$claude_dir/skills"
    ln -s "$base/rules" "$claude_dir/rules"
    ln -s "$base/agents" "$claude_dir/agents"

    remove_claude_global_symlinks "$claude_dir"
    for n in CLAUDE.md settings.json skills rules agents; do
        if [ -L "$claude_dir/$n" ] || [ -e "$claude_dir/$n" ]; then
            rc=1
        fi
    done
    assert "all 5 names removed" "$rc"
    rm -rf "$tmp"
}

# --- Test 3: symlink to */agents/claude-global/CLAUDE.md is kept ---
test_agents_path_kept() {
    if [ "$SYMLINKS_SUPPORTED" = "0" ]; then
        skip "agents-path symlink kept" "symlinks not supported in this shell"
        return
    fi
    local tmp claude_dir target rc=0
    tmp=$(make_tmpdir)
    claude_dir="$tmp/.claude"
    mkdir -p "$claude_dir"
    target="$tmp/agents/claude-global/CLAUDE.md"
    mkdir -p "$(dirname "$target")"
    echo "real" > "$target"
    ln -s "$target" "$claude_dir/CLAUDE.md"

    remove_claude_global_symlinks "$claude_dir"
    [ -L "$claude_dir/CLAUDE.md" ] || rc=1
    assert "agents-path symlink kept" "$rc"
    rm -rf "$tmp"
}

# --- Test 4: symlink to unrelated path is kept ---
test_unrelated_kept() {
    if [ "$SYMLINKS_SUPPORTED" = "0" ]; then
        skip "unrelated symlink kept" "symlinks not supported in this shell"
        return
    fi
    local tmp claude_dir target rc=0
    tmp=$(make_tmpdir)
    claude_dir="$tmp/.claude"
    mkdir -p "$claude_dir"
    target="$tmp/some/other/path/CLAUDE.md"
    mkdir -p "$(dirname "$target")"
    echo "real" > "$target"
    ln -s "$target" "$claude_dir/CLAUDE.md"

    remove_claude_global_symlinks "$claude_dir"
    [ -L "$claude_dir/CLAUDE.md" ] || rc=1
    assert "unrelated symlink kept" "$rc"
    rm -rf "$tmp"
}

# --- Test 5: regular file (not symlink) is kept ---
test_regular_file_kept() {
    local tmp claude_dir rc=0
    tmp=$(make_tmpdir)
    claude_dir="$tmp/.claude"
    mkdir -p "$claude_dir"
    echo "real content" > "$claude_dir/CLAUDE.md"

    remove_claude_global_symlinks "$claude_dir"
    [ -f "$claude_dir/CLAUDE.md" ] && [ ! -L "$claude_dir/CLAUDE.md" ] || rc=1
    assert "regular file kept" "$rc"
    rm -rf "$tmp"
}

# --- Test 6: claude_dir does not exist — no error ---
test_missing_dir_no_error() {
    local tmp claude_dir rc
    tmp=$(make_tmpdir)
    claude_dir="$tmp/does-not-exist"

    remove_claude_global_symlinks "$claude_dir"
    rc=$?
    assert "missing dir does not error" "$rc"
    rm -rf "$tmp"
}

# --- Test 7: broken symlink whose target path matches pattern is removed ---
test_broken_symlink_pattern_match_removed() {
    if [ "$SYMLINKS_SUPPORTED" = "0" ]; then
        skip "broken symlink with matching pattern removed" "symlinks not supported in this shell"
        return
    fi
    local tmp claude_dir target rc=0
    tmp=$(make_tmpdir)
    claude_dir="$tmp/.claude"
    mkdir -p "$claude_dir"
    target="$tmp/dotfiles/claude-global/CLAUDE.md"
    # Don't create the target — symlink will be broken
    ln -s "$target" "$claude_dir/CLAUDE.md"

    remove_claude_global_symlinks "$claude_dir"
    if [ -L "$claude_dir/CLAUDE.md" ]; then
        rc=1
    fi
    assert "broken symlink with matching pattern removed" "$rc"
    rm -rf "$tmp"
}

# --- Test 8: idempotency — running twice does not error ---
test_idempotent() {
    if [ "$SYMLINKS_SUPPORTED" = "0" ]; then
        skip "idempotent (two runs ok)" "symlinks not supported in this shell"
        return
    fi
    local tmp claude_dir target rc=0
    tmp=$(make_tmpdir)
    claude_dir="$tmp/.claude"
    mkdir -p "$claude_dir"
    target="$tmp/dotfiles/claude-global/CLAUDE.md"
    mkdir -p "$(dirname "$target")"
    echo "real" > "$target"
    ln -s "$target" "$claude_dir/CLAUDE.md"

    remove_claude_global_symlinks "$claude_dir" || rc=1
    remove_claude_global_symlinks "$claude_dir" || rc=1
    if [ -L "$claude_dir/CLAUDE.md" ]; then
        rc=1
    fi
    assert "idempotent (two runs ok)" "$rc"
    rm -rf "$tmp"
}

# --- Helper for C-tests: build a repo with optional tracked claude-global/ ---
make_repo_with_claude_global() {
    local home_dir="$1" tracked="$2"
    local repo="$home_dir/dotfiles"
    mkdir -p "$repo"
    git -C "$repo" init -q
    git -C "$repo" config user.email "test@example.com"
    git -C "$repo" config user.name "Test"
    echo "init" > "$repo/README.md"
    git -C "$repo" add README.md
    git -C "$repo" commit -q -m "initial"
    if [ "$tracked" = "1" ]; then
        mkdir -p "$repo/claude-global/rules"
        echo "tracked" > "$repo/claude-global/CLAUDE.md"
        echo "tracked" > "$repo/claude-global/rules/language.md"
        git -C "$repo" add claude-global
        git -C "$repo" commit -q -m "add claude-global"
    fi
}

# --- Test C1: untracked claude-global/ post-agents-split → removed ---
test_C1_untracked_removed() {
    local tmp rc=0
    tmp=$(make_tmpdir)
    make_repo_with_claude_global "$tmp" 0
    # Manually create untracked claude-global/ leftover
    mkdir -p "$tmp/dotfiles/claude-global"
    echo "leftover" > "$tmp/dotfiles/claude-global/settings.json"

    remove_claude_global_dir "$tmp"
    [ -d "$tmp/dotfiles/claude-global" ] && rc=1
    assert "C1. untracked claude-global/ removed" "$rc"
    rm -rf "$tmp"
}

# --- Test C2: tracked claude-global/ (pre-split state) → kept ---
test_C2_tracked_kept() {
    local tmp rc=0
    tmp=$(make_tmpdir)
    make_repo_with_claude_global "$tmp" 1

    remove_claude_global_dir "$tmp"
    [ -d "$tmp/dotfiles/claude-global" ] || rc=1
    [ -f "$tmp/dotfiles/claude-global/CLAUDE.md" ] || rc=1
    assert "C2. tracked claude-global/ kept" "$rc"
    rm -rf "$tmp"
}

# --- Test C3: claude-global/ does not exist → no-op, no error ---
test_C3_missing_dir_no_error() {
    local tmp rc
    tmp=$(make_tmpdir)
    make_repo_with_claude_global "$tmp" 0
    # No claude-global/ directory created

    remove_claude_global_dir "$tmp"
    rc=$?
    assert "C3. missing claude-global/ does not error" "$rc"
    rm -rf "$tmp"
}

# --- Test C4: dotfiles/.git absent (non-git dir) → kept ---
test_C4_non_git_kept() {
    local tmp rc=0
    tmp=$(make_tmpdir)
    mkdir -p "$tmp/dotfiles/claude-global"
    echo "leftover" > "$tmp/dotfiles/claude-global/settings.json"
    # No .git dir

    remove_claude_global_dir "$tmp"
    [ -d "$tmp/dotfiles/claude-global" ] || rc=1
    assert "C4. non-git dotfiles → claude-global/ kept" "$rc"
    rm -rf "$tmp"
}

# --- Test C5: nested untracked content → recursive removal ---
test_C5_nested_removed() {
    local tmp rc=0
    tmp=$(make_tmpdir)
    make_repo_with_claude_global "$tmp" 0
    mkdir -p "$tmp/dotfiles/claude-global/rules" "$tmp/dotfiles/claude-global/hooks/lib"
    echo "x" > "$tmp/dotfiles/claude-global/settings.json"
    echo "x" > "$tmp/dotfiles/claude-global/rules/language.md"
    echo "x" > "$tmp/dotfiles/claude-global/hooks/lib/workflow-state.js"

    remove_claude_global_dir "$tmp"
    [ -d "$tmp/dotfiles/claude-global" ] && rc=1
    assert "C5. nested untracked content removed" "$rc"
    rm -rf "$tmp"
}

# --- Run all ---
test_dotfiles_symlink_removed
test_all_five_removed
test_agents_path_kept
test_unrelated_kept
test_regular_file_kept
test_missing_dir_no_error
test_broken_symlink_pattern_match_removed
test_idempotent
test_C1_untracked_removed
test_C2_tracked_kept
test_C3_missing_dir_no_error
test_C4_non_git_kept
test_C5_nested_removed

echo ""
echo "---- Summary ----"
echo "Passed:  $PASS"
echo "Failed:  $FAIL"
echo "Skipped: $SKIP"
if [ "$SKIP" -ne 0 ]; then
    echo "Skipped tests:"
    for n in "${SKIP_NAMES[@]}"; do
        echo "  - $n"
    done
fi
if [ "$FAIL" -ne 0 ]; then
    echo "Failed tests:"
    for n in "${FAIL_NAMES[@]}"; do
        echo "  - $n"
    done
    exit 1
fi
exit 0
