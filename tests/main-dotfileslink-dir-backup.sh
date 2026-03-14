#!/bin/bash
# Tests for directory backup & symlink replacement logic in install/linux/dotfileslink.sh
# When a regular directory exists where a symlink should be, it should be backed up to .bak and replaced
set -euo pipefail

ERRORS=0
fail() { echo "FAIL: $1"; ERRORS=$((ERRORS + 1)); }
pass() { echo "PASS: $1"; }
skip() { echo "SKIP: $1"; }

# Git Bash (MSYS2) doesn't create real symlinks — skip -L checks on Windows
IS_WINDOWS=false
case "$OSTYPE" in msys*|cygwin*) IS_WINDOWS=true ;; esac

TMPDIR=$(mktemp -d)
trap 'rm -rf "$TMPDIR"' EXIT

# --- Helper: simulate the backup+symlink logic from dotfileslink.sh ---
do_symlink() {
    local source="$1" dest="$2"
    if [ ! -e "$source" ]; then
        echo "skipped-no-source"
        return
    fi
    if [ -d "$dest" ] && [ ! -L "$dest" ]; then
        rm -rf "$dest.bak"
        mv "$dest" "$dest.bak"
    fi
    ln -snf "$source" "$dest"
    echo "created"
}

echo "=== Normal cases ==="

# Test: backs up regular directory and creates symlink
setup="$TMPDIR/test-backup"
mkdir -p "$setup/source" "$setup/dest"
echo "test content" > "$setup/dest/file.txt"
result=$(do_symlink "$setup/source" "$setup/dest")
[ "$result" = "created" ] && pass "backup: returns created" || fail "backup: expected created, got $result"
if $IS_WINDOWS; then skip "backup: dest is symlink (no real symlinks on Git Bash)"
else [ -L "$setup/dest" ] && pass "backup: dest is symlink" || fail "backup: dest is not symlink"; fi
[ -d "$setup/dest.bak" ] && pass "backup: .bak exists" || fail "backup: .bak missing"
[ "$(cat "$setup/dest.bak/file.txt")" = "test content" ] && pass "backup: .bak has original content" || fail "backup: .bak content mismatch"

# Test: overwrites existing .bak
setup="$TMPDIR/test-overwrite"
mkdir -p "$setup/source" "$setup/dest" "$setup/dest.bak"
echo "old backup" > "$setup/dest.bak/old.txt"
echo "new content" > "$setup/dest/new.txt"
result=$(do_symlink "$setup/source" "$setup/dest")
[ "$result" = "created" ] && pass "overwrite: returns created" || fail "overwrite: expected created, got $result"
[ ! -f "$setup/dest.bak/old.txt" ] && pass "overwrite: old .bak content removed" || fail "overwrite: old .bak content still exists"
[ "$(cat "$setup/dest.bak/new.txt")" = "new content" ] && pass "overwrite: new .bak content correct" || fail "overwrite: new .bak content mismatch"

# Test: skips when already symlinked
setup="$TMPDIR/test-already"
mkdir -p "$setup/source"
ln -snf "$setup/source" "$setup/dest"
result=$(do_symlink "$setup/source" "$setup/dest")
[ "$result" = "created" ] && pass "already-linked: ln -snf succeeds" || fail "already-linked: unexpected result $result"
if $IS_WINDOWS; then skip "already-linked: dest is symlink (no real symlinks on Git Bash)"
else [ -L "$setup/dest" ] && pass "already-linked: dest is still symlink" || fail "already-linked: dest is not symlink"; fi

echo ""
echo "=== Abnormal cases ==="

# Test: skips when source does not exist
setup="$TMPDIR/test-no-source"
mkdir -p "$setup"
result=$(do_symlink "$setup/nonexistent" "$setup/dest")
[ "$result" = "skipped-no-source" ] && pass "no-source: returns skipped" || fail "no-source: expected skipped-no-source, got $result"

echo ""
if [ "$ERRORS" -eq 0 ]; then
    echo "All tests passed!"
else
    echo "$ERRORS test(s) failed"
    exit 1
fi
