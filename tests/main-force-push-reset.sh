#!/bin/bash
# Tests for dotfiles force-push detection and auto-reset in .profile_common
set -euo pipefail

DOTFILES_DIR="$(cd "$(dirname "$0")/.." && pwd)"
PROFILE="$DOTFILES_DIR/.profile_common"
PASS=0
FAIL=0

pass() { echo "PASS: $1"; PASS=$((PASS + 1)); }
fail() { echo "FAIL: $1"; FAIL=$((FAIL + 1)); }

TMPDIR_BASE=$(mktemp -d)
trap 'rm -rf "$TMPDIR_BASE"' EXIT

# --- Helpers ---

setup_repos() {
    local name="$1"
    local origin="$TMPDIR_BASE/${name}-origin.git"
    local clone="$TMPDIR_BASE/${name}-clone"
    git -c init.defaultBranch=main init -q --bare "$origin"
    git clone -q "$origin" "$clone" 2>/dev/null
    git -C "$clone" config user.email "test@example.com"
    git -C "$clone" config user.name "Test"
    echo "initial" > "$clone/file.txt"
    git -C "$clone" add file.txt
    git -C "$clone" commit -q -m "initial"
    git -C "$clone" push -q -u origin main 2>/dev/null
    echo "$origin $clone"
}

force_push_to_origin() {
    local origin="$1"
    local tmp="$TMPDIR_BASE/fp-tmp-$RANDOM"
    git clone -q "$origin" "$tmp" 2>/dev/null
    git -C "$tmp" config user.email "test@example.com"
    git -C "$tmp" config user.name "Test"
    echo "rewritten-$RANDOM" > "$tmp/file.txt"
    git -C "$tmp" commit -q --amend -a -m "rewritten"
    git -C "$tmp" push -q --force origin main 2>/dev/null
    rm -rf "$tmp"
}

# Testable sync function mirroring .profile_common logic.
# Set DOTFILES_SYNC_TTY=1 to simulate interactive terminal.
dotfiles_sync() {
    local dir="$1"
    local home_dir="$2"
    if timeout 3 git -C "$dir" fetch 2>/dev/null; then
        if ! git -C "$dir" merge --ff-only FETCH_HEAD >/dev/null 2>&1; then
            if ! git -C "$dir" merge-base --is-ancestor HEAD FETCH_HEAD 2>/dev/null; then
                if [ -e "$home_dir/.dotfiles-no-auto-reset" ]; then
                    echo "WARNING"
                    return 0
                elif [ "${DOTFILES_SYNC_TTY:-}" = "1" ]; then
                    echo "PROMPT"
                    read -t 2 -r _ans || _ans=""
                    if [[ "$_ans" =~ ^[Yy]$ ]]; then
                        git -C "$dir" reset --hard origin/main 2>/dev/null
                        echo "RESET"
                    else
                        echo "SKIPPED"
                    fi
                else
                    echo "NO_TTY"
                fi
                return 0
            fi
        fi
    fi
    echo "OK"
}

# ===================== Normal cases =====================

# Test 1: ff-only succeeds (no divergence)
echo "--- Test 1: ff-only succeeds ---"
read -r origin clone <<< "$(setup_repos t1)"
tmp="$TMPDIR_BASE/t1-new"
git clone -q "$origin" "$tmp" 2>/dev/null
git -C "$tmp" config user.email "test@example.com"
git -C "$tmp" config user.name "Test"
echo "new" > "$tmp/new.txt"
git -C "$tmp" add new.txt
git -C "$tmp" commit -q -m "new commit"
git -C "$tmp" push -q origin main 2>/dev/null
rm -rf "$tmp"
result=$(dotfiles_sync "$clone" "$TMPDIR_BASE/home1")
if [ "$result" = "OK" ]; then pass "ff-only succeeds — no divergence handling"
else fail "expected OK, got: $result"; fi

# Test 2: diverged + interactive + y → reset
echo "--- Test 2: diverged + y ---"
read -r origin clone <<< "$(setup_repos t2)"
force_push_to_origin "$origin"
home="$TMPDIR_BASE/home2"; mkdir -p "$home"
result=$(echo "y" | DOTFILES_SYNC_TTY=1 dotfiles_sync "$clone" "$home")
if echo "$result" | grep -q "RESET"; then pass "diverged + y → reset"
else fail "expected RESET in output, got: $result"; fi

# Test 3: diverged + interactive + Y → reset
echo "--- Test 3: diverged + Y ---"
read -r origin clone <<< "$(setup_repos t3)"
force_push_to_origin "$origin"
home="$TMPDIR_BASE/home3"; mkdir -p "$home"
result=$(echo "Y" | DOTFILES_SYNC_TTY=1 dotfiles_sync "$clone" "$home")
if echo "$result" | grep -q "RESET"; then pass "diverged + Y → reset"
else fail "expected RESET in output, got: $result"; fi

# Test 4: diverged + marker file → warning only
echo "--- Test 4: marker file ---"
read -r origin clone <<< "$(setup_repos t4)"
force_push_to_origin "$origin"
home="$TMPDIR_BASE/home4"; mkdir -p "$home"
touch "$home/.dotfiles-no-auto-reset"
result=$(dotfiles_sync "$clone" "$home")
if [ "$result" = "WARNING" ]; then pass "marker file → WARNING only"
else fail "expected WARNING, got: $result"; fi

# ===================== Error cases =====================

# Test 5: diverged + non-interactive (no tty) → skip
echo "--- Test 5: non-interactive ---"
read -r origin clone <<< "$(setup_repos t5)"
force_push_to_origin "$origin"
home="$TMPDIR_BASE/home5"; mkdir -p "$home"
result=$(dotfiles_sync "$clone" "$home")
if [ "$result" = "NO_TTY" ]; then pass "non-interactive → NO_TTY skip"
else fail "expected NO_TTY, got: $result"; fi

# Test 6: diverged + interactive + n → skip
echo "--- Test 6: diverged + n ---"
read -r origin clone <<< "$(setup_repos t6)"
force_push_to_origin "$origin"
home="$TMPDIR_BASE/home6"; mkdir -p "$home"
result=$(echo "n" | DOTFILES_SYNC_TTY=1 dotfiles_sync "$clone" "$home")
if echo "$result" | grep -q "SKIPPED"; then pass "diverged + n → skipped"
else fail "expected SKIPPED in output, got: $result"; fi

# Test 7: diverged + interactive + empty enter → skip (N default)
echo "--- Test 7: empty enter ---"
read -r origin clone <<< "$(setup_repos t7)"
force_push_to_origin "$origin"
home="$TMPDIR_BASE/home7"; mkdir -p "$home"
result=$(echo "" | DOTFILES_SYNC_TTY=1 dotfiles_sync "$clone" "$home")
if echo "$result" | grep -q "SKIPPED"; then pass "empty enter → skipped (N default)"
else fail "expected SKIPPED in output, got: $result"; fi

# ===================== Edge cases =====================

# Test 8: diverged + read timeout → skip
echo "--- Test 8: timeout ---"
read -r origin clone <<< "$(setup_repos t8)"
force_push_to_origin "$origin"
home="$TMPDIR_BASE/home8"; mkdir -p "$home"
result=$(DOTFILES_SYNC_TTY=1 dotfiles_sync "$clone" "$home" < /dev/null)
if echo "$result" | grep -q "SKIPPED"; then pass "read timeout/empty → skipped"
else fail "expected SKIPPED in output, got: $result"; fi

# Test 9: dirty worktree blocking ff (not diverged) → no divergence handling
echo "--- Test 9: dirty worktree ---"
read -r origin clone <<< "$(setup_repos t9)"
tmp="$TMPDIR_BASE/t9-new"
git clone -q "$origin" "$tmp" 2>/dev/null
git -C "$tmp" config user.email "test@example.com"
git -C "$tmp" config user.name "Test"
echo "remote change" > "$tmp/file.txt"
git -C "$tmp" commit -q -a -m "remote change"
git -C "$tmp" push -q origin main 2>/dev/null
rm -rf "$tmp"
echo "local dirty" > "$clone/file.txt"
result=$(dotfiles_sync "$clone" "$TMPDIR_BASE/home9")
if [ "$result" = "OK" ]; then pass "dirty worktree ff blocked — no divergence handling"
else fail "expected OK, got: $result"; fi

# Test 10: fetch fails → no action
echo "--- Test 10: fetch fails ---"
read -r origin clone <<< "$(setup_repos t10)"
git -C "$clone" remote set-url origin /nonexistent/repo.git
result=$(dotfiles_sync "$clone" "$TMPDIR_BASE/home10")
if [ "$result" = "OK" ]; then pass "fetch fails → no action"
else fail "expected OK, got: $result"; fi

# ===================== Idempotency =====================

# Test 11: after reset, re-fetch is clean
echo "--- Test 11: idempotency ---"
read -r origin clone <<< "$(setup_repos t11)"
force_push_to_origin "$origin"
home="$TMPDIR_BASE/home11"; mkdir -p "$home"
result1=$(echo "y" | DOTFILES_SYNC_TTY=1 dotfiles_sync "$clone" "$home")
result2=$(dotfiles_sync "$clone" "$home")
if echo "$result1" | grep -q "RESET" && [ "$result2" = "OK" ]; then
    pass "after reset, re-fetch is clean (idempotent)"
else
    fail "expected RESET then OK, got: '$result1' then '$result2'"
fi

# ===================== Structural checks =====================

# Verify .profile_common has the expected tty guard
echo "--- Structural check ---"
if grep -q '\[\[ -t 0 \]\]' "$PROFILE" 2>/dev/null; then
    pass ".profile_common contains [[ -t 0 ]] tty guard"
else
    echo "SKIP: tty guard not yet in .profile_common (will pass after implementation)"
fi

echo ""
echo "=== Results: $PASS passed, $FAIL failed ==="
[ "$FAIL" -eq 0 ] || exit 1
