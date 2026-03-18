#!/usr/bin/env bash
# Test: master → main migration temporary code has been removed
set -euo pipefail

DOTFILES_DIR="$(cd "$(dirname "$0")/.." && pwd)"
ERRORS=0

pass() { echo "  PASS: $1"; }
fail() { echo "  FAIL: $1"; ERRORS=$((ERRORS + 1)); }

echo "=== master → main migration cleanup tests ==="

# --- Normal cases ---
echo ""
echo "--- Normal: migration block removed ---"

if grep -q "BEGIN temporary: main branch" "$DOTFILES_DIR/.profile_common"; then
    fail ".profile_common still contains 'BEGIN temporary: main branch' block"
else
    pass ".profile_common: master→main migration block removed"
fi

if grep -q "BEGIN temporary: main branch" "$DOTFILES_DIR/install/win/profile.ps1"; then
    fail "profile.ps1 still contains 'BEGIN temporary: main branch' block"
else
    pass "profile.ps1: master→main migration block removed"
fi

echo ""
echo "--- Normal: other migration blocks preserved ---"

if grep -q "BEGIN temporary: claude-code" "$DOTFILES_DIR/.profile_common"; then
    pass ".profile_common: claude-code→claude-global block preserved"
else
    fail ".profile_common: claude-code→claude-global block is missing"
fi

if grep -q "BEGIN temporary: commands" "$DOTFILES_DIR/.profile_common"; then
    pass ".profile_common: commands→skills block preserved"
else
    fail ".profile_common: commands→skills block is missing"
fi

if grep -q "BEGIN temporary: claude-code" "$DOTFILES_DIR/install/win/profile.ps1"; then
    pass "profile.ps1: claude-code→claude-global block preserved"
else
    fail "profile.ps1: claude-code→claude-global block is missing"
fi

if grep -q "BEGIN temporary: commands" "$DOTFILES_DIR/install/win/profile.ps1"; then
    pass "profile.ps1: commands→skills block preserved"
else
    fail "profile.ps1: commands→skills block is missing"
fi

echo ""
echo "--- Normal: syntax check ---"

if bash -n "$DOTFILES_DIR/.profile_common" 2>/dev/null; then
    pass ".profile_common: bash -n syntax OK"
else
    fail ".profile_common: bash syntax error"
fi

# --- Error cases ---
echo ""
echo "--- Error: no orphaned END marker ---"

if grep -q "END temporary: main branch" "$DOTFILES_DIR/.profile_common"; then
    fail ".profile_common: orphaned 'END temporary: main branch' marker found"
else
    pass ".profile_common: no orphaned END marker"
fi

if grep -q "END temporary: main branch" "$DOTFILES_DIR/install/win/profile.ps1"; then
    fail "profile.ps1: orphaned 'END temporary: main branch' marker found"
else
    pass "profile.ps1: no orphaned END marker"
fi

echo ""
echo "--- Error: no leftover variables from deleted block ---"

# _cur_branch and _upstream were local to the deleted block in .profile_common
if grep -q '_cur_branch\|_upstream' "$DOTFILES_DIR/.profile_common"; then
    fail ".profile_common: leftover variable (_cur_branch or _upstream) found"
else
    pass ".profile_common: no leftover variables from deleted block"
fi

# $curBranch and $upstream were local to the deleted block in profile.ps1
if grep -q 'curBranch\|set-upstream-to=origin/main' "$DOTFILES_DIR/install/win/profile.ps1"; then
    fail "profile.ps1: leftover variable (\$curBranch or upstream-to) found"
else
    pass "profile.ps1: no leftover variables from deleted block"
fi

# --- Edge cases ---
echo ""
echo "--- Edge: no triple blank lines (over-deletion) ---"

# Check for 3+ consecutive blank lines (sign of over-deletion)
if awk '/^$/{c++; if(c>=3){found=1}} /^.+$/{c=0} END{exit !found}' "$DOTFILES_DIR/.profile_common" 2>/dev/null; then
    fail ".profile_common: 3+ consecutive blank lines found (possible over-deletion)"
else
    pass ".profile_common: no excessive blank lines"
fi

if awk '/^$/{c++; if(c>=3){found=1}} /^.+$/{c=0} END{exit !found}' "$DOTFILES_DIR/install/win/profile.ps1" 2>/dev/null; then
    fail "profile.ps1: 3+ consecutive blank lines found (possible over-deletion)"
else
    pass "profile.ps1: no excessive blank lines"
fi

echo ""
echo "--- Edge: adjacent code intact ---"

# .profile_common: the line before the deleted block should still be "fi" (end of git pull block)
# and the line after should be the claude-code migration block
if grep -q "BEGIN temporary: claude-code" "$DOTFILES_DIR/.profile_common"; then
    pass ".profile_common: code after deleted block is intact"
else
    fail ".profile_common: code after deleted block may be damaged"
fi

# profile.ps1: the line before should be "}" (end of broken symlink repair)
# and the line after should be the claude-code migration block
if grep -q "BEGIN temporary: claude-code" "$DOTFILES_DIR/install/win/profile.ps1"; then
    pass "profile.ps1: code after deleted block is intact"
else
    fail "profile.ps1: code after deleted block may be damaged"
fi

# --- Summary ---
echo ""
if [ "$ERRORS" -eq 0 ]; then
    echo "All tests passed!"
else
    echo "$ERRORS test(s) FAILED"
    exit 1
fi
