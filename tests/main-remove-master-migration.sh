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
echo "--- Normal: claude-code block removed, other blocks preserved ---"

if grep -q "BEGIN temporary: claude-code" "$DOTFILES_DIR/.profile_common"; then
    fail ".profile_common: claude-code→claude-global block still exists (should be removed)"
else
    pass ".profile_common: claude-code→claude-global block removed"
fi

if grep -q "BEGIN temporary: commands" "$DOTFILES_DIR/.profile_common"; then
    pass ".profile_common: commands→skills block preserved"
else
    fail ".profile_common: commands→skills block is missing"
fi

if grep -q "BEGIN temporary: claude-code" "$DOTFILES_DIR/install/win/profile.ps1"; then
    fail "profile.ps1: claude-code→claude-global block still exists (should be removed)"
else
    pass "profile.ps1: claude-code→claude-global block removed"
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

PS1_WIN_PATH=$(cygpath -w "$DOTFILES_DIR/install/win/profile.ps1" 2>/dev/null || echo "$DOTFILES_DIR/install/win/profile.ps1")
if powershell.exe -NoProfile -Command "try { [System.Management.Automation.PSParser]::Tokenize((Get-Content '$PS1_WIN_PATH' -Raw), [ref]\$null) | Out-Null; exit 0 } catch { exit 1 }" 2>/dev/null; then
    pass "profile.ps1: PowerShell syntax OK"
else
    fail "profile.ps1: PowerShell syntax error"
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

echo ""
echo "--- Error: no unmatched BEGIN/END markers ---"

# Count BEGIN and END temporary markers — they must be paired
for file in ".profile_common" "install/win/profile.ps1"; do
    begins=$(grep -c "BEGIN temporary:" "$DOTFILES_DIR/$file" || true)
    ends=$(grep -c "END temporary:" "$DOTFILES_DIR/$file" || true)
    if [ "$begins" -ne "$ends" ]; then
        fail "$file: unmatched markers (BEGIN=$begins, END=$ends)"
    else
        pass "$file: BEGIN/END markers are paired ($begins each)"
    fi
done

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

# .profile_common: after both master→main and claude-code blocks removed,
# the commands→skills migration block should still be intact
if grep -q "BEGIN temporary: commands" "$DOTFILES_DIR/.profile_common"; then
    pass ".profile_common: code after deleted blocks is intact"
else
    fail ".profile_common: code after deleted blocks may be damaged"
fi

# profile.ps1: after both master→main and claude-code blocks removed,
# the commands→skills migration block should still be intact
if grep -q "BEGIN temporary: commands" "$DOTFILES_DIR/install/win/profile.ps1"; then
    pass "profile.ps1: code after deleted blocks is intact"
else
    fail "profile.ps1: code after deleted blocks may be damaged"
fi

echo ""
echo "--- Edge: files not empty (no catastrophic deletion) ---"

for file in ".profile_common" "install/win/profile.ps1"; do
    lines=$(wc -l < "$DOTFILES_DIR/$file")
    if [ "$lines" -lt 10 ]; then
        fail "$file: suspiciously small ($lines lines)"
    else
        pass "$file: has substantial content ($lines lines)"
    fi
done

echo ""
echo "--- Edge: no duplicate migration blocks ---"

for file in ".profile_common" "install/win/profile.ps1"; do
    count=$(grep -c "BEGIN temporary: commands" "$DOTFILES_DIR/$file" || true)
    if [ "$count" -gt 1 ]; then
        fail "$file: duplicate commands→skills block (count=$count)"
    else
        pass "$file: commands→skills block appears exactly once"
    fi
done

# --- Summary ---
echo ""
if [ "$ERRORS" -eq 0 ]; then
    echo "All tests passed!"
else
    echo "$ERRORS test(s) FAILED"
    exit 1
fi
