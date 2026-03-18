#!/bin/bash
# Tests for install/linux/rize.sh
# Validates script existence, syntax, and macOS installation logic
set -euo pipefail

ERRORS=0
fail() { echo "FAIL: $1"; ERRORS=$((ERRORS + 1)); }
pass() { echo "PASS: $1"; }

SCRIPT="$(cd "$(dirname "$0")/.." && pwd)/install/linux/rize.sh"

echo "=== Normal cases ==="

# Test: script file exists
[ -f "$SCRIPT" ] && pass "script file exists" || fail "script file not found: $SCRIPT"

# Test: script has valid bash syntax
if bash -n "$SCRIPT" 2>/dev/null; then
    pass "script has valid bash syntax"
else
    fail "script has syntax errors"
fi

# Test: script sources detectos.sh
grep -q "source ~/dotfiles/bin/detectos.sh" "$SCRIPT" && pass "sources detectos.sh" || fail "does not source detectos.sh"

# Test: script uses case statement for OS branching
grep -q 'case "$OSDIST"' "$SCRIPT" && pass "uses OSDIST case branching" || fail "missing OSDIST case branching"

# Test: script handles macOS
grep -q "macos" "$SCRIPT" && pass "handles macOS" || fail "missing macOS handling"

# Test: script checks for existing installation
grep -q "/Applications/Rize.app" "$SCRIPT" && pass "checks for existing Rize.app" || fail "missing Rize.app existence check"

echo ""
echo "=== Edge cases ==="

# Test: script handles unsupported OS
grep -q "ubuntu\|Unsupported\|not available\|Skip" "$SCRIPT" && pass "handles unsupported OS" || fail "missing unsupported OS handling"

echo ""
if [ "$ERRORS" -eq 0 ]; then
    echo "All tests passed!"
else
    echo "$ERRORS test(s) failed"
    exit 1
fi
