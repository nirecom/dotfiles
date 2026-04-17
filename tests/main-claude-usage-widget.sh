#!/bin/bash
# Tests for install/linux/claude-usage-widget.sh
# Validates script existence, syntax, and installation logic
set -euo pipefail

ERRORS=0
fail() { echo "FAIL: $1"; ERRORS=$((ERRORS + 1)); }
pass() { echo "PASS: $1"; }

SCRIPT="$(cd "$(dirname "$0")/.." && pwd)/install/linux/claude-usage-widget.sh"

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

# Test: script downloads from GitHub releases
grep -q "github.com/SlavomirDurej/claude-usage-widget/releases" "$SCRIPT" && pass "downloads from GitHub releases" || fail "missing GitHub releases URL"

# Test: script has idempotent up-to-date skip (checks version against latest)
grep -q "up to date" "$SCRIPT" && pass "has idempotent up-to-date skip" || fail "missing up-to-date skip"

# Test: macOS configures login item for autostart
grep -q "osascript.*login item" "$SCRIPT" && pass "macOS: configures login item" || fail "missing macOS login item setup"

# Test: ubuntu creates autostart desktop entry
grep -q "\.config/autostart" "$SCRIPT" && pass "ubuntu: creates autostart desktop entry" || fail "missing autostart desktop entry"

echo ""
echo "=== Error cases ==="

# Test: WSL environment is skipped
grep -q 'ISWSL.*1' "$SCRIPT" && pass "skips WSL environment" || fail "missing WSL skip"

# Test: unsupported OS is handled with skip message
grep -q "not available" "$SCRIPT" && pass "handles unsupported OS with skip message" || fail "missing unsupported OS skip message"

# Test: macOS cleans up temp dmg on exit (trap)
grep -q 'trap.*rm.*TMPFILE' "$SCRIPT" && pass "macOS: trap cleans up temp dmg" || fail "missing trap for temp dmg cleanup"

# Test: macOS errors if no .app found in DMG
grep -q 'no .app found in DMG' "$SCRIPT" && pass "macOS: errors if no .app in DMG" || fail "missing .app not found error handling"

echo ""
echo "=== Edge cases ==="

# Test: macOS ARM vs x86 architecture branching
grep -q 'ISM1.*1' "$SCRIPT" && pass "macOS: ARM (M1) vs x86 branching" || fail "missing ARM vs x86 branching"

# Test: Linux ARM vs x86 architecture branching
grep -q 'uname -m' "$SCRIPT" && pass "Linux: ARM vs x86 via uname -m" || fail "missing uname -m architecture check"

# Test: macOS login item idempotent (checks before adding)
grep -q "Already in login items" "$SCRIPT" && pass "macOS: login item idempotent" || fail "missing login item idempotent check"

# Test: ubuntu autostart entry idempotent (checks before creating)
grep -q "Autostart entry already exists" "$SCRIPT" && pass "ubuntu: autostart entry idempotent" || fail "missing autostart idempotent check"

# Test: ubuntu creates .local/bin if missing
grep -q 'mkdir -p.*INSTALL_DIR' "$SCRIPT" && pass "ubuntu: creates install dir if missing" || fail "missing mkdir for install dir"

# Test: macOS dynamically finds .app in DMG (not hardcoded name)
grep -q 'find.*maxdepth.*\.app' "$SCRIPT" && pass "macOS: dynamically finds .app in DMG" || fail "missing dynamic .app detection"

# Test: macOS uses -print -quit to take first .app match
grep -q '\-print -quit' "$SCRIPT" && pass "macOS: -print -quit for deterministic .app selection" || fail "missing -print -quit for .app selection"

# Test: ubuntu sets executable permission on AppImage
grep -q 'chmod +x.*APPIMAGE_PATH' "$SCRIPT" && pass "ubuntu: chmod +x on AppImage" || fail "missing chmod +x on AppImage"

echo ""
echo "=== Update cases ==="

# Test: macOS reads CFBundleShortVersionString from installed .app
grep -q 'CFBundleShortVersionString' "$SCRIPT" && pass "macOS: reads CFBundleShortVersionString" || fail "missing CFBundleShortVersionString read"

# Test: macOS removes old .app before reinstall on update
grep -qE 'rm -rf.*APP_PATH|rm -rf.*\.app' "$SCRIPT" && pass "macOS: removes old .app before reinstall" || fail "missing rm -rf of old .app on update"

# Test: Linux reads .version sidecar file
grep -q '\.version' "$SCRIPT" && pass "linux: reads .version sidecar file" || fail "missing .version sidecar read"

# Test: Linux writes version to .version sidecar after install
grep -qE 'echo.*>.*\.version|> .*\.version' "$SCRIPT" && pass "linux: writes .version sidecar after install" || fail "missing .version sidecar write"

# Test: up-to-date skip message
grep -q 'up to date' "$SCRIPT" && pass "shows 'up to date' skip message" || fail "missing 'up to date' skip message"

# Test: strips v prefix from tag
grep -qE "sed.*s/\^v|#v" "$SCRIPT" && pass "strips 'v' prefix from tag_name" || fail "missing 'v' prefix strip"

# Test: missing .version file falls back to reinstall
grep -qE 'if \[ ! -f.*\.version|\[ -z.*VERSION' "$SCRIPT" && pass "handles missing .version file (fallback to reinstall)" || fail "missing .version fallback handling"

echo ""
if [ "$ERRORS" -eq 0 ]; then
    echo "All tests passed!"
else
    echo "$ERRORS test(s) failed"
    exit 1
fi
