#!/bin/bash
# tests/main-vscode.sh - Validate VS Code installer scripts and extension list
set -euo pipefail

WIN_SCRIPT="install/win/vscode.ps1"
LINUX_SCRIPT="install/linux/vscode.sh"
EXT_LIST="config/vscode-extensions.txt"
ERRORS=0

fail() { echo "FAIL: $1"; ERRORS=$((ERRORS + 1)); }
pass() { echo "PASS: $1"; }

# === Extension list tests ===

echo "=== Normal: extension list file exists ==="
if [ -f "$EXT_LIST" ]; then pass "extension list exists"
else fail "extension list not found at $EXT_LIST"; fi

echo ""
echo "=== Normal: extension list is not empty ==="
if [ -s "$EXT_LIST" ]; then pass "extension list has content"
else fail "extension list is empty"; fi

echo ""
echo "=== Normal: all extensions have publisher.name format ==="
INVALID=$(grep -v '^\s*$' "$EXT_LIST" | grep -v '^\s*#' | grep -cv '\.' || true)
if [ "$INVALID" -eq 0 ]; then pass "all extensions have publisher.name format"
else fail "$INVALID extension(s) missing publisher prefix"; fi

echo ""
echo "=== Edge: no duplicate extensions ==="
DUPES=$(sort "$EXT_LIST" | uniq -d | wc -l)
if [ "$DUPES" -eq 0 ]; then pass "no duplicate extensions"
else fail "found $DUPES duplicate extension(s)"; fi

echo ""
echo "=== Edge: no trailing whitespace in extension list ==="
TRAILING=$(grep -c ' $' "$EXT_LIST" || true)
if [ "$TRAILING" -eq 0 ]; then pass "no trailing whitespace"
else fail "$TRAILING line(s) with trailing whitespace"; fi

# === Windows script tests ===

echo ""
echo "=== Normal: Windows script exists ==="
if [ -f "$WIN_SCRIPT" ]; then pass "Windows script exists"
else fail "Windows script not found at $WIN_SCRIPT"; fi

echo ""
echo "=== Normal: Windows script installs via winget ==="
if grep -q 'Microsoft.VisualStudioCode' "$WIN_SCRIPT"; then pass "uses correct winget package ID"
else fail "missing winget package ID"; fi

echo ""
echo "=== Normal: Windows script checks if already installed ==="
if grep -q 'already installed' "$WIN_SCRIPT"; then pass "has already-installed check"
else fail "missing already-installed message"; fi

echo ""
echo "=== Normal: Windows script reads shared extension list ==="
if grep -q 'vscode-extensions.txt' "$WIN_SCRIPT"; then pass "reads shared extension list"
else fail "does not reference shared extension list"; fi

echo ""
echo "=== Normal: Windows script installs extensions via code CLI ==="
if grep -q 'code --install-extension' "$WIN_SCRIPT"; then pass "installs extensions via code CLI"
else fail "missing code --install-extension"; fi

echo ""
echo "=== Normal: Windows script skips already-installed extensions ==="
if grep -q 'code --list-extensions' "$WIN_SCRIPT"; then pass "checks installed extensions"
else fail "missing installed extension check"; fi

echo ""
echo "=== Error: Windows script handles missing winget ==="
if grep -q 'winget not found' "$WIN_SCRIPT"; then pass "handles missing winget"
else fail "no fallback for missing winget"; fi

echo ""
echo "=== Error: Windows script handles missing extension list ==="
if grep -q 'Extension list not found' "$WIN_SCRIPT"; then pass "handles missing extension list"
else fail "no error handling for missing extension list"; fi

echo ""
echo "=== Edge: Windows script refreshes PATH after install ==="
if grep -q 'GetEnvironmentVariable' "$WIN_SCRIPT"; then pass "refreshes PATH"
else fail "missing PATH refresh after install"; fi

echo ""
echo "=== Edge: Windows script skips comment lines ==="
if grep -q '#' "$WIN_SCRIPT" && grep -q 'notmatch.*#' "$WIN_SCRIPT"; then pass "skips comment lines"
else fail "does not skip comment lines in extension list"; fi

# === Linux script tests ===

echo ""
echo "=== Normal: Linux script exists ==="
if [ -f "$LINUX_SCRIPT" ]; then pass "Linux script exists"
else fail "Linux script not found at $LINUX_SCRIPT"; fi

echo ""
echo "=== Normal: Linux script sources detectos.sh ==="
if grep -q 'source.*detectos.sh' "$LINUX_SCRIPT"; then pass "sources detectos.sh"
else fail "does not source detectos.sh"; fi

echo ""
echo "=== Normal: Linux script handles Ubuntu ==="
if grep -q '"ubuntu"' "$LINUX_SCRIPT"; then pass "handles Ubuntu"
else fail "missing Ubuntu case"; fi

echo ""
echo "=== Normal: Linux script handles macOS ==="
if grep -q '"macos"' "$LINUX_SCRIPT"; then pass "handles macOS"
else fail "missing macOS case"; fi

echo ""
echo "=== Normal: Linux script installs via brew on macOS ==="
if grep -q 'brew install.*visual-studio-code' "$LINUX_SCRIPT"; then pass "uses brew cask on macOS"
else fail "missing brew install for macOS"; fi

echo ""
echo "=== Normal: Linux script checks if already installed ==="
if grep -q 'already installed' "$LINUX_SCRIPT"; then pass "has already-installed check"
else fail "missing already-installed message"; fi

echo ""
echo "=== Normal: Linux script reads shared extension list ==="
if grep -q 'vscode-extensions.txt' "$LINUX_SCRIPT"; then pass "reads shared extension list"
else fail "does not reference shared extension list"; fi

echo ""
echo "=== Normal: Linux script installs extensions via code CLI ==="
if grep -q 'code --install-extension' "$LINUX_SCRIPT"; then pass "installs extensions via code CLI"
else fail "missing code --install-extension"; fi

echo ""
echo "=== Edge: Linux script skips WSL ==="
if grep -q 'WSL detected' "$LINUX_SCRIPT"; then pass "skips WSL"
else fail "missing WSL skip logic"; fi

echo ""
echo "=== Edge: Linux script skips comment lines ==="
if grep -q '^\s*#' "$LINUX_SCRIPT" && grep -q '\#\*' "$LINUX_SCRIPT"; then pass "skips comment lines"
else fail "does not skip comment lines in extension list"; fi

echo ""
echo "=== Error: Linux script handles missing brew ==="
if grep -q 'Homebrew not found' "$LINUX_SCRIPT"; then pass "handles missing Homebrew"
else fail "no fallback for missing Homebrew"; fi

echo ""
echo "=== Error: Linux script handles unsupported OS ==="
if grep -q 'Unsupported OS' "$LINUX_SCRIPT"; then pass "handles unsupported OS"
else fail "no handling for unsupported OS"; fi

echo ""
echo "=== Error: Linux script handles missing extension list ==="
if grep -q 'Extension list not found' "$LINUX_SCRIPT"; then pass "handles missing extension list"
else fail "no error handling for missing extension list"; fi

# === Cross-platform consistency tests ===

echo ""
echo "=== Normal: both scripts use --force flag for extensions ==="
if grep -q '\-\-force' "$WIN_SCRIPT" && grep -q '\-\-force' "$LINUX_SCRIPT"; then
    pass "both scripts use --force flag"
else fail "missing --force flag in one or both scripts"; fi

echo ""
echo "=== Idempotency: both scripts check existing extensions before install ==="
if grep -q 'list-extensions' "$WIN_SCRIPT" && grep -q 'list-extensions' "$LINUX_SCRIPT"; then
    pass "both scripts check existing extensions"
else fail "missing existing extension check in one or both scripts"; fi

echo ""
echo "=== Error: Windows script handles code not found after install ==="
if grep -q "code.*command not found" "$WIN_SCRIPT"; then pass "handles code not found after install"
else fail "missing code-not-found-after-install handling"; fi

echo ""
echo "=== Error: Linux script handles code not found after install ==="
if grep -q "code.*command not found" "$LINUX_SCRIPT"; then pass "handles code not found after install"
else fail "missing code-not-found-after-install handling"; fi

echo ""
echo "=== Edge: Windows script uses case-insensitive extension matching ==="
if grep -q '(?i)' "$WIN_SCRIPT"; then pass "case-insensitive extension matching"
else fail "missing case-insensitive extension matching"; fi

echo ""
echo "=== Edge: Linux script uses case-insensitive grep for extensions ==="
if grep -q 'grep -qi' "$LINUX_SCRIPT"; then pass "case-insensitive grep for extensions"
else fail "missing case-insensitive grep for extensions"; fi

echo ""
echo "=== Normal: Windows script suppresses extension install errors ==="
if grep -q 'install-extension.*2>.*null' "$WIN_SCRIPT"; then pass "suppresses extension install errors"
else fail "missing error suppression on install-extension"; fi

echo ""
echo "=== Normal: Linux script suppresses extension install errors ==="
if grep -q 'install-extension.*2>/dev/null' "$LINUX_SCRIPT"; then pass "suppresses extension install errors"
else fail "missing error suppression on install-extension"; fi

echo ""
echo "=== Normal: Linux script has execute permission in git ==="
MODE=$(git ls-files -s "$LINUX_SCRIPT" | awk '{print $1}')
if [ "$MODE" = "100755" ]; then pass "vscode.sh has execute permission"
else fail "vscode.sh is $MODE (expected 100755)"; fi

echo ""
echo "=== Normal: pre-commit hook checks .sh execute permission ==="
HOOK="claude-global/hooks/pre-commit"
if grep -q 'execute permission' "$HOOK" && grep -q '\.sh' "$HOOK"; then
    pass "pre-commit checks .sh execute permission"
else fail "pre-commit missing .sh permission check"; fi

echo ""
echo "=== Edge: extension list has at least one extension ==="
EXT_COUNT=$(grep -v '^\s*$' "$EXT_LIST" | grep -cv '^\s*#' || true)
if [ "$EXT_COUNT" -gt 0 ]; then pass "extension list has $EXT_COUNT extension(s)"
else fail "extension list has no extensions"; fi

echo ""
echo "=== Results ==="
if [ "$ERRORS" -eq 0 ]; then echo "All tests passed!"
else echo "$ERRORS test(s) failed"; exit 1; fi
