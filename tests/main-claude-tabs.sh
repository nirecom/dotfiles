#!/bin/bash
# tests/main-claude-tabs.sh - Validate claude-tabs.ps1 installer script
set -euo pipefail

SCRIPT="install/win/claude-tabs.ps1"
ERRORS=0

fail() { echo "FAIL: $1"; ERRORS=$((ERRORS + 1)); }
pass() { echo "PASS: $1"; }

# --- Structure validation tests ---

echo "=== Normal: script file exists ==="
if [ -f "$SCRIPT" ]; then pass "script exists"
else fail "script not found at $SCRIPT"; fi

echo ""
echo "=== Normal: idempotency check — tests for existing install ==="
if grep -q 'already installed' "$SCRIPT"; then pass "has already-installed check"
else fail "missing already-installed message"; fi

echo ""
echo "=== Normal: checks both per-user and per-machine paths ==="
if grep -q 'LOCALAPPDATA' "$SCRIPT" && grep -q 'ProgramFiles' "$SCRIPT"; then
    pass "checks both install locations"
else fail "missing install location checks"; fi

echo ""
echo "=== Normal: fetches from GitHub API ==="
if grep -q 'api.github.com/repos/gunba/claude-tabs' "$SCRIPT"; then
    pass "uses correct GitHub API URL"
else fail "missing or wrong GitHub API URL"; fi

echo ""
echo "=== Normal: walks multiple releases for assets ==="
if grep -q 'per_page' "$SCRIPT"; then
    pass "walks multiple releases"
else fail "missing multi-release fallback"; fi

echo ""
echo "=== Normal: matches x64-setup.exe asset ==="
if grep -q 'x64-setup' "$SCRIPT"; then pass "filters for x64-setup asset"
else fail "missing x64-setup asset filter"; fi

echo ""
echo "=== Normal: silent install flag ==="
if grep -q '"/S"' "$SCRIPT" || grep -q "'/S'" "$SCRIPT"; then
    pass "uses silent install flag"
else fail "missing /S silent install flag"; fi

echo ""
echo "=== Error: handles missing asset gracefully ==="
if grep -q 'Could not find' "$SCRIPT" || grep -q 'Warning' "$SCRIPT"; then
    pass "handles missing asset with warning"
else fail "no error handling for missing asset"; fi

echo ""
echo "=== Edge: cleans up temp file ==="
if grep -q 'Remove-Item' "$SCRIPT"; then pass "cleans up temp file"
else fail "missing temp file cleanup"; fi

echo ""
echo "=== Edge: polls for install completion ==="
if grep -q 'Waiting' "$SCRIPT"; then
    pass "polls for install completion"
else fail "missing install completion polling"; fi

echo ""
echo "=== Normal: timeout is 90 seconds ==="
if grep -q '$timeout = 90' "$SCRIPT"; then pass "timeout is 90s"
else fail "missing or wrong timeout value"; fi

echo ""
echo "=== Normal: polling interval is 5 seconds ==="
if grep -q 'Sleep -Seconds 5' "$SCRIPT" && grep -q '$i += 5' "$SCRIPT"; then
    pass "polling interval is 5s"
else fail "missing or wrong polling interval"; fi

echo ""
echo "=== Normal: selects first matching asset ==="
if grep -q 'Select-Object -First 1' "$SCRIPT"; then pass "selects first asset"
else fail "missing Select-Object -First 1"; fi

echo ""
echo "=== Normal: uses UseBasicParsing for download ==="
if grep -q 'UseBasicParsing' "$SCRIPT"; then pass "uses UseBasicParsing"
else fail "missing UseBasicParsing flag"; fi

echo ""
echo "=== Normal: uses Start-Process -Wait ==="
if grep -q 'Start-Process.*-Wait' "$SCRIPT"; then pass "uses Start-Process -Wait"
else fail "missing Start-Process -Wait"; fi

echo ""
echo "=== Edge: polling uses for loop ==="
if grep -q 'for ($i = 0' "$SCRIPT"; then pass "polling uses for loop"
else fail "missing for loop in polling"; fi

echo ""
echo "=== Edge: temp cleanup uses SilentlyContinue ==="
if grep -q 'Remove-Item.*SilentlyContinue' "$SCRIPT"; then
    pass "temp cleanup ignores errors"
else fail "missing SilentlyContinue on Remove-Item"; fi

echo ""
echo "=== Error: warns on install timeout ==="
if grep -q 'install may have failed' "$SCRIPT"; then pass "warns on timeout"
else fail "missing timeout warning message"; fi

echo ""
echo "=== Results ==="
if [ "$ERRORS" -eq 0 ]; then echo "All tests passed!"
else echo "$ERRORS test(s) failed"; exit 1; fi
