#!/bin/bash
# Smoke tests for agents repo split step 2 changes.
# Verifies: AGENTS_CONFIG_DIR/AGENTS_DIR exports in .profile_common and profile.ps1,
#           settings.json hook path migration (old path gone, new path present).
set -euo pipefail

DOTFILES_DIR="$(cd "$(dirname "$0")/.." && pwd)"
SETTINGS="$DOTFILES_DIR/claude-global/settings.json"
PROFILE_COMMON="$DOTFILES_DIR/.profile_common"
PROFILE_PS1="$DOTFILES_DIR/install/win/profile.ps1"
ERRORS=0

fail() { echo "FAIL: $1"; ERRORS=$((ERRORS + 1)); }
pass() { echo "PASS: $1"; }

for f in "$SETTINGS" "$PROFILE_COMMON" "$PROFILE_PS1"; do
    if [ ! -f "$f" ]; then
        echo "FATAL: required file not found: $f"
        exit 2
    fi
done

# ---------------------------------------------------------------------------
# N1: settings.json has 0 occurrences of old path $DOTFILES_DIR/claude-global/hooks/
# ---------------------------------------------------------------------------
echo ""
echo "=== N1: settings.json — old path absent ==="

OLD_COUNT=$(grep -o '\$DOTFILES_DIR/claude-global/hooks/' "$SETTINGS" 2>/dev/null | wc -l || true)
if [ "$OLD_COUNT" -eq 0 ]; then
    pass "N1. settings.json contains 0 occurrences of \$DOTFILES_DIR/claude-global/hooks/"
else
    fail "N1. settings.json still contains $OLD_COUNT occurrence(s) of \$DOTFILES_DIR/claude-global/hooks/ (expected 0)"
fi

# ---------------------------------------------------------------------------
# N2: settings.json has exactly 11 occurrences of $AGENTS_CONFIG_DIR/hooks/
# ---------------------------------------------------------------------------
echo ""
echo "=== N2: settings.json — new path count ==="

NEW_COUNT=$(grep -o '\$AGENTS_CONFIG_DIR/hooks/' "$SETTINGS" 2>/dev/null | wc -l || true)
if [ "$NEW_COUNT" -eq 11 ]; then
    pass "N2. settings.json contains exactly 11 occurrences of \$AGENTS_CONFIG_DIR/hooks/"
else
    fail "N2. settings.json contains $NEW_COUNT occurrence(s) of \$AGENTS_CONFIG_DIR/hooks/ (expected 11)"
fi

# ---------------------------------------------------------------------------
# N3: .profile_common exports AGENTS_CONFIG_DIR with :-$DOTFILES_DIR/claude-global fallback
# ---------------------------------------------------------------------------
echo ""
echo "=== N3: .profile_common — export AGENTS_CONFIG_DIR ==="

if grep -qE 'export AGENTS_CONFIG_DIR=.*:-.*DOTFILES_DIR.*claude-global' "$PROFILE_COMMON"; then
    pass "N3. .profile_common exports AGENTS_CONFIG_DIR with :-\$DOTFILES_DIR/claude-global fallback"
else
    fail "N3. .profile_common does not export AGENTS_CONFIG_DIR with expected fallback"
fi

# ---------------------------------------------------------------------------
# N4: .profile_common exports AGENTS_DIR with :-$DOTFILES_DIR fallback
# ---------------------------------------------------------------------------
echo ""
echo "=== N4: .profile_common — export AGENTS_DIR ==="

if grep -qE 'export AGENTS_DIR=.*:-.*DOTFILES_DIR[^/]' "$PROFILE_COMMON"; then
    pass "N4. .profile_common exports AGENTS_DIR with :-\$DOTFILES_DIR fallback"
else
    fail "N4. .profile_common does not export AGENTS_DIR with expected fallback"
fi

# ---------------------------------------------------------------------------
# N5: profile.ps1 sets $env:AGENTS_CONFIG_DIR with $DotfilesDir\claude-global fallback
# ---------------------------------------------------------------------------
echo ""
echo "=== N5: profile.ps1 — \$env:AGENTS_CONFIG_DIR ==="

if grep -qE '\$env:AGENTS_CONFIG_DIR.*=.*DotfilesDir.*claude-global' "$PROFILE_PS1"; then
    pass "N5. profile.ps1 sets \$env:AGENTS_CONFIG_DIR with \$DotfilesDir\\claude-global fallback"
else
    fail "N5. profile.ps1 does not set \$env:AGENTS_CONFIG_DIR with expected fallback"
fi

# ---------------------------------------------------------------------------
# N6: profile.ps1 sets $env:AGENTS_DIR with $DotfilesDir fallback
# ---------------------------------------------------------------------------
echo ""
echo "=== N6: profile.ps1 — \$env:AGENTS_DIR ==="

if grep -qE '\$env:AGENTS_DIR[[:space:]]*=.*DotfilesDir' "$PROFILE_PS1"; then
    pass "N6. profile.ps1 sets \$env:AGENTS_DIR with \$DotfilesDir fallback"
else
    fail "N6. profile.ps1 does not set \$env:AGENTS_DIR with expected fallback"
fi

# ---------------------------------------------------------------------------
# E1: .profile_common compat block appears AFTER export DOTFILES_DIR line
# ---------------------------------------------------------------------------
echo ""
echo "=== E1: .profile_common — AGENTS_CONFIG_DIR defined after DOTFILES_DIR ==="

DOTFILES_DIR_LINE=$(grep -n 'export DOTFILES_DIR=' "$PROFILE_COMMON" | head -1 | cut -d: -f1)
AGENTS_CONFIG_LINE=$(grep -n 'export AGENTS_CONFIG_DIR=' "$PROFILE_COMMON" | head -1 | cut -d: -f1)

if [ -n "$DOTFILES_DIR_LINE" ] && [ -n "$AGENTS_CONFIG_LINE" ] && [ "$AGENTS_CONFIG_LINE" -gt "$DOTFILES_DIR_LINE" ]; then
    pass "E1. AGENTS_CONFIG_DIR (line $AGENTS_CONFIG_LINE) appears after DOTFILES_DIR (line $DOTFILES_DIR_LINE)"
else
    fail "E1. AGENTS_CONFIG_DIR (line ${AGENTS_CONFIG_LINE:-?}) does not appear after DOTFILES_DIR (line ${DOTFILES_DIR_LINE:-?})"
fi

# ---------------------------------------------------------------------------
# E2: profile.ps1 compat block appears AFTER $env:DOTFILES_DIR = $DotfilesDir line
# ---------------------------------------------------------------------------
echo ""
echo "=== E2: profile.ps1 — AGENTS_CONFIG_DIR defined after \$env:DOTFILES_DIR ==="

DOTFILES_ENV_LINE=$(grep -n '\$env:DOTFILES_DIR' "$PROFILE_PS1" | head -1 | cut -d: -f1)
AGENTS_PS1_LINE=$(grep -n '\$env:AGENTS_CONFIG_DIR' "$PROFILE_PS1" | head -1 | cut -d: -f1)

if [ -n "$DOTFILES_ENV_LINE" ] && [ -n "$AGENTS_PS1_LINE" ] && [ "$AGENTS_PS1_LINE" -gt "$DOTFILES_ENV_LINE" ]; then
    pass "E2. \$env:AGENTS_CONFIG_DIR (line $AGENTS_PS1_LINE) appears after \$env:DOTFILES_DIR (line $DOTFILES_ENV_LINE)"
else
    fail "E2. \$env:AGENTS_CONFIG_DIR (line ${AGENTS_PS1_LINE:-?}) does not appear after \$env:DOTFILES_DIR (line ${DOTFILES_ENV_LINE:-?})"
fi

# ---------------------------------------------------------------------------
# E3: settings.json is valid JSON
# ---------------------------------------------------------------------------
echo ""
echo "=== E3: settings.json — valid JSON ==="

if node -e "JSON.parse(require('fs').readFileSync(process.argv[1],'utf8'))" -- "$SETTINGS" 2>/dev/null; then
    pass "E3. settings.json is valid JSON"
else
    fail "E3. settings.json failed JSON parse"
fi

# ---------------------------------------------------------------------------
# N7: pre-commit uses AGENTS_CONFIG_DIR to locate scanner (no old DOTFILES_DIR path)
# ---------------------------------------------------------------------------
echo ""
echo "=== N7: pre-commit — scanner path uses AGENTS_CONFIG_DIR ==="

PRE_COMMIT="$DOTFILES_DIR/claude-global/hooks/pre-commit"
if grep -q '_cfg_dir.*AGENTS_CONFIG_DIR' "$PRE_COMMIT" && grep -q 'SCANNER=.*_cfg_dir.*bin/scan-outbound' "$PRE_COMMIT"; then
    pass "N7. pre-commit uses AGENTS_CONFIG_DIR to locate scan-outbound.sh"
else
    fail "N7. pre-commit does not use AGENTS_CONFIG_DIR for scanner path"
fi

# ---------------------------------------------------------------------------
# N8: commit-msg uses AGENTS_CONFIG_DIR to locate scanner
# ---------------------------------------------------------------------------
echo ""
echo "=== N8: commit-msg — scanner path uses AGENTS_CONFIG_DIR ==="

COMMIT_MSG="$DOTFILES_DIR/claude-global/hooks/commit-msg"
if grep -q '_cfg_dir.*AGENTS_CONFIG_DIR' "$COMMIT_MSG" && grep -q 'SCANNER=.*_cfg_dir.*bin/scan-outbound' "$COMMIT_MSG"; then
    pass "N8. commit-msg uses AGENTS_CONFIG_DIR to locate scan-outbound.sh"
else
    fail "N8. commit-msg does not use AGENTS_CONFIG_DIR for scanner path"
fi

# ---------------------------------------------------------------------------
# N9: scan-outbound.sh uses DOTFILES_PRIVATE_DIR optional fallback
# ---------------------------------------------------------------------------
echo ""
echo "=== N9: scan-outbound.sh — dotfiles-private uses DOTFILES_PRIVATE_DIR fallback ==="

SCAN_OUTBOUND="$DOTFILES_DIR/bin/scan-outbound.sh"
if grep -q 'DOTFILES_PRIVATE_DIR:-' "$SCAN_OUTBOUND"; then
    pass "N9. scan-outbound.sh uses \${DOTFILES_PRIVATE_DIR:-...} fallback for private allowlist"
else
    fail "N9. scan-outbound.sh does not use DOTFILES_PRIVATE_DIR fallback"
fi

# ---------------------------------------------------------------------------
# Results
# ---------------------------------------------------------------------------
echo ""
echo "=== Results ==="
if [ "$ERRORS" -eq 0 ]; then
    echo "All tests passed!"
else
    echo "$ERRORS test(s) failed"
    exit 1
fi
