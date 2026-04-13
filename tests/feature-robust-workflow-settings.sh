#!/bin/bash
# TDD tests for settings.json new entries (SR-D3-1 through SR-D3-5)
# Features NOT YET IMPLEMENTED — all SR-D3-* tests are expected to FAIL.
set -euo pipefail

DOTFILES_DIR="$(cd "$(dirname "$0")/.." && pwd)"
SETTINGS="$DOTFILES_DIR/claude-global/settings.json"
ERRORS=0

fail() { echo "FAIL: $1"; ERRORS=$((ERRORS + 1)); }
pass() { echo "PASS: $1"; }

# Verify settings.json exists
if [ ! -f "$SETTINGS" ]; then
    echo "FATAL: settings.json not found at $SETTINGS"
    exit 2
fi

# ---------------------------------------------------------------------------
# SR-D3-1: permissions.ask contains entry matching WORKFLOW_USER_VERIFIED
# Expected FAIL before settings.json is updated
# ---------------------------------------------------------------------------

echo ""
echo "=== settings.json: SR-D3-1 — ask contains WORKFLOW_USER_VERIFIED ==="

if node -e "
const s=JSON.parse(require('fs').readFileSync(process.argv[1],'utf8'));
const ask=s.permissions&&s.permissions.ask||[];
process.exit(ask.some(e=>e.includes('WORKFLOW_USER_VERIFIED')) ? 0 : 1);
" -- "$SETTINGS" 2>/dev/null; then
    pass "SR-D3-1. permissions.ask contains WORKFLOW_USER_VERIFIED entry"
else
    fail "SR-D3-1. permissions.ask does NOT contain WORKFLOW_USER_VERIFIED entry"
fi

# ---------------------------------------------------------------------------
# SR-D3-2: permissions.ask contains entry matching WORKFLOW_RESET_FROM
# Expected FAIL before settings.json is updated
# ---------------------------------------------------------------------------

echo ""
echo "=== settings.json: SR-D3-2 — ask contains WORKFLOW_RESET_FROM ==="

if node -e "
const s=JSON.parse(require('fs').readFileSync(process.argv[1],'utf8'));
const ask=s.permissions&&s.permissions.ask||[];
process.exit(ask.some(e=>e.includes('WORKFLOW_RESET_FROM')) ? 0 : 1);
" -- "$SETTINGS" 2>/dev/null; then
    pass "SR-D3-2. permissions.ask contains WORKFLOW_RESET_FROM entry"
else
    fail "SR-D3-2. permissions.ask does NOT contain WORKFLOW_RESET_FROM entry"
fi

# ---------------------------------------------------------------------------
# SR-D3-3: permissions.allow does NOT contain DQ WORKFLOW_RESET_FROM (moved to ask)
# Expected FAIL before settings.json is updated
# ---------------------------------------------------------------------------

echo ""
echo "=== settings.json: SR-D3-3 — allow does NOT contain DQ WORKFLOW_RESET_FROM ==="

if node -e "
const s=JSON.parse(require('fs').readFileSync(process.argv[1],'utf8'));
const allow=s.permissions&&s.permissions.allow||[];
// The DQ entry that should be removed: Bash(echo \"<<WORKFLOW_RESET_FROM:*>\">)
// It contains double-quote before <<WORKFLOW_RESET_FROM
const hasDQ = allow.some(e => e.includes('WORKFLOW_RESET_FROM') && e.includes('\"'));
process.exit(hasDQ ? 1 : 0);
" -- "$SETTINGS" 2>/dev/null; then
    pass "SR-D3-3. permissions.allow does NOT contain DQ WORKFLOW_RESET_FROM (correctly moved to ask)"
else
    fail "SR-D3-3. permissions.allow still contains DQ WORKFLOW_RESET_FROM entry (should be in ask)"
fi

# ---------------------------------------------------------------------------
# SR-D3-4: permissions.allow contains MARK_STEP entry (underscore format)
# ---------------------------------------------------------------------------

echo ""
echo "=== settings.json: SR-D3-4 — allow contains MARK_STEP (underscore format) ==="

if node -e "
const s=JSON.parse(require('fs').readFileSync(process.argv[1],'utf8'));
const allow=s.permissions&&s.permissions.allow||[];
// Underscore format: Bash(echo '<<WORKFLOW_MARK_STEP_*>>')
const has = allow.some(e => e.includes('WORKFLOW_MARK_STEP'));
process.exit(has ? 0 : 1);
" -- "$SETTINGS" 2>/dev/null; then
    pass "SR-D3-4. permissions.allow contains WORKFLOW_MARK_STEP entry"
else
    fail "SR-D3-4. permissions.allow does NOT contain WORKFLOW_MARK_STEP entry"
fi

# ---------------------------------------------------------------------------
# SR-D3-5: permissions.allow does NOT contain RESET_FROM entry (moved to ask)
# ---------------------------------------------------------------------------

echo ""
echo "=== settings.json: SR-D3-5 — allow does NOT contain RESET_FROM ==="

if node -e "
const s=JSON.parse(require('fs').readFileSync(process.argv[1],'utf8'));
const allow=s.permissions&&s.permissions.allow||[];
// RESET_FROM should be in ask, not allow
const hasResetFrom = allow.some(e => e.includes('WORKFLOW_RESET_FROM'));
process.exit(hasResetFrom ? 1 : 0);
" -- "$SETTINGS" 2>/dev/null; then
    pass "SR-D3-5. permissions.allow does NOT contain WORKFLOW_RESET_FROM entry (correctly in ask)"
else
    fail "SR-D3-5. permissions.allow still contains WORKFLOW_RESET_FROM entry (should be in ask)"
fi

# ---------------------------------------------------------------------------
# SR-D3-6: permissions.ask contains RESET_FROM entry (underscore format)
# ---------------------------------------------------------------------------

echo ""
echo "=== settings.json: SR-D3-6 — ask contains RESET_FROM (underscore format) ==="

if node -e "
const s=JSON.parse(require('fs').readFileSync(process.argv[1],'utf8'));
const ask=s.permissions&&s.permissions.ask||[];
process.exit(ask.some(e => e.includes('WORKFLOW_RESET_FROM_') && e.includes('>>')) ? 0 : 1);
" -- "$SETTINGS" 2>/dev/null; then
    pass "SR-D3-6. permissions.ask contains WORKFLOW_RESET_FROM_ (underscore format) entry"
else
    fail "SR-D3-6. permissions.ask does NOT contain WORKFLOW_RESET_FROM_ (underscore format) entry"
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
