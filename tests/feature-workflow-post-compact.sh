#!/bin/bash
# TDD tests for claude-global/hooks/post-compact.js
# File does NOT exist yet — all tests are expected to FAIL (TDD placeholders).
set -euo pipefail

DOTFILES_DIR="$(cd "$(dirname "$0")/.." && pwd)"
POST_COMPACT="$DOTFILES_DIR/claude-global/hooks/post-compact.js"
ERRORS=0

fail() { echo "FAIL: $1"; ERRORS=$((ERRORS + 1)); }
pass() { echo "PASS: $1"; }
expected_fail() { echo "Expected FAIL (not yet implemented): $1"; }

TMPDIR_BASE=$(mktemp -d)
WORKFLOW_DIR="$TMPDIR_BASE/workflow-state"
mkdir -p "$WORKFLOW_DIR"
trap 'rm -rf "$TMPDIR_BASE"' EXIT

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

run_with_timeout() {
    if command -v timeout >/dev/null 2>&1; then
        timeout 180 "$@"
    else
        perl -e 'alarm 180; exec @ARGV' -- "$@"
    fi
}

# ---------------------------------------------------------------------------
# B1: Normal — session_id present → valid JSON with additionalContext
# ---------------------------------------------------------------------------

echo ""
echo "=== B1: session_id present → stdout is valid JSON with additionalContext ==="

if [ ! -f "$POST_COMPACT" ]; then
    expected_fail "B1. post-compact.js does not exist yet"
else
    B1_SID="b1-test-id"
    B1_OUTPUT=$(echo "{\"session_id\":\"$B1_SID\"}" | CLAUDE_WORKFLOW_DIR="$WORKFLOW_DIR" run_with_timeout node "$POST_COMPACT" 2>/dev/null || echo "ERROR_EXIT")

    # Assert: valid JSON
    B1_VALID_JSON=$(printf '%s' "$B1_OUTPUT" | node -e "
try { JSON.parse(require('fs').readFileSync('/dev/stdin','utf8')); console.log('yes'); } catch(e) { console.log('no'); }
" 2>/dev/null || echo "no")

    if [ "$B1_VALID_JSON" != "yes" ]; then
        fail "B1. output is not valid JSON: $B1_OUTPUT"
    else
        # Assert: additionalContext field exists
        B1_HAS_CTX=$(printf '%s' "$B1_OUTPUT" | node -e "
try { const o=JSON.parse(require('fs').readFileSync('/dev/stdin','utf8')); console.log(typeof o.additionalContext === 'string' ? 'yes' : 'no'); } catch(e) { console.log('no'); }
" 2>/dev/null || echo "no")

        if [ "$B1_HAS_CTX" != "yes" ]; then
            fail "B1. additionalContext field missing in output: $B1_OUTPUT"
        else
            # Assert: additionalContext contains session_id
            B1_HAS_SID=$(printf '%s' "$B1_OUTPUT" | node -e "
try { const o=JSON.parse(require('fs').readFileSync('/dev/stdin','utf8')); console.log(o.additionalContext.includes('$B1_SID') ? 'yes' : 'no'); } catch(e) { console.log('no'); }
" 2>/dev/null || echo "no")

            # Assert: additionalContext contains state path
            B1_HAS_PATH=$(printf '%s' "$B1_OUTPUT" | node -e "
try { const o=JSON.parse(require('fs').readFileSync('/dev/stdin','utf8')); console.log((o.additionalContext.includes('.json') || o.additionalContext.includes('State file')) ? 'yes' : 'no'); } catch(e) { console.log('no'); }
" 2>/dev/null || echo "no")

            if [ "$B1_HAS_SID" = "yes" ] && [ "$B1_HAS_PATH" = "yes" ]; then
                pass "B1. additionalContext contains session_id and state path"
            elif [ "$B1_HAS_SID" != "yes" ]; then
                fail "B1. additionalContext does not contain session_id '$B1_SID': $B1_OUTPUT"
            else
                fail "B1. additionalContext does not contain state path: $B1_OUTPUT"
            fi
        fi
    fi
fi

# ---------------------------------------------------------------------------
# B2: Edge — session_id missing → exit 0, stdout is {}
# ---------------------------------------------------------------------------

echo ""
echo "=== B2: session_id missing → exit 0, stdout is {} ==="

if [ ! -f "$POST_COMPACT" ]; then
    expected_fail "B2. post-compact.js does not exist yet"
else
    B2_EXIT=0
    B2_OUTPUT=$(echo '{"other_key":"value"}' | CLAUDE_WORKFLOW_DIR="$WORKFLOW_DIR" run_with_timeout node "$POST_COMPACT" 2>/dev/null) || B2_EXIT=$?

    if [ "$B2_EXIT" != "0" ]; then
        fail "B2. expected exit 0 but got $B2_EXIT"
    else
        B2_IS_EMPTY=$(printf '%s' "$B2_OUTPUT" | node -e "
try {
  const o = JSON.parse(require('fs').readFileSync('/dev/stdin','utf8'));
  console.log(Object.keys(o).length === 0 ? 'yes' : 'no');
} catch(e) { console.log('no'); }
" 2>/dev/null || echo "no")
        if [ "$B2_IS_EMPTY" = "yes" ]; then
            pass "B2. session_id missing → exit 0 and stdout is {}"
        else
            fail "B2. session_id missing → stdout is not {}: $B2_OUTPUT"
        fi
    fi
fi

# ---------------------------------------------------------------------------
# B3: Error — malformed JSON stdin → exit 0, stdout is {}
# ---------------------------------------------------------------------------

echo ""
echo "=== B3: malformed JSON stdin → exit 0, stdout is {} ==="

if [ ! -f "$POST_COMPACT" ]; then
    expected_fail "B3. post-compact.js does not exist yet"
else
    B3_EXIT=0
    B3_OUTPUT=$(echo 'not json at all' | CLAUDE_WORKFLOW_DIR="$WORKFLOW_DIR" run_with_timeout node "$POST_COMPACT" 2>/dev/null) || B3_EXIT=$?

    if [ "$B3_EXIT" != "0" ]; then
        fail "B3. expected exit 0 but got $B3_EXIT"
    else
        B3_IS_EMPTY=$(printf '%s' "$B3_OUTPUT" | node -e "
try {
  const o = JSON.parse(require('fs').readFileSync('/dev/stdin','utf8'));
  console.log(Object.keys(o).length === 0 ? 'yes' : 'no');
} catch(e) { console.log('no'); }
" 2>/dev/null || echo "no")
        if [ "$B3_IS_EMPTY" = "yes" ]; then
            pass "B3. malformed JSON → exit 0 and stdout is {}"
        else
            fail "B3. malformed JSON → stdout is not {}: $B3_OUTPUT"
        fi
    fi
fi

# ---------------------------------------------------------------------------
# Results
# ---------------------------------------------------------------------------

echo ""
echo "=== Results ==="
if [ "$ERRORS" -eq 0 ]; then
    echo "All tests passed (some may be Expected FAIL placeholders — that is OK)!"
else
    echo "$ERRORS test(s) failed"
    exit 1
fi
