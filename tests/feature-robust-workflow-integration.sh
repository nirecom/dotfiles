#!/bin/bash
# TDD integration tests: PB-gate-removed, PB-mark-removed, INT-1
# Features NOT YET IMPLEMENTED — some tests are expected to FAIL.
set -euo pipefail

DOTFILES_DIR="$(cd "$(dirname "$0")/.." && pwd)"
GATE_HOOK="$DOTFILES_DIR/claude-global/hooks/workflow-gate.js"
MARK_HOOK="$DOTFILES_DIR/claude-global/hooks/workflow-mark.js"
SESSION_START="$DOTFILES_DIR/claude-global/hooks/session-start.js"
ERRORS=0

fail() { echo "FAIL: $1"; ERRORS=$((ERRORS + 1)); }
pass() { echo "PASS: $1"; }

TMPDIR_BASE=$(mktemp -d)
trap 'rm -rf "$TMPDIR_BASE"' EXIT

setup_repo() {
    local repo="$TMPDIR_BASE/repo-$RANDOM"
    mkdir -p "$repo"
    git -C "$repo" init -q
    git -C "$repo" config user.email "test@example.com"
    git -C "$repo" config user.name "Test"
    echo "init" > "$repo/README.md"
    git -C "$repo" add README.md
    git -C "$repo" commit -q -m "initial"
    echo "$repo"
}

write_state() {
    local repo="$1" sid="$2" json="$3"
    local gitdir
    gitdir=$(git -C "$repo" rev-parse --git-dir)
    mkdir -p "$repo/$gitdir/workflow"
    printf '%s' "$json" > "$repo/$gitdir/workflow/${sid}.json"
}

read_state_status() {
    local repo="$1" sid="$2" step="$3"
    local gitdir
    gitdir=$(git -C "$repo" rev-parse --git-dir 2>/dev/null || echo ".git")
    local state_file="$repo/$gitdir/workflow/${sid}.json"
    if [ ! -f "$state_file" ]; then echo "MISSING"; return; fi
    node -e "
      try {
        const s = JSON.parse(require('fs').readFileSync(process.argv[1], 'utf8'));
        const step = s.steps && s.steps['$step'];
        console.log(step && step.status ? step.status : 'MISSING');
      } catch (e) { console.log('MISSING'); }
    " "$state_file" 2>/dev/null || echo "MISSING"
}

# ---------------------------------------------------------------------------
# PB-gate-removed: workflow-gate.js does NOT call isPrivateRepo
# Expected FAIL before implementation (bypass still present)
# ---------------------------------------------------------------------------

echo ""
echo "=== integration: PB-gate-removed — workflow-gate.js has no isPrivateRepo ==="

GATE_COUNT=$(grep -c "isPrivateRepo" "$GATE_HOOK" 2>/dev/null || true)
if [ "${GATE_COUNT:-0}" = "0" ]; then
    pass "PB-gate-removed. workflow-gate.js does not call isPrivateRepo"
else
    fail "PB-gate-removed. workflow-gate.js still contains $GATE_COUNT isPrivateRepo reference(s)"
fi

# ---------------------------------------------------------------------------
# PB-mark-removed: workflow-mark.js does NOT call isPrivateRepo
# Expected FAIL before implementation (bypass still present)
# ---------------------------------------------------------------------------

echo ""
echo "=== integration: PB-mark-removed — workflow-mark.js has no isPrivateRepo ==="

MARK_COUNT=$(grep -c "isPrivateRepo" "$MARK_HOOK" 2>/dev/null || true)
if [ "${MARK_COUNT:-0}" = "0" ]; then
    pass "PB-mark-removed. workflow-mark.js does not call isPrivateRepo"
else
    fail "PB-mark-removed. workflow-mark.js still contains $MARK_COUNT isPrivateRepo reference(s)"
fi

# ---------------------------------------------------------------------------
# INT-1: session-start creates state → gate reports "not complete" (not "no workflow state")
# Expected FAIL before implementation (session-start doesn't create state yet)
# ---------------------------------------------------------------------------

echo ""
echo "=== integration: INT-1 — session-start + gate flow ==="

REPO=$(setup_repo)
SID="int1-$(printf '%04x%04x' $RANDOM $RANDOM)"
ENV_FILE="$TMPDIR_BASE/int1.env"

# Step 1: Run session-start to (eventually) create the state file
echo "{\"session_id\":\"$SID\"}" | CLAUDE_PROJECT_DIR="$REPO" CLAUDE_ENV_FILE="$ENV_FILE" node "$SESSION_START" 2>/dev/null || true

# Step 2: Run workflow-gate with a git commit command and the same session_id
COMMIT_JSON="{\"tool_name\":\"Bash\",\"tool_input\":{\"command\":\"git commit -m test\"},\"session_id\":\"$SID\"}"
RESULT=$(echo "$COMMIT_JSON" | CLAUDE_PROJECT_DIR="$REPO" node "$GATE_HOOK" 2>/dev/null || true)

# INT-1a: decision must be "block"
INT1A_OK=$(node -e "
try {
  const r = JSON.parse(process.argv[1]);
  process.exit(r.decision === 'block' ? 0 : 1);
} catch(e) { process.exit(1); }
" "$RESULT" 2>/dev/null; echo $?)
if [ "$INT1A_OK" = "0" ]; then
    pass "INT-1a. gate decision is 'block'"
else
    fail "INT-1a. gate decision is not 'block' (got: $RESULT)"
fi

# INT-1b: reason must contain "not complete"
INT1B_OK=$(node -e "
try {
  const r = JSON.parse(process.argv[1]);
  process.exit((r.reason||'').includes('not complete') ? 0 : 1);
} catch(e) { process.exit(1); }
" "$RESULT" 2>/dev/null; echo $?)
if [ "$INT1B_OK" = "0" ]; then
    pass "INT-1b. gate reason contains 'not complete'"
else
    fail "INT-1b. gate reason does not contain 'not complete' (got: $RESULT)"
fi

# INT-1c: reason must NOT contain "no workflow state"
INT1C_OK=$(node -e "
try {
  const r = JSON.parse(process.argv[1]);
  process.exit((r.reason||'').includes('no workflow state') ? 1 : 0);
} catch(e) { process.exit(1); }
" "$RESULT" 2>/dev/null; echo $?)
if [ "$INT1C_OK" = "0" ]; then
    pass "INT-1c. gate reason does NOT contain 'no workflow state'"
else
    fail "INT-1c. gate reason contains 'no workflow state' — session-start did not pre-create state (got: $RESULT)"
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
