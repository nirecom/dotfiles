#!/bin/bash
# Test suite for workflow state machine:
#   claude-global/hooks/workflow-gate.js   (PreToolUse commit gate)
#   claude-global/hooks/mark-step.js       (step completion CLI)
#   claude-global/hooks/session-start.js   (SessionStart hook)
set -euo pipefail

DOTFILES_DIR="$(cd "$(dirname "$0")/.." && pwd)"
GATE_HOOK="$DOTFILES_DIR/claude-global/hooks/workflow-gate.js"
MARK_STEP="$DOTFILES_DIR/claude-global/hooks/mark-step.js"
MARK_HOOK="$DOTFILES_DIR/claude-global/hooks/workflow-mark.js"
SESSION_START="$DOTFILES_DIR/claude-global/hooks/session-start.js"
ERRORS=0

fail() { echo "FAIL: $1"; ERRORS=$((ERRORS + 1)); }
pass() { echo "PASS: $1"; }

# ---------------------------------------------------------------------------
# Temporary git repo setup
# ---------------------------------------------------------------------------

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

# Create the workflow/<session-id>.json state file in the repo
# Usage: write_state <repo> <session_id> <json_content>
write_state() {
    local repo="$1" sid="$2" json="$3"
    local gitdir
    gitdir=$(git -C "$repo" rev-parse --git-dir)
    mkdir -p "$repo/$gitdir/workflow"
    printf '%s' "$json" > "$repo/$gitdir/workflow/${sid}.json"
}

# ---------------------------------------------------------------------------
# Helper: all-complete state JSON
# ---------------------------------------------------------------------------

ALL_COMPLETE_JSON() {
    local sid="${1:-test-session}"
    cat <<EOF
{
  "version": 1,
  "session_id": "$sid",
  "created_at": "2026-04-11T10:00:00.000Z",
  "steps": {
    "research":          {"status": "complete", "updated_at": "2026-04-11T10:01:00.000Z"},
    "plan":              {"status": "complete", "updated_at": "2026-04-11T10:02:00.000Z"},
    "write_tests":       {"status": "complete", "updated_at": "2026-04-11T10:03:00.000Z"},
    "code":              {"status": "complete", "updated_at": "2026-04-11T10:04:00.000Z"},
    "verify":            {"status": "complete", "updated_at": "2026-04-11T10:05:00.000Z"},
    "docs":              {"status": "complete", "updated_at": "2026-04-11T10:06:00.000Z"},
    "user_verification": {"status": "complete", "updated_at": "2026-04-11T10:07:00.000Z"}
  }
}
EOF
}

ALL_PENDING_JSON() {
    local sid="${1:-test-session}"
    cat <<EOF
{
  "version": 1,
  "session_id": "$sid",
  "created_at": "2026-04-11T10:00:00.000Z",
  "steps": {
    "research":          {"status": "pending", "updated_at": null},
    "plan":              {"status": "pending", "updated_at": null},
    "write_tests":       {"status": "pending", "updated_at": null},
    "code":              {"status": "pending", "updated_at": null},
    "verify":            {"status": "pending", "updated_at": null},
    "docs":              {"status": "pending", "updated_at": null},
    "user_verification": {"status": "pending", "updated_at": null}
  }
}
EOF
}

COMMIT_JSON='{"tool_name":"Bash","tool_input":{"command":"git commit -m \"test\""},"session_id":"test-session"}'

# ---------------------------------------------------------------------------
# workflow-gate.js helpers
# ---------------------------------------------------------------------------

run_gate() {
    local repo="$1" json="$2"
    echo "$json" | CLAUDE_PROJECT_DIR="$repo" node "$GATE_HOOK" 2>/dev/null || true
}

expect_approve_gate() {
    local desc="$1" repo="$2" json="$3"
    local result
    result=$(run_gate "$repo" "$json")
    if echo "$result" | grep -q '"approve"'; then pass "$desc"
    else fail "$desc — expected approve, got: $result"; fi
}

expect_block_gate() {
    local desc="$1" repo="$2" json="$3"
    local result
    result=$(run_gate "$repo" "$json")
    if echo "$result" | grep -q '"block"'; then pass "$desc"
    else fail "$desc — expected block, got: $result"; fi
}

expect_block_gate_contains() {
    local desc="$1" repo="$2" json="$3" needle="$4"
    local result
    result=$(run_gate "$repo" "$json")
    if echo "$result" | grep -q '"block"' && echo "$result" | grep -qi "$needle"; then
        pass "$desc"
    else
        fail "$desc — expected block containing '$needle', got: $result"
    fi
}

# ---------------------------------------------------------------------------
# workflow-mark.js helpers
# ---------------------------------------------------------------------------

run_mark_hook() {
    local repo="$1" json="$2"
    echo "$json" | CLAUDE_PROJECT_DIR="$repo" node "$MARK_HOOK" 2>/dev/null || true
}

# Read the state file and extract steps.<step>.status using node.
# Prints the status string, or "MISSING" if the file / step is absent.
read_state_status() {
    local repo="$1" sid="$2" step="$3"
    local gitdir
    gitdir=$(git -C "$repo" rev-parse --git-dir 2>/dev/null || echo ".git")
    local state_file="$repo/$gitdir/workflow/${sid}.json"
    if [ ! -f "$state_file" ]; then
        echo "MISSING"
        return
    fi
    node -e "
      try {
        const s = JSON.parse(require('fs').readFileSync(process.argv[1], 'utf8'));
        const step = s.steps && s.steps['$step'];
        console.log(step && step.status ? step.status : 'MISSING');
      } catch (e) { console.log('MISSING'); }
    " "$state_file" 2>/dev/null || echo "MISSING"
}

expect_state_step() {
    local desc="$1" repo="$2" sid="$3" step="$4" expected="$5"
    local actual
    actual=$(read_state_status "$repo" "$sid" "$step")
    if [ "$actual" = "$expected" ]; then
        pass "$desc"
    else
        fail "$desc — expected steps.$step.status=$expected, got: $actual"
    fi
}

expect_no_state_change() {
    local desc="$1" repo="$2" sid="$3" step="$4" expected_unchanged="$5"
    local actual
    actual=$(read_state_status "$repo" "$sid" "$step")
    if [ "$actual" = "$expected_unchanged" ]; then
        pass "$desc"
    else
        fail "$desc — expected steps.$step.status to remain $expected_unchanged, got: $actual"
    fi
}

# ---------------------------------------------------------------------------
# === workflow-gate.js: Normal cases (approve) ===
# ---------------------------------------------------------------------------

echo "=== workflow-gate: Normal cases (approve) ==="

# Test 1: All 7 steps complete → approve
REPO=$(setup_repo)
write_state "$REPO" "test-session" "$(ALL_COMPLETE_JSON test-session)"
expect_approve_gate "1. All 7 steps complete → approve" "$REPO" "$COMMIT_JSON"

# Test 2: research skipped, rest complete → approve
REPO=$(setup_repo)
write_state "$REPO" "test-session" '{
  "version": 1,
  "session_id": "test-session",
  "created_at": "2026-04-11T10:00:00.000Z",
  "steps": {
    "research":          {"status": "skipped",  "updated_at": "2026-04-11T10:01:00.000Z"},
    "plan":              {"status": "complete",  "updated_at": "2026-04-11T10:02:00.000Z"},
    "write_tests":       {"status": "complete",  "updated_at": "2026-04-11T10:03:00.000Z"},
    "code":              {"status": "complete",  "updated_at": "2026-04-11T10:04:00.000Z"},
    "verify":            {"status": "complete",  "updated_at": "2026-04-11T10:05:00.000Z"},
    "docs":              {"status": "complete",  "updated_at": "2026-04-11T10:06:00.000Z"},
    "user_verification": {"status": "complete",  "updated_at": "2026-04-11T10:07:00.000Z"}
  }
}'
expect_approve_gate "2. research skipped, rest complete → approve" "$REPO" "$COMMIT_JSON"

# Test 3: plan skipped, rest complete → approve
REPO=$(setup_repo)
write_state "$REPO" "test-session" '{
  "version": 1,
  "session_id": "test-session",
  "created_at": "2026-04-11T10:00:00.000Z",
  "steps": {
    "research":          {"status": "complete",  "updated_at": "2026-04-11T10:01:00.000Z"},
    "plan":              {"status": "skipped",   "updated_at": "2026-04-11T10:02:00.000Z"},
    "write_tests":       {"status": "complete",  "updated_at": "2026-04-11T10:03:00.000Z"},
    "code":              {"status": "complete",  "updated_at": "2026-04-11T10:04:00.000Z"},
    "verify":            {"status": "complete",  "updated_at": "2026-04-11T10:05:00.000Z"},
    "docs":              {"status": "complete",  "updated_at": "2026-04-11T10:06:00.000Z"},
    "user_verification": {"status": "complete",  "updated_at": "2026-04-11T10:07:00.000Z"}
  }
}'
expect_approve_gate "3. plan skipped, rest complete → approve" "$REPO" "$COMMIT_JSON"

# Test 4: git -C /path commit form → correctly intercepted (block when state file missing)
REPO=$(setup_repo)
GIT_C_JSON="{\"tool_name\":\"Bash\",\"tool_input\":{\"command\":\"git -C $REPO commit -m msg\"},\"session_id\":\"test-session\"}"
expect_block_gate "4. git -C /path commit → intercepted (block when state missing)" "$REPO" "$GIT_C_JSON"

# ---------------------------------------------------------------------------
# === workflow-gate.js: Error/block cases ===
# ---------------------------------------------------------------------------

echo ""
echo "=== workflow-gate: Error/block cases ==="

# Test 5: research pending → block, message contains "research"
REPO=$(setup_repo)
write_state "$REPO" "test-session" '{
  "version": 1,
  "session_id": "test-session",
  "created_at": "2026-04-11T10:00:00.000Z",
  "steps": {
    "research":          {"status": "pending",  "updated_at": null},
    "plan":              {"status": "complete", "updated_at": "2026-04-11T10:02:00.000Z"},
    "write_tests":       {"status": "complete", "updated_at": "2026-04-11T10:03:00.000Z"},
    "code":              {"status": "complete", "updated_at": "2026-04-11T10:04:00.000Z"},
    "verify":            {"status": "complete", "updated_at": "2026-04-11T10:05:00.000Z"},
    "docs":              {"status": "complete", "updated_at": "2026-04-11T10:06:00.000Z"},
    "user_verification": {"status": "complete", "updated_at": "2026-04-11T10:07:00.000Z"}
  }
}'
expect_block_gate_contains "5. research pending → block with 'research' in message" "$REPO" "$COMMIT_JSON" "research"

# Test 6: Multiple steps pending (plan, write_tests) → block, message contains both
REPO=$(setup_repo)
write_state "$REPO" "test-session" '{
  "version": 1,
  "session_id": "test-session",
  "created_at": "2026-04-11T10:00:00.000Z",
  "steps": {
    "research":          {"status": "complete", "updated_at": "2026-04-11T10:01:00.000Z"},
    "plan":              {"status": "pending",  "updated_at": null},
    "write_tests":       {"status": "pending",  "updated_at": null},
    "code":              {"status": "complete", "updated_at": "2026-04-11T10:04:00.000Z"},
    "verify":            {"status": "complete", "updated_at": "2026-04-11T10:05:00.000Z"},
    "docs":              {"status": "complete", "updated_at": "2026-04-11T10:06:00.000Z"},
    "user_verification": {"status": "complete", "updated_at": "2026-04-11T10:07:00.000Z"}
  }
}'
RESULT=$(run_gate "$REPO" "$COMMIT_JSON")
if echo "$RESULT" | grep -q '"block"' && echo "$RESULT" | grep -qi "plan" && echo "$RESULT" | grep -qi "write_tests"; then
    pass "6. Multiple pending steps → block, message contains both step names"
else
    fail "6. Multiple pending steps → expected block with 'plan' and 'write_tests', got: $RESULT"
fi

# Test 7: write_tests set to skipped (non-skippable) → block
REPO=$(setup_repo)
write_state "$REPO" "test-session" '{
  "version": 1,
  "session_id": "test-session",
  "created_at": "2026-04-11T10:00:00.000Z",
  "steps": {
    "research":          {"status": "complete", "updated_at": "2026-04-11T10:01:00.000Z"},
    "plan":              {"status": "complete", "updated_at": "2026-04-11T10:02:00.000Z"},
    "write_tests":       {"status": "skipped",  "updated_at": "2026-04-11T10:03:00.000Z"},
    "code":              {"status": "complete", "updated_at": "2026-04-11T10:04:00.000Z"},
    "verify":            {"status": "complete", "updated_at": "2026-04-11T10:05:00.000Z"},
    "docs":              {"status": "complete", "updated_at": "2026-04-11T10:06:00.000Z"},
    "user_verification": {"status": "complete", "updated_at": "2026-04-11T10:07:00.000Z"}
  }
}'
expect_block_gate "7. write_tests skipped (non-skippable) → block" "$REPO" "$COMMIT_JSON"

# Test 8: user_verification set to skipped → block
REPO=$(setup_repo)
write_state "$REPO" "test-session" '{
  "version": 1,
  "session_id": "test-session",
  "created_at": "2026-04-11T10:00:00.000Z",
  "steps": {
    "research":          {"status": "complete", "updated_at": "2026-04-11T10:01:00.000Z"},
    "plan":              {"status": "complete", "updated_at": "2026-04-11T10:02:00.000Z"},
    "write_tests":       {"status": "complete", "updated_at": "2026-04-11T10:03:00.000Z"},
    "code":              {"status": "complete", "updated_at": "2026-04-11T10:04:00.000Z"},
    "verify":            {"status": "complete", "updated_at": "2026-04-11T10:05:00.000Z"},
    "docs":              {"status": "complete", "updated_at": "2026-04-11T10:06:00.000Z"},
    "user_verification": {"status": "skipped",  "updated_at": "2026-04-11T10:07:00.000Z"}
  }
}'
expect_block_gate "8. user_verification skipped → block" "$REPO" "$COMMIT_JSON"

# ---------------------------------------------------------------------------
# === workflow-gate.js: Fail-safe cases (BLOCK, not approve) ===
# ---------------------------------------------------------------------------

echo ""
echo "=== workflow-gate: Fail-safe cases (block) ==="

# Test 9: session_id missing from stdin → block (fail-safe)
REPO=$(setup_repo)
NO_SID_JSON='{"tool_name":"Bash","tool_input":{"command":"git commit -m test"}}'
expect_block_gate "9. session_id missing from stdin → block (fail-safe)" "$REPO" "$NO_SID_JSON"

# Test 10: State file not found → block (fail-safe)
REPO=$(setup_repo)
# No write_state call — state file does not exist
expect_block_gate "10. State file not found → block (fail-safe)" "$REPO" "$COMMIT_JSON"

# Test 11: State JSON corrupted → block (fail-safe)
REPO=$(setup_repo)
write_state "$REPO" "test-session" "NOT VALID JSON }{{"
expect_block_gate "11. Corrupted state JSON → block (fail-safe)" "$REPO" "$COMMIT_JSON"

# ---------------------------------------------------------------------------
# === workflow-gate.js: Other edge cases ===
# ---------------------------------------------------------------------------

echo ""
echo "=== workflow-gate: Other edge cases ==="

# Test 12: Non-Bash tool (Read) → approve
REPO=$(setup_repo)
NON_BASH_JSON='{"tool_name":"Read","tool_input":{"file_path":"README.md"},"session_id":"test-session"}'
expect_approve_gate "12. Non-Bash tool (Read) → approve" "$REPO" "$NON_BASH_JSON"

# Test 13: git status command (not commit) → approve
REPO=$(setup_repo)
GIT_STATUS_JSON='{"tool_name":"Bash","tool_input":{"command":"git status"},"session_id":"test-session"}'
expect_approve_gate "13. git status (not commit) → approve" "$REPO" "$GIT_STATUS_JSON"

# Test 14: Private repo → approve
# SKIPPED: requires gh API network call or complex mocking.
# The is-private-repo.js module calls `gh api repos/<id> --jq .private` which
# requires GitHub authentication and network access. Mocking it in a shell test
# would require modifying the module or adding test-only injection points.
echo "SKIP: 14. Private repo → approve (requires gh API / network)"

# Test 15: Empty/missing tool_input → approve
REPO=$(setup_repo)
EMPTY_INPUT_JSON='{"tool_name":"Bash","session_id":"test-session"}'
expect_approve_gate "15. Missing tool_input → approve" "$REPO" "$EMPTY_INPUT_JSON"

# ---------------------------------------------------------------------------
# === workflow-gate.js: Idempotency ===
# ---------------------------------------------------------------------------

echo ""
echo "=== workflow-gate: Idempotency ==="

# Test 16: Same state, hook called twice → identical result
REPO=$(setup_repo)
write_state "$REPO" "test-session" "$(ALL_PENDING_JSON test-session)"
RESULT1=$(run_gate "$REPO" "$COMMIT_JSON")
RESULT2=$(run_gate "$REPO" "$COMMIT_JSON")
if [ "$RESULT1" = "$RESULT2" ]; then pass "16. Idempotent block result"
else fail "16. Idempotent block — results differ: '$RESULT1' vs '$RESULT2'"; fi

# ---------------------------------------------------------------------------
# mark-step.js helpers
# ---------------------------------------------------------------------------

run_mark_step() {
    local env_file="$1"; shift
    CLAUDE_ENV_FILE="$env_file" node "$MARK_STEP" "$@" 2>/dev/null
}

run_mark_step_with_stderr() {
    local env_file="$1"; shift
    CLAUDE_ENV_FILE="$env_file" node "$MARK_STEP" "$@" 2>&1 || true
}

get_state_file() {
    local repo="$1" sid="$2"
    local gitdir
    gitdir=$(git -C "$repo" rev-parse --git-dir)
    echo "$repo/$gitdir/workflow/${sid}.json"
}

read_state_field() {
    # read_state_field <state_file> <step> <field>
    # Uses node to parse JSON — avoids jq dependency
    node -e "let b='';process.stdin.on('data',c=>b+=c);process.stdin.on('end',()=>console.log(JSON.parse(b).steps['$2']['$3']))" < "$1" 2>/dev/null || echo "null"
}

# ---------------------------------------------------------------------------
# === mark-step.js: Normal cases ===
# ---------------------------------------------------------------------------

echo ""
echo "=== mark-step: Normal cases ==="

# Test 17: mark research complete → state file created, research=complete
REPO=$(setup_repo)
SID="sid-$RANDOM"
ENV_FILE=$(mktemp)
echo "CLAUDE_SESSION_ID=$SID" > "$ENV_FILE"
if CLAUDE_PROJECT_DIR="$REPO" CLAUDE_ENV_FILE="$ENV_FILE" node "$MARK_STEP" research complete 2>/dev/null; then
    STATE_FILE=$(get_state_file "$REPO" "$SID")
    if [ -f "$STATE_FILE" ]; then
        STATUS=$(node -e "let b='';process.stdin.on('data',c=>b+=c);process.stdin.on('end',()=>console.log(JSON.parse(b).steps.research.status))" < "$STATE_FILE" 2>/dev/null || echo "error")
        if [ "$STATUS" = "complete" ]; then pass "17. mark research complete → state file created"
        else fail "17. mark research complete → status='$STATUS', expected 'complete'"; fi
    else
        fail "17. mark research complete → state file not found at $STATE_FILE"
    fi
else
    fail "17. mark research complete → exit non-zero"
fi
rm -f "$ENV_FILE"

# Test 18: plan skipped → accepted (skippable step)
REPO=$(setup_repo)
SID="sid-$RANDOM"
ENV_FILE=$(mktemp)
echo "CLAUDE_SESSION_ID=$SID" > "$ENV_FILE"
if CLAUDE_PROJECT_DIR="$REPO" CLAUDE_ENV_FILE="$ENV_FILE" node "$MARK_STEP" plan skipped 2>/dev/null; then
    STATE_FILE=$(get_state_file "$REPO" "$SID")
    if [ -f "$STATE_FILE" ]; then
        STATUS=$(node -e "let b='';process.stdin.on('data',c=>b+=c);process.stdin.on('end',()=>console.log(JSON.parse(b).steps.plan.status))" < "$STATE_FILE" 2>/dev/null || echo "error")
        if [ "$STATUS" = "skipped" ]; then pass "18. plan skipped → state=skipped"
        else fail "18. plan skipped → status='$STATUS', expected 'skipped'"; fi
    else
        fail "18. plan skipped → state file not found"
    fi
else
    fail "18. plan skipped → exit non-zero"
fi
rm -f "$ENV_FILE"

# Test 19: State file doesn't exist → auto-created with all steps pending, then target updated
REPO=$(setup_repo)
SID="sid-$RANDOM"
ENV_FILE=$(mktemp)
echo "CLAUDE_SESSION_ID=$SID" > "$ENV_FILE"
CLAUDE_PROJECT_DIR="$REPO" CLAUDE_ENV_FILE="$ENV_FILE" node "$MARK_STEP" code complete 2>/dev/null || true
rm -f "$ENV_FILE"
STATE_FILE=$(get_state_file "$REPO" "$SID")
if [ -f "$STATE_FILE" ]; then
    CODE_STATUS=$(node -e "let b='';process.stdin.on('data',c=>b+=c);process.stdin.on('end',()=>console.log(JSON.parse(b).steps.code.status))" < "$STATE_FILE" 2>/dev/null || echo "error")
    RESEARCH_STATUS=$(node -e "let b='';process.stdin.on('data',c=>b+=c);process.stdin.on('end',()=>console.log(JSON.parse(b).steps.research.status))" < "$STATE_FILE" 2>/dev/null || echo "error")
    if [ "$CODE_STATUS" = "complete" ] && [ "$RESEARCH_STATUS" = "pending" ]; then
        pass "19. Auto-create state file with all pending, then update target step"
    else
        fail "19. Auto-create → code='$CODE_STATUS' (want complete), research='$RESEARCH_STATUS' (want pending)"
    fi
else
    fail "19. Auto-create → state file not found"
fi

# Test 20: updated_at field is set to a non-null ISO timestamp after marking
REPO=$(setup_repo)
SID="sid-$RANDOM"
ENV_FILE=$(mktemp)
echo "CLAUDE_SESSION_ID=$SID" > "$ENV_FILE"
CLAUDE_PROJECT_DIR="$REPO" CLAUDE_ENV_FILE="$ENV_FILE" node "$MARK_STEP" verify complete 2>/dev/null || true
rm -f "$ENV_FILE"
STATE_FILE=$(get_state_file "$REPO" "$SID")
if [ -f "$STATE_FILE" ]; then
    UPDATED=$(node -e "let b='';process.stdin.on('data',c=>b+=c);process.stdin.on('end',()=>console.log(JSON.parse(b).steps.verify.updated_at))" < "$STATE_FILE" 2>/dev/null || echo "null")
    if [ "$UPDATED" != "null" ] && [ -n "$UPDATED" ]; then
        pass "20. updated_at is non-null ISO timestamp after marking"
    else
        fail "20. updated_at is null or empty: '$UPDATED'"
    fi
else
    fail "20. updated_at test → state file not found"
fi

# ---------------------------------------------------------------------------
# === mark-step.js: --reset-from cases ===
# ---------------------------------------------------------------------------

echo ""
echo "=== mark-step: --reset-from cases ==="

STEPS_IN_ORDER="research plan write_tests code verify docs user_verification"

# Test 21: --reset-from docs → research/plan/write_tests/code/verify=complete, docs/user_verification=pending
REPO=$(setup_repo)
SID="sid-$RANDOM"
ENV_FILE=$(mktemp)
echo "CLAUDE_SESSION_ID=$SID" > "$ENV_FILE"
CLAUDE_PROJECT_DIR="$REPO" CLAUDE_ENV_FILE="$ENV_FILE" node "$MARK_STEP" --reset-from docs 2>/dev/null || true
rm -f "$ENV_FILE"
STATE_FILE=$(get_state_file "$REPO" "$SID")
if [ -f "$STATE_FILE" ]; then
    RESULT=$(node -e "
let b='';process.stdin.on('data',c=>b+=c);process.stdin.on('end',()=>{
  const s=JSON.parse(b);
  const ok = s.steps.research.status==='complete'
    && s.steps.plan.status==='complete'
    && s.steps.write_tests.status==='complete'
    && s.steps.code.status==='complete'
    && s.steps.verify.status==='complete'
    && s.steps.docs.status==='pending'
    && s.steps.user_verification.status==='pending';
  console.log(ok ? 'ok' : JSON.stringify(Object.fromEntries(Object.entries(s.steps).map(([k,v])=>[k,v.status]))));
})" < "$STATE_FILE" 2>/dev/null || echo "error")
    if [ "$RESULT" = "ok" ]; then pass "21. --reset-from docs"
    else fail "21. --reset-from docs → $RESULT"; fi
else
    fail "21. --reset-from docs → state file not found"
fi

# Test 22: --reset-from research → all 7 steps pending
REPO=$(setup_repo)
SID="sid-$RANDOM"
ENV_FILE=$(mktemp)
echo "CLAUDE_SESSION_ID=$SID" > "$ENV_FILE"
CLAUDE_PROJECT_DIR="$REPO" CLAUDE_ENV_FILE="$ENV_FILE" node "$MARK_STEP" --reset-from research 2>/dev/null || true
rm -f "$ENV_FILE"
STATE_FILE=$(get_state_file "$REPO" "$SID")
if [ -f "$STATE_FILE" ]; then
    RESULT=$(node -e "
let b='';process.stdin.on('data',c=>b+=c);process.stdin.on('end',()=>{
  const s=JSON.parse(b);
  const steps=['research','plan','write_tests','code','verify','docs','user_verification'];
  const ok=steps.every(k=>s.steps[k].status==='pending');
  console.log(ok ? 'ok' : JSON.stringify(Object.fromEntries(steps.map(k=>[k,s.steps[k].status]))));
})" < "$STATE_FILE" 2>/dev/null || echo "error")
    if [ "$RESULT" = "ok" ]; then pass "22. --reset-from research → all pending"
    else fail "22. --reset-from research → $RESULT"; fi
else
    fail "22. --reset-from research → state file not found"
fi

# Test 23: --reset-from user_verification → first 6 complete, user_verification pending
REPO=$(setup_repo)
SID="sid-$RANDOM"
ENV_FILE=$(mktemp)
echo "CLAUDE_SESSION_ID=$SID" > "$ENV_FILE"
CLAUDE_PROJECT_DIR="$REPO" CLAUDE_ENV_FILE="$ENV_FILE" node "$MARK_STEP" --reset-from user_verification 2>/dev/null || true
rm -f "$ENV_FILE"
STATE_FILE=$(get_state_file "$REPO" "$SID")
if [ -f "$STATE_FILE" ]; then
    RESULT=$(node -e "
let b='';process.stdin.on('data',c=>b+=c);process.stdin.on('end',()=>{
  const s=JSON.parse(b);
  const ok = s.steps.research.status==='complete'
    && s.steps.plan.status==='complete'
    && s.steps.write_tests.status==='complete'
    && s.steps.code.status==='complete'
    && s.steps.verify.status==='complete'
    && s.steps.docs.status==='complete'
    && s.steps.user_verification.status==='pending';
  console.log(ok ? 'ok' : JSON.stringify(Object.fromEntries(Object.entries(s.steps).map(([k,v])=>[k,v.status]))));
})" < "$STATE_FILE" 2>/dev/null || echo "error")
    if [ "$RESULT" = "ok" ]; then pass "23. --reset-from user_verification"
    else fail "23. --reset-from user_verification → $RESULT"; fi
else
    fail "23. --reset-from user_verification → state file not found"
fi

# Test 24: --reset-from with invalid step name → exit 1
REPO=$(setup_repo)
SID="sid-$RANDOM"
ENV_FILE=$(mktemp)
echo "CLAUDE_SESSION_ID=$SID" > "$ENV_FILE"
if CLAUDE_PROJECT_DIR="$REPO" CLAUDE_ENV_FILE="$ENV_FILE" node "$MARK_STEP" --reset-from invalid_step_name 2>/dev/null; then
    fail "24. --reset-from invalid step → expected exit 1, got exit 0"
else
    pass "24. --reset-from invalid step → exit 1"
fi
rm -f "$ENV_FILE"

# ---------------------------------------------------------------------------
# === mark-step.js: Error cases ===
# ---------------------------------------------------------------------------

echo ""
echo "=== mark-step: Error cases ==="

# Test 25: Invalid step name → exit 1
REPO=$(setup_repo)
SID="sid-$RANDOM"
ENV_FILE=$(mktemp)
echo "CLAUDE_SESSION_ID=$SID" > "$ENV_FILE"
if CLAUDE_PROJECT_DIR="$REPO" CLAUDE_ENV_FILE="$ENV_FILE" node "$MARK_STEP" not_a_real_step complete 2>/dev/null; then
    fail "25. Invalid step name → expected exit 1, got exit 0"
else
    pass "25. Invalid step name → exit 1"
fi
rm -f "$ENV_FILE"

# Test 26: Invalid status value → exit 1
REPO=$(setup_repo)
SID="sid-$RANDOM"
ENV_FILE=$(mktemp)
echo "CLAUDE_SESSION_ID=$SID" > "$ENV_FILE"
if CLAUDE_PROJECT_DIR="$REPO" CLAUDE_ENV_FILE="$ENV_FILE" node "$MARK_STEP" research done 2>/dev/null; then
    fail "26. Invalid status 'done' → expected exit 1, got exit 0"
else
    pass "26. Invalid status 'done' → exit 1"
fi
rm -f "$ENV_FILE"

# Test 27: user_verification skipped → exit 1
REPO=$(setup_repo)
SID="sid-$RANDOM"
ENV_FILE=$(mktemp)
echo "CLAUDE_SESSION_ID=$SID" > "$ENV_FILE"
if CLAUDE_PROJECT_DIR="$REPO" CLAUDE_ENV_FILE="$ENV_FILE" node "$MARK_STEP" user_verification skipped 2>/dev/null; then
    fail "27. user_verification skipped → expected exit 1, got exit 0"
else
    pass "27. user_verification skipped → exit 1"
fi
rm -f "$ENV_FILE"

# Test 28: Missing arguments (only 1 arg: step name, no status) → exit 1
REPO=$(setup_repo)
SID="sid-$RANDOM"
ENV_FILE=$(mktemp)
echo "CLAUDE_SESSION_ID=$SID" > "$ENV_FILE"
if CLAUDE_PROJECT_DIR="$REPO" CLAUDE_ENV_FILE="$ENV_FILE" node "$MARK_STEP" research 2>/dev/null; then
    fail "28. Missing arguments (1 arg) → expected exit 1, got exit 0"
else
    pass "28. Missing arguments → exit 1"
fi
rm -f "$ENV_FILE"

# ---------------------------------------------------------------------------
# === mark-step.js: Idempotency ===
# ---------------------------------------------------------------------------

echo ""
echo "=== mark-step: Idempotency ==="

# Test 29: Mark same step complete twice → exit 0, still complete
REPO=$(setup_repo)
SID="sid-$RANDOM"
ENV_FILE=$(mktemp)
echo "CLAUDE_SESSION_ID=$SID" > "$ENV_FILE"
CLAUDE_PROJECT_DIR="$REPO" CLAUDE_ENV_FILE="$ENV_FILE" node "$MARK_STEP" research complete 2>/dev/null || true
if CLAUDE_PROJECT_DIR="$REPO" CLAUDE_ENV_FILE="$ENV_FILE" node "$MARK_STEP" research complete 2>/dev/null; then
    STATE_FILE=$(get_state_file "$REPO" "$SID")
    STATUS=$(node -e "let b='';process.stdin.on('data',c=>b+=c);process.stdin.on('end',()=>console.log(JSON.parse(b).steps.research.status))" < "$STATE_FILE" 2>/dev/null || echo "error")
    if [ "$STATUS" = "complete" ]; then pass "29. Mark same step complete twice → idempotent"
    else fail "29. Idempotent mark → status='$STATUS', expected 'complete'"; fi
else
    fail "29. Mark same step complete twice → second call exit non-zero"
fi
rm -f "$ENV_FILE"

# ---------------------------------------------------------------------------
# === mark-step.js: Session ID auto-detection ===
# ---------------------------------------------------------------------------

echo ""
echo "=== mark-step: Session ID auto-detection ==="

# Test 30: CLAUDE_ENV_FILE set with valid CLAUDE_SESSION_ID → step marked correctly
REPO=$(setup_repo)
SID="sid-autodetect-$RANDOM"
ENV_FILE=$(mktemp)
echo "CLAUDE_SESSION_ID=$SID" > "$ENV_FILE"
if CLAUDE_PROJECT_DIR="$REPO" CLAUDE_ENV_FILE="$ENV_FILE" node "$MARK_STEP" research complete 2>/dev/null; then
    STATE_FILE=$(get_state_file "$REPO" "$SID")
    if [ -f "$STATE_FILE" ]; then
        STATUS=$(node -e "let b='';process.stdin.on('data',c=>b+=c);process.stdin.on('end',()=>console.log(JSON.parse(b).steps.research.status))" < "$STATE_FILE" 2>/dev/null || echo "error")
        if [ "$STATUS" = "complete" ]; then pass "30. CLAUDE_ENV_FILE with valid CLAUDE_SESSION_ID → step marked correctly"
        else fail "30. CLAUDE_ENV_FILE → status='$STATUS', expected 'complete'"; fi
    else
        fail "30. CLAUDE_ENV_FILE → state file not found at $STATE_FILE"
    fi
else
    fail "30. CLAUDE_ENV_FILE → exit non-zero"
fi
rm -f "$ENV_FILE"

# Test 31: CLAUDE_ENV_FILE set but contains no CLAUDE_SESSION_ID line → exit 1
REPO=$(setup_repo)
ENV_FILE=$(mktemp)
echo "SOME_OTHER_VAR=value" > "$ENV_FILE"
if CLAUDE_PROJECT_DIR="$REPO" CLAUDE_ENV_FILE="$ENV_FILE" node "$MARK_STEP" research complete 2>/dev/null; then
    fail "31. CLAUDE_ENV_FILE without CLAUDE_SESSION_ID → expected exit 1, got exit 0"
else
    pass "31. CLAUDE_ENV_FILE without CLAUDE_SESSION_ID → exit 1"
fi
rm -f "$ENV_FILE"

# Test 32: CLAUDE_ENV_FILE not set (env var absent) → exit 1
REPO=$(setup_repo)
if CLAUDE_PROJECT_DIR="$REPO" node "$MARK_STEP" research complete 2>/dev/null; then
    fail "32. CLAUDE_ENV_FILE not set → expected exit 1, got exit 0"
else
    pass "32. CLAUDE_ENV_FILE not set → exit 1"
fi

# Test 33: CLAUDE_ENV_FILE points to non-existent file → exit 1
REPO=$(setup_repo)
NONEXISTENT_FILE="$TMPDIR_BASE/does-not-exist-$RANDOM.env"
if CLAUDE_PROJECT_DIR="$REPO" CLAUDE_ENV_FILE="$NONEXISTENT_FILE" node "$MARK_STEP" research complete 2>/dev/null; then
    fail "33. CLAUDE_ENV_FILE non-existent file → expected exit 1, got exit 0"
else
    pass "33. CLAUDE_ENV_FILE non-existent file → exit 1"
fi

# Test 34: CLAUDE_ENV_FILE is set to empty file (no CLAUDE_SESSION_ID line) → exit 1
REPO=$(setup_repo)
ENV_FILE=$(mktemp)
# empty file — no content written
if CLAUDE_PROJECT_DIR="$REPO" CLAUDE_ENV_FILE="$ENV_FILE" node "$MARK_STEP" research complete 2>/dev/null; then
    fail "34. CLAUDE_ENV_FILE empty file → expected exit 1, got exit 0"
else
    pass "34. CLAUDE_ENV_FILE empty file → exit 1"
fi
rm -f "$ENV_FILE"

# ---------------------------------------------------------------------------
# session-start.js helpers
# ---------------------------------------------------------------------------

run_session_start() {
    local json="$1"
    shift
    echo "$json" | "$@" node "$SESSION_START" 2>/dev/null
}

# ---------------------------------------------------------------------------
# === session-start.js: Normal cases ===
# ---------------------------------------------------------------------------

echo ""
echo "=== session-start: Normal cases ==="

# Test 35: With CLAUDE_ENV_FILE set → file contains CLAUDE_SESSION_ID=abc123
REPO=$(setup_repo)
ENV_FILE="$TMPDIR_BASE/claude-env-$RANDOM.txt"
touch "$ENV_FILE"
echo '{"session_id":"abc123"}' | CLAUDE_PROJECT_DIR="$REPO" CLAUDE_ENV_FILE="$ENV_FILE" node "$SESSION_START" 2>/dev/null || true
if grep -qx "CLAUDE_SESSION_ID=abc123" "$ENV_FILE" 2>/dev/null; then
    pass "35. CLAUDE_ENV_FILE → file contains KEY=VALUE line (no export prefix)"
else
    fail "35. CLAUDE_ENV_FILE → expected exact line 'CLAUDE_SESSION_ID=abc123', file content: $(cat "$ENV_FILE" 2>/dev/null || echo '(not found)')"
fi

# Test 36: stdout output is {}
REPO=$(setup_repo)
STDOUT=$(echo '{"session_id":"abc123"}' | CLAUDE_PROJECT_DIR="$REPO" node "$SESSION_START" 2>/dev/null || true)
if echo "$STDOUT" | grep -q "{}"; then
    pass "36. session-start stdout is {}"
else
    fail "36. session-start stdout → expected '{}', got: '$STDOUT'"
fi

# Test 37: Zombie cleanup — state file with all updated_at 8 days ago → deleted
REPO=$(setup_repo)
SID_ZOMBIE="zombie-$RANDOM"
gitdir_zombie=$(git -C "$REPO" rev-parse --git-dir)
mkdir -p "$REPO/$gitdir_zombie/workflow"
ZOMBIE_FILE="$REPO/$gitdir_zombie/workflow/${SID_ZOMBIE}.json"
# updated_at values set to 8 days ago in JSON content — cleanup checks JSON timestamps
EIGHT_DAYS_AGO=$(node -e "console.log(new Date(Date.now()-8*24*60*60*1000).toISOString())" 2>/dev/null || echo "2026-04-03T10:00:00.000Z")
cat > "$ZOMBIE_FILE" <<EOF
{
  "version": 1,
  "session_id": "$SID_ZOMBIE",
  "created_at": "$EIGHT_DAYS_AGO",
  "steps": {
    "research":          {"status": "complete", "updated_at": "$EIGHT_DAYS_AGO"},
    "plan":              {"status": "complete", "updated_at": "$EIGHT_DAYS_AGO"},
    "write_tests":       {"status": "complete", "updated_at": "$EIGHT_DAYS_AGO"},
    "code":              {"status": "complete", "updated_at": "$EIGHT_DAYS_AGO"},
    "verify":            {"status": "complete", "updated_at": "$EIGHT_DAYS_AGO"},
    "docs":              {"status": "complete", "updated_at": "$EIGHT_DAYS_AGO"},
    "user_verification": {"status": "complete", "updated_at": "$EIGHT_DAYS_AGO"}
  }
}
EOF
echo '{"session_id":"new-session"}' | CLAUDE_PROJECT_DIR="$REPO" node "$SESSION_START" 2>/dev/null || true
if [ ! -f "$ZOMBIE_FILE" ]; then
    pass "37. Zombie cleanup: 8-day-old state file deleted"
else
    fail "37. Zombie cleanup: state file still exists: $ZOMBIE_FILE"
fi

# Test 38: State file with updated_at 3 days ago → NOT deleted
REPO=$(setup_repo)
SID_RECENT="recent-$RANDOM"
gitdir_recent=$(git -C "$REPO" rev-parse --git-dir)
mkdir -p "$REPO/$gitdir_recent/workflow"
RECENT_FILE="$REPO/$gitdir_recent/workflow/${SID_RECENT}.json"
THREE_DAYS_AGO=$(node -e "console.log(new Date(Date.now()-3*24*60*60*1000).toISOString())" 2>/dev/null || echo "2026-04-08T10:00:00.000Z")
cat > "$RECENT_FILE" <<EOF
{
  "version": 1,
  "session_id": "$SID_RECENT",
  "created_at": "$THREE_DAYS_AGO",
  "steps": {
    "research":          {"status": "complete", "updated_at": "$THREE_DAYS_AGO"},
    "plan":              {"status": "complete", "updated_at": "$THREE_DAYS_AGO"},
    "write_tests":       {"status": "complete", "updated_at": "$THREE_DAYS_AGO"},
    "code":              {"status": "complete", "updated_at": "$THREE_DAYS_AGO"},
    "verify":            {"status": "complete", "updated_at": "$THREE_DAYS_AGO"},
    "docs":              {"status": "complete", "updated_at": "$THREE_DAYS_AGO"},
    "user_verification": {"status": "complete", "updated_at": "$THREE_DAYS_AGO"}
  }
}
EOF
echo '{"session_id":"new-session"}' | CLAUDE_PROJECT_DIR="$REPO" node "$SESSION_START" 2>/dev/null || true
if [ -f "$RECENT_FILE" ]; then
    pass "38. Recent state file (3 days) NOT deleted by zombie cleanup"
else
    fail "38. Recent state file was incorrectly deleted"
fi

# ---------------------------------------------------------------------------
# === session-start.js: Edge cases ===
# ---------------------------------------------------------------------------

echo ""
echo "=== session-start: Edge cases ==="

# Test 39: CLAUDE_ENV_FILE not set → exits 0, no error
REPO=$(setup_repo)
if echo '{"session_id":"abc123"}' | CLAUDE_PROJECT_DIR="$REPO" node "$SESSION_START" 2>/dev/null; then
    pass "39. CLAUDE_ENV_FILE not set → exits 0"
else
    fail "39. CLAUDE_ENV_FILE not set → expected exit 0, got non-zero"
fi

# Test 40: .git/workflow/ directory doesn't exist → cleanup runs without error
REPO=$(setup_repo)
# Do NOT create the workflow directory — verify no crash
if echo '{"session_id":"abc123"}' | CLAUDE_PROJECT_DIR="$REPO" node "$SESSION_START" 2>/dev/null; then
    pass "40. Missing workflow dir → cleanup runs without error"
else
    fail "40. Missing workflow dir → session-start crashed (exit non-zero)"
fi

# Test 41: stdin is invalid JSON → exits 0 (fail-open for SessionStart)
REPO=$(setup_repo)
if echo 'NOT VALID JSON' | CLAUDE_PROJECT_DIR="$REPO" node "$SESSION_START" 2>/dev/null; then
    pass "41. Invalid JSON stdin → exits 0 (fail-open)"
else
    fail "41. Invalid JSON stdin → expected exit 0 (fail-open), got non-zero"
fi

# ---------------------------------------------------------------------------
# === is-private-repo.js: toNativePath / resolveRepoDir ===
# ---------------------------------------------------------------------------

echo ""
echo "=== is-private-repo: toNativePath / resolveRepoDir ==="

# Test 42: toNativePath converts /c/... to C:/... on win32
RESULT=$(cd "$DOTFILES_DIR" && node -e "
  Object.defineProperty(process,'platform',{value:'win32',configurable:true});
  const {toNativePath}=require('./claude-global/hooks/lib/is-private-repo.js');
  console.log(toNativePath('/c/git/dotfiles'));
" 2>/dev/null)
[ "$RESULT" = "C:/git/dotfiles" ] && pass "42. toNativePath /c/git/dotfiles → C:/git/dotfiles" || fail "42. toNativePath: expected C:/git/dotfiles, got '$RESULT'"

# Test 43: toNativePath handles different drive letters
RESULT=$(cd "$DOTFILES_DIR" && node -e "
  Object.defineProperty(process,'platform',{value:'win32',configurable:true});
  const {toNativePath}=require('./claude-global/hooks/lib/is-private-repo.js');
  console.log(toNativePath('/d/foo/bar'));
" 2>/dev/null)
[ "$RESULT" = "D:/foo/bar" ] && pass "43. toNativePath /d/foo/bar → D:/foo/bar" || fail "43. toNativePath: expected D:/foo/bar, got '$RESULT'"

# Test 44: toNativePath leaves already-Windows paths unchanged
RESULT=$(cd "$DOTFILES_DIR" && node -e "
  Object.defineProperty(process,'platform',{value:'win32',configurable:true});
  const {toNativePath}=require('./claude-global/hooks/lib/is-private-repo.js');
  console.log(toNativePath('C:/git/dotfiles'));
" 2>/dev/null)
[ "$RESULT" = "C:/git/dotfiles" ] && pass "44. toNativePath C:/git/dotfiles → C:/git/dotfiles (no change)" || fail "44. toNativePath: expected C:/git/dotfiles, got '$RESULT'"

# Test 45: toNativePath leaves relative paths unchanged
RESULT=$(cd "$DOTFILES_DIR" && node -e "
  Object.defineProperty(process,'platform',{value:'win32',configurable:true});
  const {toNativePath}=require('./claude-global/hooks/lib/is-private-repo.js');
  console.log(toNativePath('.'));
" 2>/dev/null)
[ "$RESULT" = "." ] && pass "45. toNativePath '.' → '.' (no change)" || fail "45. toNativePath: expected '.', got '$RESULT'"

# Test 46: toNativePath leaves Linux-style non-drive paths unchanged on win32
RESULT=$(cd "$DOTFILES_DIR" && node -e "
  Object.defineProperty(process,'platform',{value:'win32',configurable:true});
  const {toNativePath}=require('./claude-global/hooks/lib/is-private-repo.js');
  console.log(toNativePath('/usr/local/bin'));
" 2>/dev/null)
[ "$RESULT" = "/usr/local/bin" ] && pass "46. toNativePath /usr/local/bin → /usr/local/bin (no change)" || fail "46. toNativePath: expected /usr/local/bin, got '$RESULT'"

# Test 47: toNativePath handles single-char drive with trailing slash
RESULT=$(cd "$DOTFILES_DIR" && node -e "
  Object.defineProperty(process,'platform',{value:'win32',configurable:true});
  const {toNativePath}=require('./claude-global/hooks/lib/is-private-repo.js');
  console.log(toNativePath('/z/'));
" 2>/dev/null)
[ "$RESULT" = "Z:/" ] && pass "47. toNativePath /z/ → Z:/" || fail "47. toNativePath: expected Z:/, got '$RESULT'"

# Test 48: resolveRepoDir converts WSL path via toNativePath on win32 (CLAUDE_PROJECT_DIR unset)
RESULT=$(cd "$DOTFILES_DIR" && node -e "
  delete process.env.HOOK_CWD; delete process.env.CLAUDE_PROJECT_DIR;
  Object.defineProperty(process,'platform',{value:'win32',configurable:true});
  const {resolveRepoDir}=require('./claude-global/hooks/lib/is-private-repo.js');
  console.log(resolveRepoDir('git -C /c/git/dotfiles commit'));
" 2>/dev/null)
[ "$RESULT" = "C:/git/dotfiles" ] && pass "48. resolveRepoDir: WSL path /c/git/dotfiles → C:/git/dotfiles on win32" || fail "48. resolveRepoDir: expected C:/git/dotfiles, got '$RESULT'"

# Test 49: resolveRepoDir returns "." when no -C flag and CLAUDE_PROJECT_DIR unset
RESULT=$(cd "$DOTFILES_DIR" && node -e "
  delete process.env.HOOK_CWD; delete process.env.CLAUDE_PROJECT_DIR;
  Object.defineProperty(process,'platform',{value:'win32',configurable:true});
  const {resolveRepoDir}=require('./claude-global/hooks/lib/is-private-repo.js');
  console.log(resolveRepoDir('git commit'));
" 2>/dev/null)
[ "$RESULT" = "." ] && pass "49. resolveRepoDir: no -C flag → '.'" || fail "49. resolveRepoDir: expected '.', got '$RESULT'"

# ---------------------------------------------------------------------------
# === workflow-mark: New hook ===
# ---------------------------------------------------------------------------
# TDD: workflow-mark.js does NOT exist yet. These tests are expected to FAIL
# with a "Cannot find module" / MODULE_NOT_FOUND style error until the hook is
# implemented. Each test asserts state changes (or lack thereof) via the state
# file rather than hook stdout so the failure mode is always a node error from
# the missing hook, surfaced as state==MISSING on our side.

echo ""
echo "=== workflow-mark: New hook — Normal cases ==="

# Helper: build a PostToolUse-style hook input JSON with a Bash command and
# tool_response.exit_code=0. Escape embedded double quotes in $cmd.
build_mark_json() {
    local cmd="$1" sid="${2:-test-session}"
    # Escape backslashes and double quotes in $cmd for JSON
    local esc=${cmd//\\/\\\\}
    esc=${esc//\"/\\\"}
    printf '{"tool_name":"Bash","tool_input":{"command":"%s"},"tool_response":{"exit_code":0,"stdout":"%s\\n","stderr":""},"session_id":"%s"}' "$esc" "$esc" "$sid"
}

# Test N1: echo "<<WORKFLOW_MARK_STEP:research:complete>>" (double-quoted)
REPO=$(setup_repo)
write_state "$REPO" "test-session" "$(ALL_PENDING_JSON test-session)"
N1_JSON=$(build_mark_json 'echo "<<WORKFLOW_MARK_STEP_research_complete>>"')
run_mark_hook "$REPO" "$N1_JSON" >/dev/null
expect_state_step "N1. echo \"<<...>>\" (double-quoted) → research=complete" "$REPO" "test-session" "research" "complete"

# Test N2: echo '<<WORKFLOW_MARK_STEP_research_complete>>' (single-quoted)
REPO=$(setup_repo)
write_state "$REPO" "test-session" "$(ALL_PENDING_JSON test-session)"
N2_JSON=$(build_mark_json "echo '<<WORKFLOW_MARK_STEP_research_complete>>'")
run_mark_hook "$REPO" "$N2_JSON" >/dev/null
expect_state_step "N2. echo '<<...>>' (single-quoted) → research=complete" "$REPO" "test-session" "research" "complete"

# Test N3: status skipped on research → recorded
REPO=$(setup_repo)
write_state "$REPO" "test-session" "$(ALL_PENDING_JSON test-session)"
N3_JSON=$(build_mark_json 'echo "<<WORKFLOW_MARK_STEP_research_skipped>>"')
run_mark_hook "$REPO" "$N3_JSON" >/dev/null
expect_state_step "N3. status=skipped on research → recorded" "$REPO" "test-session" "research" "skipped"

# Test N4: status in_progress on write_tests → recorded
REPO=$(setup_repo)
write_state "$REPO" "test-session" "$(ALL_PENDING_JSON test-session)"
N4_JSON=$(build_mark_json 'echo "<<WORKFLOW_MARK_STEP_write_tests_in_progress>>"')
run_mark_hook "$REPO" "$N4_JSON" >/dev/null
expect_state_step "N4. status=in_progress on write_tests → recorded" "$REPO" "test-session" "write_tests" "in_progress"

echo ""
echo "=== workflow-mark: New hook — Must-NOT-mark cases ==="

# Test F1: cat SKILL.md (marker in file contents, not a literal echo command)
REPO=$(setup_repo)
write_state "$REPO" "test-session" "$(ALL_PENDING_JSON test-session)"
F1_JSON=$(build_mark_json 'cat SKILL.md')
run_mark_hook "$REPO" "$F1_JSON" >/dev/null
expect_no_state_change "F1. cat SKILL.md with marker in stdout → unchanged" "$REPO" "test-session" "research" "pending"

# Test F2: git diff showing marker
REPO=$(setup_repo)
write_state "$REPO" "test-session" "$(ALL_PENDING_JSON test-session)"
F2_JSON=$(build_mark_json 'git diff')
run_mark_hook "$REPO" "$F2_JSON" >/dev/null
expect_no_state_change "F2. git diff showing marker → unchanged" "$REPO" "test-session" "research" "pending"

# Test F3: grep WORKFLOW_MARK_STEP file.sh
REPO=$(setup_repo)
write_state "$REPO" "test-session" "$(ALL_PENDING_JSON test-session)"
F3_JSON=$(build_mark_json 'grep WORKFLOW_MARK_STEP file.sh')
run_mark_hook "$REPO" "$F3_JSON" >/dev/null
expect_no_state_change "F3. grep WORKFLOW_MARK_STEP → unchanged" "$REPO" "test-session" "research" "pending"

# Test F4: echo piped to tee
REPO=$(setup_repo)
write_state "$REPO" "test-session" "$(ALL_PENDING_JSON test-session)"
F4_JSON=$(build_mark_json 'echo "<<WORKFLOW_MARK_STEP_research_complete>>" | tee /tmp/log')
run_mark_hook "$REPO" "$F4_JSON" >/dev/null
expect_no_state_change "F4. echo \"<<...>>\" | tee /tmp/log → unchanged" "$REPO" "test-session" "research" "pending"

# Test F5: cd /tmp && echo "<<...>>" (prefix chaining)
REPO=$(setup_repo)
write_state "$REPO" "test-session" "$(ALL_PENDING_JSON test-session)"
F5_JSON=$(build_mark_json 'cd /tmp && echo "<<WORKFLOW_MARK_STEP_research_complete>>"')
run_mark_hook "$REPO" "$F5_JSON" >/dev/null
expect_no_state_change "F5. cd /tmp && echo \"<<...>>\" (prefix chain) → unchanged" "$REPO" "test-session" "research" "pending"

# Test F6: echo "<<...>>" ; rm foo (trailing chain)
REPO=$(setup_repo)
write_state "$REPO" "test-session" "$(ALL_PENDING_JSON test-session)"
F6_JSON=$(build_mark_json 'echo "<<WORKFLOW_MARK_STEP_research_complete>>" ; rm foo')
run_mark_hook "$REPO" "$F6_JSON" >/dev/null
expect_no_state_change "F6. echo \"<<...>>\" ; rm foo (trailing chain) → unchanged" "$REPO" "test-session" "research" "pending"

# Test F7: echo " <<...>> " (inner spaces around marker inside quotes)
REPO=$(setup_repo)
write_state "$REPO" "test-session" "$(ALL_PENDING_JSON test-session)"
F7_JSON=$(build_mark_json 'echo " <<WORKFLOW_MARK_STEP_research_complete>> "')
run_mark_hook "$REPO" "$F7_JSON" >/dev/null
expect_no_state_change "F7. echo \" <<...>> \" (inner spaces) → unchanged" "$REPO" "test-session" "research" "pending"

# Test F8: 10KB padded command containing 'echo' as a substring but not as the command
REPO=$(setup_repo)
write_state "$REPO" "test-session" "$(ALL_PENDING_JSON test-session)"
F8_PAD=$(printf 'x%.0s' {1..10000})
F8_JSON=$(build_mark_json "node run.js --msg echoes-${F8_PAD}-end")
run_mark_hook "$REPO" "$F8_JSON" >/dev/null
expect_no_state_change "F8. 10KB padded command with 'echo' as substring → unchanged" "$REPO" "test-session" "research" "pending"

# Test F9: printf instead of echo
REPO=$(setup_repo)
write_state "$REPO" "test-session" "$(ALL_PENDING_JSON test-session)"
F9_JSON=$(build_mark_json 'printf "<<WORKFLOW_MARK_STEP_research_complete>>"')
run_mark_hook "$REPO" "$F9_JSON" >/dev/null
expect_no_state_change "F9. printf \"<<...>>\" (not echo) → unchanged" "$REPO" "test-session" "research" "pending"

echo ""
echo "=== workflow-mark: New hook — Error / edge cases ==="

# Test E1: unknown step "foo" → state unchanged, hook exit 0
REPO=$(setup_repo)
write_state "$REPO" "test-session" "$(ALL_PENDING_JSON test-session)"
E1_JSON=$(build_mark_json 'echo "<<WORKFLOW_MARK_STEP_foo_complete>>"')
run_mark_hook "$REPO" "$E1_JSON" >/dev/null
expect_no_state_change "E1. unknown step 'foo' → research unchanged" "$REPO" "test-session" "research" "pending"

# Test E2: unknown status "done" → state unchanged
REPO=$(setup_repo)
write_state "$REPO" "test-session" "$(ALL_PENDING_JSON test-session)"
E2_JSON=$(build_mark_json 'echo "<<WORKFLOW_MARK_STEP_research_done>>"')
run_mark_hook "$REPO" "$E2_JSON" >/dev/null
expect_no_state_change "E2. unknown status 'done' → research unchanged" "$REPO" "test-session" "research" "pending"

# Test E3: user_verification_complete via marker → REJECTED
REPO=$(setup_repo)
write_state "$REPO" "test-session" "$(ALL_PENDING_JSON test-session)"
E3_JSON=$(build_mark_json 'echo "<<WORKFLOW_MARK_STEP_user_verification_complete>>"')
run_mark_hook "$REPO" "$E3_JSON" >/dev/null
expect_no_state_change "E3. user_verification_complete via marker → REJECTED" "$REPO" "test-session" "user_verification" "pending"

# Test E4: user_verification_skipped via marker → REJECTED
REPO=$(setup_repo)
write_state "$REPO" "test-session" "$(ALL_PENDING_JSON test-session)"
E4_JSON=$(build_mark_json 'echo "<<WORKFLOW_MARK_STEP_user_verification_skipped>>"')
run_mark_hook "$REPO" "$E4_JSON" >/dev/null
expect_no_state_change "E4. user_verification_skipped via marker → REJECTED" "$REPO" "test-session" "user_verification" "pending"

# Test E5: session_id not in stdin AND CLAUDE_ENV_FILE unset →
#   state unchanged, hook stdout JSON contains "systemMessage", exit 0
REPO=$(setup_repo)
write_state "$REPO" "test-session" "$(ALL_PENDING_JSON test-session)"
E5_CMD='echo "<<WORKFLOW_MARK_STEP_research_complete>>"'
E5_ESC=${E5_CMD//\"/\\\"}
E5_JSON=$(printf '{"tool_name":"Bash","tool_input":{"command":"%s"},"tool_response":{"exit_code":0,"stdout":"%s\\n","stderr":""}}' "$E5_ESC" "$E5_ESC")
E5_OUT=$(echo "$E5_JSON" | CLAUDE_PROJECT_DIR="$REPO" env -u CLAUDE_ENV_FILE node "$MARK_HOOK" 2>/dev/null || true)
E5_EXIT=$?
expect_no_state_change "E5a. no session_id → research unchanged" "$REPO" "test-session" "research" "pending"
if echo "$E5_OUT" | grep -q "additionalContext"; then
    pass "E5b. no session_id → stdout JSON contains additionalContext"
else
    fail "E5b. no session_id → expected additionalContext in stdout, got: $E5_OUT"
fi
if [ "$E5_EXIT" = "0" ]; then
    pass "E5c. no session_id → hook exit 0"
else
    fail "E5c. no session_id → expected exit 0, got: $E5_EXIT"
fi

# Test E6: private repo test deferred — hard to fake without network.
# TODO: Add test once we can stub is-private-repo.js or use a fixture repo
# with a known-private remote URL. For now, skip.

# Test E7: tool_response.exit_code=1 → state unchanged (echo supposedly failed)
REPO=$(setup_repo)
write_state "$REPO" "test-session" "$(ALL_PENDING_JSON test-session)"
E7_CMD='echo "<<WORKFLOW_MARK_STEP_research_complete>>"'
E7_ESC=${E7_CMD//\"/\\\"}
E7_JSON=$(printf '{"tool_name":"Bash","tool_input":{"command":"%s"},"tool_response":{"exit_code":1,"stdout":"","stderr":"oops"},"session_id":"test-session"}' "$E7_ESC")
run_mark_hook "$REPO" "$E7_JSON" >/dev/null
expect_no_state_change "E7. tool_response.exit_code=1 → unchanged" "$REPO" "test-session" "research" "pending"

# Test E8: tool_name != Bash (e.g. Write) → ignored, state unchanged
REPO=$(setup_repo)
write_state "$REPO" "test-session" "$(ALL_PENDING_JSON test-session)"
E8_JSON='{"tool_name":"Write","tool_input":{"file_path":"/tmp/foo","content":"<<WORKFLOW_MARK_STEP_research_complete>>"},"tool_response":{"success":true},"session_id":"test-session"}'
run_mark_hook "$REPO" "$E8_JSON" >/dev/null
expect_no_state_change "E8. tool_name=Write → unchanged" "$REPO" "test-session" "research" "pending"

echo ""
echo "=== workflow-mark: New hook — Idempotency ==="

# Test I1: same marker applied twice → state valid, status=complete
REPO=$(setup_repo)
write_state "$REPO" "test-session" "$(ALL_PENDING_JSON test-session)"
I1_JSON=$(build_mark_json 'echo "<<WORKFLOW_MARK_STEP_research_complete>>"')
run_mark_hook "$REPO" "$I1_JSON" >/dev/null
run_mark_hook "$REPO" "$I1_JSON" >/dev/null
expect_state_step "I1. same marker applied twice → research=complete (idempotent)" "$REPO" "test-session" "research" "complete"

# Test I2: concurrent write race — deferred. Platform-dependent and requires
# deterministic interleaving; skip until we have a fixture for file-lock testing.

# ---------------------------------------------------------------------------
# === workflow-mark: RESET_FROM marker ===
# ---------------------------------------------------------------------------

echo ""
echo "=== workflow-mark: RESET_FROM marker — Normal cases ==="

# Helper: build a hook input JSON for WORKFLOW_RESET_FROM commands.
# Like build_mark_json but takes the raw command string (no escaping needed
# for the reset commands which use only alphanumeric step names).
build_reset_json() {
    local cmd="$1" sid="${2:-test-session}"
    local esc=${cmd//\\/\\\\}
    esc=${esc//\"/\\\"}
    printf '{"tool_name":"Bash","tool_input":{"command":"%s"},"tool_response":{"exit_code":0,"stdout":"%s\\n","stderr":""},"session_id":"%s"}' "$esc" "$esc" "$sid"
}

# Test R1: RESET_FROM_write_tests on ALL_COMPLETE → research=complete, plan=complete,
#          write_tests=pending, code=pending, verify=pending, docs=pending,
#          user_verification=pending
REPO=$(setup_repo)
write_state "$REPO" "test-session" "$(ALL_COMPLETE_JSON test-session)"
R1_JSON=$(build_reset_json 'echo "<<WORKFLOW_RESET_FROM_write_tests>>"')
run_mark_hook "$REPO" "$R1_JSON" >/dev/null
expect_state_step "R1a. RESET_FROM:write_tests → research=complete" "$REPO" "test-session" "research" "complete"
expect_state_step "R1b. RESET_FROM:write_tests → plan=complete"     "$REPO" "test-session" "plan"     "complete"
expect_state_step "R1c. RESET_FROM:write_tests → write_tests=pending" "$REPO" "test-session" "write_tests" "pending"
expect_state_step "R1d. RESET_FROM:write_tests → code=pending"      "$REPO" "test-session" "code"     "pending"
expect_state_step "R1e. RESET_FROM:write_tests → verify=pending"    "$REPO" "test-session" "verify"   "pending"
expect_state_step "R1f. RESET_FROM:write_tests → docs=pending"      "$REPO" "test-session" "docs"     "pending"
expect_state_step "R1g. RESET_FROM:write_tests → user_verification=pending" "$REPO" "test-session" "user_verification" "pending"

# Test R2: RESET_FROM_research → all steps pending (nothing before research)
REPO=$(setup_repo)
write_state "$REPO" "test-session" "$(ALL_COMPLETE_JSON test-session)"
R2_JSON=$(build_reset_json 'echo "<<WORKFLOW_RESET_FROM_research>>"')
run_mark_hook "$REPO" "$R2_JSON" >/dev/null
expect_state_step "R2a. RESET_FROM:research → research=pending"          "$REPO" "test-session" "research"          "pending"
expect_state_step "R2b. RESET_FROM:research → plan=pending"              "$REPO" "test-session" "plan"              "pending"
expect_state_step "R2c. RESET_FROM:research → write_tests=pending"       "$REPO" "test-session" "write_tests"       "pending"
expect_state_step "R2d. RESET_FROM:research → code=pending"              "$REPO" "test-session" "code"              "pending"
expect_state_step "R2e. RESET_FROM:research → verify=pending"            "$REPO" "test-session" "verify"            "pending"
expect_state_step "R2f. RESET_FROM:research → docs=pending"              "$REPO" "test-session" "docs"              "pending"
expect_state_step "R2g. RESET_FROM:research → user_verification=pending" "$REPO" "test-session" "user_verification" "pending"

# Test R3: RESET_FROM_user_verification → all steps before it complete, user_verification=pending
REPO=$(setup_repo)
write_state "$REPO" "test-session" "$(ALL_PENDING_JSON test-session)"
R3_JSON=$(build_reset_json 'echo "<<WORKFLOW_RESET_FROM_user_verification>>"')
run_mark_hook "$REPO" "$R3_JSON" >/dev/null
expect_state_step "R3a. RESET_FROM:user_verification → research=complete"    "$REPO" "test-session" "research"          "complete"
expect_state_step "R3b. RESET_FROM:user_verification → plan=complete"        "$REPO" "test-session" "plan"              "complete"
expect_state_step "R3c. RESET_FROM:user_verification → write_tests=complete" "$REPO" "test-session" "write_tests"       "complete"
expect_state_step "R3d. RESET_FROM:user_verification → code=complete"        "$REPO" "test-session" "code"              "complete"
expect_state_step "R3e. RESET_FROM:user_verification → verify=complete"      "$REPO" "test-session" "verify"            "complete"
expect_state_step "R3f. RESET_FROM:user_verification → docs=complete"        "$REPO" "test-session" "docs"              "complete"
expect_state_step "R3g. RESET_FROM:user_verification → user_verification=pending" "$REPO" "test-session" "user_verification" "pending"

# Test R4: single-quote variant → NOT processed (SQ RESET_FROM removed from source)
REPO=$(setup_repo)
write_state "$REPO" "test-session" "$(ALL_COMPLETE_JSON test-session)"
R4_JSON=$(build_reset_json "echo '<<WORKFLOW_RESET_FROM_write_tests>>'")
run_mark_hook "$REPO" "$R4_JSON" >/dev/null
expect_no_state_change "R4a. single-quote RESET_FROM → research unchanged (complete)"   "$REPO" "test-session" "research"   "complete"
expect_no_state_change "R4b. single-quote RESET_FROM → plan unchanged (complete)"       "$REPO" "test-session" "plan"       "complete"
expect_no_state_change "R4c. single-quote RESET_FROM → write_tests unchanged (complete)" "$REPO" "test-session" "write_tests" "complete"
expect_no_state_change "R4d. single-quote RESET_FROM → code unchanged (complete)"       "$REPO" "test-session" "code"       "complete"

echo ""
echo "=== workflow-mark: RESET_FROM marker — Must-NOT-match cases ==="

# Test RF1: echo "<<WORKFLOW_RESET_FROM_write_tests>>" | tee /tmp/log → state unchanged
REPO=$(setup_repo)
write_state "$REPO" "test-session" "$(ALL_COMPLETE_JSON test-session)"
RF1_JSON=$(build_reset_json 'echo "<<WORKFLOW_RESET_FROM_write_tests>>" | tee /tmp/log')
run_mark_hook "$REPO" "$RF1_JSON" >/dev/null
expect_no_state_change "RF1. echo \"<<...>>\" | tee /tmp/log → write_tests unchanged (complete)" "$REPO" "test-session" "write_tests" "complete"

# Test RF2: cd /tmp && echo "<<WORKFLOW_RESET_FROM_write_tests>>" → state unchanged
REPO=$(setup_repo)
write_state "$REPO" "test-session" "$(ALL_COMPLETE_JSON test-session)"
RF2_JSON=$(build_reset_json 'cd /tmp && echo "<<WORKFLOW_RESET_FROM_write_tests>>"')
run_mark_hook "$REPO" "$RF2_JSON" >/dev/null
expect_no_state_change "RF2. cd && echo \"<<...>>\" (prefix chain) → write_tests unchanged (complete)" "$REPO" "test-session" "write_tests" "complete"

# Test RF3: printf "<<WORKFLOW_RESET_FROM_write_tests>>" → state unchanged
REPO=$(setup_repo)
write_state "$REPO" "test-session" "$(ALL_COMPLETE_JSON test-session)"
RF3_JSON=$(build_reset_json 'printf "<<WORKFLOW_RESET_FROM_write_tests>>"')
run_mark_hook "$REPO" "$RF3_JSON" >/dev/null
expect_no_state_change "RF3. printf \"<<...>>\" (not echo) → write_tests unchanged (complete)" "$REPO" "test-session" "write_tests" "complete"

echo ""
echo "=== workflow-mark: RESET_FROM marker — Error / edge cases ==="

# Test RE1: unknown step "foo" → state unchanged, hook exit 0
REPO=$(setup_repo)
write_state "$REPO" "test-session" "$(ALL_COMPLETE_JSON test-session)"
RE1_JSON=$(build_reset_json 'echo "<<WORKFLOW_RESET_FROM_foo>>"')
run_mark_hook "$REPO" "$RE1_JSON" >/dev/null
expect_no_state_change "RE1. unknown step 'foo' → research unchanged (complete)" "$REPO" "test-session" "research" "complete"

# Test RE2: missing session_id → state unchanged, hook exit 0, stdout has additionalContext
REPO=$(setup_repo)
write_state "$REPO" "test-session" "$(ALL_COMPLETE_JSON test-session)"
RE2_CMD='echo "<<WORKFLOW_RESET_FROM_write_tests>>"'
RE2_ESC=${RE2_CMD//\"/\\\"}
RE2_JSON=$(printf '{"tool_name":"Bash","tool_input":{"command":"%s"},"tool_response":{"exit_code":0,"stdout":"%s\\n","stderr":""}}' "$RE2_ESC" "$RE2_ESC")
RE2_OUT=$(echo "$RE2_JSON" | CLAUDE_PROJECT_DIR="$REPO" env -u CLAUDE_ENV_FILE node "$MARK_HOOK" 2>/dev/null || true)
RE2_EXIT=$?
expect_no_state_change "RE2a. no session_id → write_tests unchanged (complete)" "$REPO" "test-session" "write_tests" "complete"
if echo "$RE2_OUT" | grep -q "additionalContext"; then
    pass "RE2b. no session_id → stdout JSON contains additionalContext"
else
    fail "RE2b. no session_id → expected additionalContext in stdout, got: $RE2_OUT"
fi
if [ "$RE2_EXIT" = "0" ]; then
    pass "RE2c. no session_id → hook exit 0"
else
    fail "RE2c. no session_id → expected exit 0, got: $RE2_EXIT"
fi

# Test RE3: exit_code=1 → state unchanged
REPO=$(setup_repo)
write_state "$REPO" "test-session" "$(ALL_COMPLETE_JSON test-session)"
RE3_CMD='echo "<<WORKFLOW_RESET_FROM_write_tests>>"'
RE3_ESC=${RE3_CMD//\"/\\\"}
RE3_JSON=$(printf '{"tool_name":"Bash","tool_input":{"command":"%s"},"tool_response":{"exit_code":1,"stdout":"","stderr":"oops"},"session_id":"test-session"}' "$RE3_ESC")
run_mark_hook "$REPO" "$RE3_JSON" >/dev/null
expect_no_state_change "RE3. exit_code=1 → write_tests unchanged (complete)" "$REPO" "test-session" "write_tests" "complete"

echo ""
echo "=== workflow-mark: RESET_FROM marker — Idempotency ==="

# Test RI1: apply R1 twice → same final state (no crash, write_tests still pending)
REPO=$(setup_repo)
write_state "$REPO" "test-session" "$(ALL_COMPLETE_JSON test-session)"
RI1_JSON=$(build_reset_json 'echo "<<WORKFLOW_RESET_FROM_write_tests>>"')
run_mark_hook "$REPO" "$RI1_JSON" >/dev/null
run_mark_hook "$REPO" "$RI1_JSON" >/dev/null
expect_state_step "RI1a. RESET_FROM applied twice → research=complete (idempotent)"   "$REPO" "test-session" "research"   "complete"
expect_state_step "RI1b. RESET_FROM applied twice → write_tests=pending (idempotent)" "$REPO" "test-session" "write_tests" "pending"
expect_state_step "RI1c. RESET_FROM applied twice → code=pending (idempotent)"        "$REPO" "test-session" "code"        "pending"

# ---------------------------------------------------------------------------
# settings.json — hook registration structure
# ---------------------------------------------------------------------------

echo ""
echo "=== settings.json: hook registration structure ==="

SETTINGS="$DOTFILES_DIR/claude-global/settings.json"

# SR1: hooks.PostToolUse exists
# Pass SETTINGS via argv (not embedded in -e string) so MSYS2 translates the path on Windows.
if node -e "const s=JSON.parse(require('fs').readFileSync(process.argv[1],'utf8')); process.exit(s.hooks && s.hooks.PostToolUse ? 0 : 1);" -- "$SETTINGS" 2>/dev/null; then
    pass "SR1. hooks.PostToolUse exists"
else
    fail "SR1. hooks.PostToolUse missing — workflow-mark.js will never be called"
fi

# SR2: hooks.PostToolUse[0].matcher === "Bash"
if node -e "const s=JSON.parse(require('fs').readFileSync(process.argv[1],'utf8')); const pt=s.hooks&&s.hooks.PostToolUse; process.exit(pt&&pt[0]&&pt[0].matcher==='Bash' ? 0 : 1);" -- "$SETTINGS" 2>/dev/null; then
    pass "SR2. hooks.PostToolUse[0].matcher === \"Bash\""
else
    fail "SR2. hooks.PostToolUse[0].matcher is not \"Bash\""
fi

# SR3: hooks.PostToolUse[0].hooks[0].command references workflow-mark.js
if node -e "const s=JSON.parse(require('fs').readFileSync(process.argv[1],'utf8')); const pt=s.hooks&&s.hooks.PostToolUse; const cmd=pt&&pt[0]&&pt[0].hooks&&pt[0].hooks[0]&&pt[0].hooks[0].command||''; process.exit(cmd.includes('workflow-mark.js') ? 0 : 1);" -- "$SETTINGS" 2>/dev/null; then
    pass "SR3. hooks.PostToolUse command references workflow-mark.js"
else
    fail "SR3. hooks.PostToolUse command does not reference workflow-mark.js"
fi

# SR4: permissions.PostToolUse must NOT exist (placement guard)
if node -e "const s=JSON.parse(require('fs').readFileSync(process.argv[1],'utf8')); process.exit(s.permissions&&s.permissions.PostToolUse ? 1 : 0);" -- "$SETTINGS" 2>/dev/null; then
    pass "SR4. permissions.PostToolUse absent (not misplaced)"
else
    fail "SR4. permissions.PostToolUse present — PostToolUse is misplaced inside permissions"
fi

# ---------------------------------------------------------------------------
# E2E: PostToolUse hook real invocation (requires RUN_E2E=1)
# ---------------------------------------------------------------------------

echo ""
echo "=== E2E: PostToolUse hook real invocation ==="

if [ "${RUN_E2E:-0}" = "1" ]; then
    E1_SESSION_ID="e1e1e1e1-0000-0000-0000-000000000001"
    E1_REPO="$TMPDIR_BASE/e2e-e1-repo"

    # Setup: fresh git repo (no remote → isPrivateRepo() returns false)
    mkdir -p "$E1_REPO/.claude"
    git -C "$E1_REPO" init -q
    git -C "$E1_REPO" config user.email "test@example.com"
    git -C "$E1_REPO" config user.name "Test"

    # Copy settings.json so --setting-sources project picks up the PostToolUse hook
    cp "$DOTFILES_DIR/claude-global/settings.json" "$E1_REPO/.claude/settings.json"

    E1_STATE_FILE="$E1_REPO/.git/workflow/$E1_SESSION_ID.json"

    # Run claude -p with a prompt that emits the workflow marker.
    # DOTFILES_DIR is exported so the hook command (node "$DOTFILES_DIR/...") resolves correctly.
    # --setting-sources project: only load <cwd>/.claude/settings.json (has PostToolUse hook).
    # --dangerously-skip-permissions: allows the echo command without interactive prompt.
    E1_OUTPUT=$(
        cd "$E1_REPO" && \
        DOTFILES_DIR="$DOTFILES_DIR" timeout 120 \
        claude -p \
            'Run exactly this Bash command and nothing else: echo "<<WORKFLOW_MARK_STEP_research_complete>>"' \
            --session-id "$E1_SESSION_ID" \
            --setting-sources project \
            --dangerously-skip-permissions \
            --output-format text \
        2>&1
    )
    E1_EXIT=$?

    if [ -f "$E1_STATE_FILE" ]; then
        # Check that steps.research.status === "complete"
        E1_STATUS=$(node -e "
const s = JSON.parse(require('fs').readFileSync(process.argv[1], 'utf8'));
process.stdout.write((s.steps && s.steps.research && s.steps.research.status) || '');
" -- "$E1_STATE_FILE" 2>/dev/null)
        if [ "$E1_STATUS" = "complete" ]; then
            pass "E1. claude -p PostToolUse hook fires → research=complete in state file"
        else
            fail "E1. claude -p PostToolUse hook fired but research.status=\"$E1_STATUS\" (expected \"complete\"). State: $(node -e "process.stdout.write(require('fs').readFileSync(process.argv[1],'utf8'));" -- "$E1_STATE_FILE" 2>/dev/null)"
        fi
    else
        fail "E1. claude -p PostToolUse hook did not create state file $E1_STATE_FILE. claude exit=$E1_EXIT output: $E1_OUTPUT"
    fi
else
    echo "SKIP: E1. claude -p E2E (set RUN_E2E=1 to enable)"
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
