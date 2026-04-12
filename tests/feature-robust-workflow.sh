#!/bin/bash
# Test suite for workflow state machine:
#   claude-global/hooks/workflow-gate.js   (PreToolUse commit gate)
#   claude-global/hooks/mark-step.js       (step completion CLI)
#   claude-global/hooks/session-start.js   (SessionStart hook)
set -euo pipefail

DOTFILES_DIR="$(cd "$(dirname "$0")/.." && pwd)"
GATE_HOOK="$DOTFILES_DIR/claude-global/hooks/workflow-gate.js"
MARK_STEP="$DOTFILES_DIR/claude-global/hooks/mark-step.js"
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
    echo "$json" | HOOK_CWD="$repo" node "$GATE_HOOK" 2>/dev/null || true
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
if HOOK_CWD="$REPO" CLAUDE_ENV_FILE="$ENV_FILE" node "$MARK_STEP" research complete 2>/dev/null; then
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
if HOOK_CWD="$REPO" CLAUDE_ENV_FILE="$ENV_FILE" node "$MARK_STEP" plan skipped 2>/dev/null; then
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
HOOK_CWD="$REPO" CLAUDE_ENV_FILE="$ENV_FILE" node "$MARK_STEP" code complete 2>/dev/null || true
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
HOOK_CWD="$REPO" CLAUDE_ENV_FILE="$ENV_FILE" node "$MARK_STEP" verify complete 2>/dev/null || true
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
HOOK_CWD="$REPO" CLAUDE_ENV_FILE="$ENV_FILE" node "$MARK_STEP" --reset-from docs 2>/dev/null || true
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
HOOK_CWD="$REPO" CLAUDE_ENV_FILE="$ENV_FILE" node "$MARK_STEP" --reset-from research 2>/dev/null || true
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
HOOK_CWD="$REPO" CLAUDE_ENV_FILE="$ENV_FILE" node "$MARK_STEP" --reset-from user_verification 2>/dev/null || true
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
if HOOK_CWD="$REPO" CLAUDE_ENV_FILE="$ENV_FILE" node "$MARK_STEP" --reset-from invalid_step_name 2>/dev/null; then
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
if HOOK_CWD="$REPO" CLAUDE_ENV_FILE="$ENV_FILE" node "$MARK_STEP" not_a_real_step complete 2>/dev/null; then
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
if HOOK_CWD="$REPO" CLAUDE_ENV_FILE="$ENV_FILE" node "$MARK_STEP" research done 2>/dev/null; then
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
if HOOK_CWD="$REPO" CLAUDE_ENV_FILE="$ENV_FILE" node "$MARK_STEP" user_verification skipped 2>/dev/null; then
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
if HOOK_CWD="$REPO" CLAUDE_ENV_FILE="$ENV_FILE" node "$MARK_STEP" research 2>/dev/null; then
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
HOOK_CWD="$REPO" CLAUDE_ENV_FILE="$ENV_FILE" node "$MARK_STEP" research complete 2>/dev/null || true
if HOOK_CWD="$REPO" CLAUDE_ENV_FILE="$ENV_FILE" node "$MARK_STEP" research complete 2>/dev/null; then
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
if HOOK_CWD="$REPO" CLAUDE_ENV_FILE="$ENV_FILE" node "$MARK_STEP" research complete 2>/dev/null; then
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
if HOOK_CWD="$REPO" CLAUDE_ENV_FILE="$ENV_FILE" node "$MARK_STEP" research complete 2>/dev/null; then
    fail "31. CLAUDE_ENV_FILE without CLAUDE_SESSION_ID → expected exit 1, got exit 0"
else
    pass "31. CLAUDE_ENV_FILE without CLAUDE_SESSION_ID → exit 1"
fi
rm -f "$ENV_FILE"

# Test 32: CLAUDE_ENV_FILE not set (env var absent) → exit 1
REPO=$(setup_repo)
if HOOK_CWD="$REPO" node "$MARK_STEP" research complete 2>/dev/null; then
    fail "32. CLAUDE_ENV_FILE not set → expected exit 1, got exit 0"
else
    pass "32. CLAUDE_ENV_FILE not set → exit 1"
fi

# Test 33: CLAUDE_ENV_FILE points to non-existent file → exit 1
REPO=$(setup_repo)
NONEXISTENT_FILE="$TMPDIR_BASE/does-not-exist-$RANDOM.env"
if HOOK_CWD="$REPO" CLAUDE_ENV_FILE="$NONEXISTENT_FILE" node "$MARK_STEP" research complete 2>/dev/null; then
    fail "33. CLAUDE_ENV_FILE non-existent file → expected exit 1, got exit 0"
else
    pass "33. CLAUDE_ENV_FILE non-existent file → exit 1"
fi

# Test 34: CLAUDE_ENV_FILE is set to empty file (no CLAUDE_SESSION_ID line) → exit 1
REPO=$(setup_repo)
ENV_FILE=$(mktemp)
# empty file — no content written
if HOOK_CWD="$REPO" CLAUDE_ENV_FILE="$ENV_FILE" node "$MARK_STEP" research complete 2>/dev/null; then
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
echo '{"session_id":"abc123"}' | HOOK_CWD="$REPO" CLAUDE_ENV_FILE="$ENV_FILE" node "$SESSION_START" 2>/dev/null || true
if grep -qx "CLAUDE_SESSION_ID=abc123" "$ENV_FILE" 2>/dev/null; then
    pass "35. CLAUDE_ENV_FILE → file contains KEY=VALUE line (no export prefix)"
else
    fail "35. CLAUDE_ENV_FILE → expected exact line 'CLAUDE_SESSION_ID=abc123', file content: $(cat "$ENV_FILE" 2>/dev/null || echo '(not found)')"
fi

# Test 36: stdout output is {}
REPO=$(setup_repo)
STDOUT=$(echo '{"session_id":"abc123"}' | HOOK_CWD="$REPO" node "$SESSION_START" 2>/dev/null || true)
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
echo '{"session_id":"new-session"}' | HOOK_CWD="$REPO" node "$SESSION_START" 2>/dev/null || true
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
echo '{"session_id":"new-session"}' | HOOK_CWD="$REPO" node "$SESSION_START" 2>/dev/null || true
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
if echo '{"session_id":"abc123"}' | HOOK_CWD="$REPO" node "$SESSION_START" 2>/dev/null; then
    pass "39. CLAUDE_ENV_FILE not set → exits 0"
else
    fail "39. CLAUDE_ENV_FILE not set → expected exit 0, got non-zero"
fi

# Test 40: .git/workflow/ directory doesn't exist → cleanup runs without error
REPO=$(setup_repo)
# Do NOT create the workflow directory — verify no crash
if echo '{"session_id":"abc123"}' | HOOK_CWD="$REPO" node "$SESSION_START" 2>/dev/null; then
    pass "40. Missing workflow dir → cleanup runs without error"
else
    fail "40. Missing workflow dir → session-start crashed (exit non-zero)"
fi

# Test 41: stdin is invalid JSON → exits 0 (fail-open for SessionStart)
REPO=$(setup_repo)
if echo 'NOT VALID JSON' | HOOK_CWD="$REPO" node "$SESSION_START" 2>/dev/null; then
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

# Test 48: resolveRepoDir converts WSL path via toNativePath on win32 (HOOK_CWD unset)
RESULT=$(cd "$DOTFILES_DIR" && node -e "
  delete process.env.HOOK_CWD;
  Object.defineProperty(process,'platform',{value:'win32',configurable:true});
  const {resolveRepoDir}=require('./claude-global/hooks/lib/is-private-repo.js');
  console.log(resolveRepoDir('git -C /c/git/dotfiles commit'));
" 2>/dev/null)
[ "$RESULT" = "C:/git/dotfiles" ] && pass "48. resolveRepoDir: WSL path /c/git/dotfiles → C:/git/dotfiles on win32" || fail "48. resolveRepoDir: expected C:/git/dotfiles, got '$RESULT'"

# Test 49: resolveRepoDir returns "." when no -C flag and HOOK_CWD unset
RESULT=$(cd "$DOTFILES_DIR" && node -e "
  delete process.env.HOOK_CWD;
  Object.defineProperty(process,'platform',{value:'win32',configurable:true});
  const {resolveRepoDir}=require('./claude-global/hooks/lib/is-private-repo.js');
  console.log(resolveRepoDir('git commit'));
" 2>/dev/null)
[ "$RESULT" = "." ] && pass "49. resolveRepoDir: no -C flag → '.'" || fail "49. resolveRepoDir: expected '.', got '$RESULT'"

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
