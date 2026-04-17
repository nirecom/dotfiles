#!/bin/bash
# Tests for evidence-based write_tests/docs enforcement
# in workflow-gate.js (PreToolUse) and workflow-mark.js (PostToolUse)
set -euo pipefail

DOTFILES_DIR="$(cd "$(dirname "$0")/.." && pwd)"
GATE_HOOK="$DOTFILES_DIR/claude-global/hooks/workflow-gate.js"
MARK_HOOK="$DOTFILES_DIR/claude-global/hooks/workflow-mark.js"
ERRORS=0

fail() { echo "FAIL: $1"; ERRORS=$((ERRORS + 1)); }
pass() { echo "PASS: $1"; }

TMPDIR_BASE=$(mktemp -d)
WORKFLOW_DIR="$TMPDIR_BASE/workflow-state"
mkdir -p "$WORKFLOW_DIR"
export CLAUDE_WORKFLOW_DIR="$WORKFLOW_DIR"
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
    local sid="$1" json="$2"
    mkdir -p "$WORKFLOW_DIR"
    printf '%s' "$json" > "$WORKFLOW_DIR/${sid}.json"
}

read_state_status() {
    local sid="$1" step="$2"
    local state_file="$WORKFLOW_DIR/${sid}.json"
    if [ ! -f "$state_file" ]; then echo "MISSING"; return; fi
    node -e "
      try {
        const s = JSON.parse(require('fs').readFileSync(process.argv[1], 'utf8'));
        const step = s.steps && s.steps['$step'];
        console.log(step && step.status ? step.status : 'MISSING');
      } catch (e) { console.log('MISSING'); }
    " "$state_file" 2>/dev/null || echo "MISSING"
}

# Read an arbitrary nested field from steps.<step>.<field> (e.g. skip_reason).
# Prints "MISSING" when the file, step, or field is absent.
read_state_field() {
    local sid="$1" step="$2" field="$3"
    local state_file="$WORKFLOW_DIR/${sid}.json"
    if [ ! -f "$state_file" ]; then echo "MISSING"; return; fi
    node -e "
      try {
        const s = JSON.parse(require('fs').readFileSync(process.argv[1], 'utf8'));
        const step = s.steps && s.steps['$step'];
        if (!step || step['$field'] === undefined || step['$field'] === null) {
          console.log('MISSING');
        } else {
          console.log(step['$field']);
        }
      } catch (e) { console.log('MISSING'); }
    " "$state_file" 2>/dev/null || echo "MISSING"
}

expect_state_step() {
    local desc="$1" sid="$2" step="$3" expected="$4"
    local actual
    actual=$(read_state_status "$sid" "$step")
    if [ "$actual" = "$expected" ]; then pass "$desc"
    else fail "$desc — expected steps.$step.status=$expected, got: $actual"; fi
}

# State JSON where all steps are complete EXCEPT a given step
ALL_COMPLETE_EXCEPT() {
    local except_step="$1" sid="${2:-test-session}"
    cat <<EOF
{
  "version": 1,
  "session_id": "$sid",
  "created_at": "2026-04-11T10:00:00.000Z",
  "steps": {
    "research":          {"status": "complete", "updated_at": "2026-04-11T10:01:00.000Z"},
    "plan":              {"status": "complete", "updated_at": "2026-04-11T10:02:00.000Z"},
    "write_tests":       {"status": "$([ "$except_step" = "write_tests" ] && echo "pending" || echo "complete")", "updated_at": $([ "$except_step" = "write_tests" ] && echo "null" || echo '"2026-04-11T10:03:00.000Z"')},
    "code":              {"status": "complete", "updated_at": "2026-04-11T10:04:00.000Z"},
    "verify":            {"status": "complete", "updated_at": "2026-04-11T10:05:00.000Z"},
    "docs":              {"status": "$([ "$except_step" = "docs" ] && echo "pending" || echo "complete")", "updated_at": $([ "$except_step" = "docs" ] && echo "null" || echo '"2026-04-11T10:06:00.000Z"')},
    "user_verification": {"status": "complete", "updated_at": "2026-04-11T10:07:00.000Z"}
  }
}
EOF
}

ALL_COMPLETE_EXCEPT_TWO() {
    local sid="${1:-test-session}"
    cat <<EOF
{
  "version": 1,
  "session_id": "$sid",
  "created_at": "2026-04-11T10:00:00.000Z",
  "steps": {
    "research":          {"status": "complete", "updated_at": "2026-04-11T10:01:00.000Z"},
    "plan":              {"status": "complete", "updated_at": "2026-04-11T10:02:00.000Z"},
    "write_tests":       {"status": "pending", "updated_at": null},
    "code":              {"status": "complete", "updated_at": "2026-04-11T10:04:00.000Z"},
    "verify":            {"status": "complete", "updated_at": "2026-04-11T10:05:00.000Z"},
    "docs":              {"status": "pending", "updated_at": null},
    "user_verification": {"status": "complete", "updated_at": "2026-04-11T10:07:00.000Z"}
  }
}
EOF
}

# Convert path to mixed-mode (C:/...) for node compatibility on Windows
to_node_path() {
    cygpath -m "$1" 2>/dev/null || echo "$1"
}

run_gate() {
    local json="$1"
    echo "$json" | CLAUDE_WORKFLOW_DIR="$WORKFLOW_DIR" node "$GATE_HOOK" 2>/dev/null
}

run_mark() {
    local json="$1"
    echo "$json" | CLAUDE_WORKFLOW_DIR="$WORKFLOW_DIR" node "$MARK_HOOK" 2>/dev/null || true
}

build_mark_json() {
    local cmd="$1" sid="${2:-test-session}" exit_code="${3:-0}"
    local esc=${cmd//\\/\\\\}
    esc=${esc//\"/\\\"}
    printf '{"tool_name":"Bash","tool_input":{"command":"%s"},"tool_response":{"exit_code":%s,"stdout":"%s\\n","stderr":""},"session_id":"%s"}' \
        "$esc" "$exit_code" "$esc" "$sid"
}

# ===========================================================================
# workflow-gate.js — evidence-based checks
# ===========================================================================

echo ""
echo "=== WS-EV-1: tests/ file staged, write_tests=pending → gate approves ==="

REPO=$(setup_repo)
REPO_N=$(to_node_path "$REPO")
SID="ev1-$$"
write_state "$SID" "$(ALL_COMPLETE_EXCEPT write_tests "$SID")"
# Stage a test file
mkdir -p "$REPO/tests"
echo "test content" > "$REPO/tests/my-test.sh"
git -C "$REPO" add tests/my-test.sh

GATE_INPUT=$(printf '{"tool_name":"Bash","tool_input":{"command":"git -C %s commit -m \\"test\\""},"session_id":"%s"}' "$REPO_N" "$SID")
GATE_OUT=$(run_gate "$GATE_INPUT")

if echo "$GATE_OUT" | grep -q '"approve"'; then
    pass "WS-EV-1. tests/ staged + write_tests=pending → approve"
else
    fail "WS-EV-1. expected approve, got: $GATE_OUT"
fi

echo ""
echo "=== WS-EV-2: docs/*.md file staged, docs=pending → gate approves ==="

REPO=$(setup_repo)
REPO_N=$(to_node_path "$REPO")
SID="ev2-$$"
write_state "$SID" "$(ALL_COMPLETE_EXCEPT docs "$SID")"
# Stage a doc file
mkdir -p "$REPO/docs"
echo "doc content" > "$REPO/docs/guide.md"
git -C "$REPO" add docs/guide.md

GATE_INPUT=$(printf '{"tool_name":"Bash","tool_input":{"command":"git -C %s commit -m \\"test\\""},"session_id":"%s"}' "$REPO_N" "$SID")
GATE_OUT=$(run_gate "$GATE_INPUT")

if echo "$GATE_OUT" | grep -q '"approve"'; then
    pass "WS-EV-2. docs/ staged + docs=pending → approve"
else
    fail "WS-EV-2. expected approve, got: $GATE_OUT"
fi

echo ""
echo "=== WS-EV-3: no tests/ staged, write_tests=pending → gate blocks ==="

REPO=$(setup_repo)
REPO_N=$(to_node_path "$REPO")
SID="ev3-$$"
write_state "$SID" "$(ALL_COMPLETE_EXCEPT write_tests "$SID")"
# Stage a non-test file only
echo "source code" > "$REPO/app.js"
git -C "$REPO" add app.js

GATE_INPUT=$(printf '{"tool_name":"Bash","tool_input":{"command":"git -C %s commit -m \\"test\\""},"session_id":"%s"}' "$REPO_N" "$SID")
GATE_OUT=$(run_gate "$GATE_INPUT")

if echo "$GATE_OUT" | grep -q '"block"' && echo "$GATE_OUT" | grep -q 'write_tests'; then
    pass "WS-EV-3. no tests/ staged + write_tests=pending → block mentioning write_tests"
else
    fail "WS-EV-3. expected block mentioning write_tests, got: $GATE_OUT"
fi

echo ""
echo "=== WS-EV-4: no docs staged, docs=pending → gate blocks ==="

REPO=$(setup_repo)
REPO_N=$(to_node_path "$REPO")
SID="ev4-$$"
write_state "$SID" "$(ALL_COMPLETE_EXCEPT docs "$SID")"
# Stage a non-doc file only
echo "source code" > "$REPO/app.js"
git -C "$REPO" add app.js

GATE_INPUT=$(printf '{"tool_name":"Bash","tool_input":{"command":"git -C %s commit -m \\"test\\""},"session_id":"%s"}' "$REPO_N" "$SID")
GATE_OUT=$(run_gate "$GATE_INPUT")

if echo "$GATE_OUT" | grep -q '"block"' && echo "$GATE_OUT" | grep -q 'docs'; then
    pass "WS-EV-4. no docs staged + docs=pending → block mentioning docs"
else
    fail "WS-EV-4. expected block mentioning docs, got: $GATE_OUT"
fi

echo ""
echo "=== WS-EV-5: claude-global/ only staged, write_tests=pending → gate blocks ==="

REPO=$(setup_repo)
REPO_N=$(to_node_path "$REPO")
SID="ev5-$$"
write_state "$SID" "$(ALL_COMPLETE_EXCEPT write_tests "$SID")"
# Stage a file in claude-global/ only (no tests/)
mkdir -p "$REPO/claude-global"
echo "config" > "$REPO/claude-global/settings.json"
git -C "$REPO" add claude-global/settings.json

GATE_INPUT=$(printf '{"tool_name":"Bash","tool_input":{"command":"git -C %s commit -m \\"test\\""},"session_id":"%s"}' "$REPO_N" "$SID")
GATE_OUT=$(run_gate "$GATE_INPUT")

if echo "$GATE_OUT" | grep -q '"block"' && echo "$GATE_OUT" | grep -q 'write_tests'; then
    pass "WS-EV-5. claude-global/ only + write_tests=pending → block (no exempt)"
else
    fail "WS-EV-5. expected block mentioning write_tests, got: $GATE_OUT"
fi

# ===========================================================================
# workflow-mark.js — MARK_STEP rejection + NOT_NEEDED handlers
# ===========================================================================

echo ""
echo "=== WS-EV-6: MARK_STEP write_tests_complete → rejected, NOT recorded ==="

REPO=$(setup_repo)
SID="ev6-$$"
write_state "$SID" "$(ALL_COMPLETE_EXCEPT write_tests "$SID")"

MARK_JSON=$(build_mark_json 'echo "<<WORKFLOW_MARK_STEP_write_tests_complete>>"' "$SID")
MARK_OUT=$(run_mark "$MARK_JSON")

if echo "$MARK_OUT" | grep -q "NOT recorded"; then
    pass "WS-EV-6a. MARK_STEP write_tests_complete → additionalContext contains 'NOT recorded'"
else
    fail "WS-EV-6a. expected 'NOT recorded' in output, got: $MARK_OUT"
fi

ACTUAL_STATUS=$(read_state_status "$SID" "write_tests")
if [ "$ACTUAL_STATUS" = "pending" ]; then
    pass "WS-EV-6b. write_tests state remains pending"
else
    fail "WS-EV-6b. expected write_tests=pending, got: $ACTUAL_STATUS"
fi

echo ""
echo "=== WS-EV-7: MARK_STEP docs_complete → rejected, NOT recorded ==="

REPO=$(setup_repo)
SID="ev7-$$"
write_state "$SID" "$(ALL_COMPLETE_EXCEPT docs "$SID")"

MARK_JSON=$(build_mark_json 'echo "<<WORKFLOW_MARK_STEP_docs_complete>>"' "$SID")
MARK_OUT=$(run_mark "$MARK_JSON")

if echo "$MARK_OUT" | grep -q "NOT recorded"; then
    pass "WS-EV-7a. MARK_STEP docs_complete → additionalContext contains 'NOT recorded'"
else
    fail "WS-EV-7a. expected 'NOT recorded' in output, got: $MARK_OUT"
fi

ACTUAL_STATUS=$(read_state_status "$SID" "docs")
if [ "$ACTUAL_STATUS" = "pending" ]; then
    pass "WS-EV-7b. docs state remains pending"
else
    fail "WS-EV-7b. expected docs=pending, got: $ACTUAL_STATUS"
fi

echo ""
echo "=== WS-EV-8: WORKFLOW_WRITE_TESTS_NOT_NEEDED → write_tests=complete ==="

REPO=$(setup_repo)
SID="ev8-$$"
write_state "$SID" "$(ALL_COMPLETE_EXCEPT write_tests "$SID")"

MARK_JSON=$(build_mark_json 'echo "<<WORKFLOW_WRITE_TESTS_NOT_NEEDED>>"' "$SID")
MARK_OUT=$(run_mark "$MARK_JSON")

expect_state_step "WS-EV-8. WORKFLOW_WRITE_TESTS_NOT_NEEDED → write_tests=complete" \
    "$SID" "write_tests" "complete"

echo ""
echo "=== WS-EV-9: WORKFLOW_DOCS_NOT_NEEDED: <reason> → docs=complete, skip_reason recorded ==="

REPO=$(setup_repo)
SID="ev9-$$"
write_state "$SID" "$(ALL_COMPLETE_EXCEPT docs "$SID")"

MARK_JSON=$(build_mark_json 'echo "<<WORKFLOW_DOCS_NOT_NEEDED: hook-internal refactor, no user-visible behavior>>"' "$SID")
MARK_OUT=$(run_mark "$MARK_JSON")

expect_state_step "WS-EV-9a. DOCS_NOT_NEEDED w/reason → docs=complete" \
    "$SID" "docs" "complete"

EV9_REASON=$(read_state_field "$SID" "docs" "skip_reason")
if [ "$EV9_REASON" = "hook-internal refactor, no user-visible behavior" ]; then
    pass "WS-EV-9b. docs.skip_reason recorded"
else
    fail "WS-EV-9b. expected skip_reason='hook-internal refactor, no user-visible behavior', got: $EV9_REASON"
fi

echo ""
echo "=== WS-EV-10: WORKFLOW_DOCS_NOT_NEEDED with CJK reason → accepted ==="

REPO=$(setup_repo)
SID="ev10-$$"
write_state "$SID" "$(ALL_COMPLETE_EXCEPT docs "$SID")"

MARK_JSON=$(build_mark_json 'echo "<<WORKFLOW_DOCS_NOT_NEEDED: 誤字修正のみで挙動変更なし>>"' "$SID")
MARK_OUT=$(run_mark "$MARK_JSON")

expect_state_step "WS-EV-10a. CJK reason → docs=complete" \
    "$SID" "docs" "complete"

EV10_REASON=$(read_state_field "$SID" "docs" "skip_reason")
if [ "$EV10_REASON" = "誤字修正のみで挙動変更なし" ]; then
    pass "WS-EV-10b. docs.skip_reason recorded (CJK)"
else
    fail "WS-EV-10b. expected skip_reason='誤字修正のみで挙動変更なし', got: $EV10_REASON"
fi

echo ""
echo "=== WS-EV-11: WORKFLOW_DOCS_NOT_NEEDED with too-short reason → rejected ==="

REPO=$(setup_repo)
SID="ev11-$$"
write_state "$SID" "$(ALL_COMPLETE_EXCEPT docs "$SID")"

MARK_JSON=$(build_mark_json 'echo "<<WORKFLOW_DOCS_NOT_NEEDED: xx>>"' "$SID")
MARK_OUT=$(run_mark "$MARK_JSON")

EV11_STATUS=$(read_state_status "$SID" "docs")
if [ "$EV11_STATUS" = "pending" ]; then
    pass "WS-EV-11a. too-short reason → docs stays pending"
else
    fail "WS-EV-11a. expected docs=pending, got: $EV11_STATUS"
fi

if echo "$MARK_OUT" | grep -qi "too short"; then
    pass "WS-EV-11b. output mentions 'too short'"
else
    fail "WS-EV-11b. expected output to contain 'too short' (case-insensitive), got: $MARK_OUT"
fi

echo ""
echo "=== WS-EV-12: WORKFLOW_DOCS_NOT_NEEDED with ASCII dud ('none') → rejected ==="

REPO=$(setup_repo)
SID="ev12-$$"
write_state "$SID" "$(ALL_COMPLETE_EXCEPT docs "$SID")"

MARK_JSON=$(build_mark_json 'echo "<<WORKFLOW_DOCS_NOT_NEEDED: none>>"' "$SID")
MARK_OUT=$(run_mark "$MARK_JSON")

EV12_STATUS=$(read_state_status "$SID" "docs")
if [ "$EV12_STATUS" = "pending" ]; then
    pass "WS-EV-12a. ASCII dud 'none' → docs stays pending"
else
    fail "WS-EV-12a. expected docs=pending, got: $EV12_STATUS"
fi

if echo "$MARK_OUT" | grep -qi "placeholder"; then
    pass "WS-EV-12b. output mentions 'placeholder'"
else
    fail "WS-EV-12b. expected output to contain 'placeholder' (case-insensitive), got: $MARK_OUT"
fi

echo ""
echo "=== WS-EV-13: WORKFLOW_DOCS_NOT_NEEDED idempotent overwrite of skip_reason ==="

REPO=$(setup_repo)
SID="ev13-$$"
write_state "$SID" "$(ALL_COMPLETE_EXCEPT docs "$SID")"

MARK_JSON=$(build_mark_json 'echo "<<WORKFLOW_DOCS_NOT_NEEDED: first reason here>>"' "$SID")
MARK_OUT=$(run_mark "$MARK_JSON")

EV13_REASON1=$(read_state_field "$SID" "docs" "skip_reason")
if [ "$EV13_REASON1" = "first reason here" ]; then
    pass "WS-EV-13a. first skip_reason recorded"
else
    fail "WS-EV-13a. expected skip_reason='first reason here', got: $EV13_REASON1"
fi

MARK_JSON=$(build_mark_json 'echo "<<WORKFLOW_DOCS_NOT_NEEDED: second reason here>>"' "$SID")
MARK_OUT=$(run_mark "$MARK_JSON")

expect_state_step "WS-EV-13b. after second mark docs=complete" \
    "$SID" "docs" "complete"

EV13_REASON2=$(read_state_field "$SID" "docs" "skip_reason")
if [ "$EV13_REASON2" = "second reason here" ]; then
    pass "WS-EV-13c. skip_reason overwritten with second value"
else
    fail "WS-EV-13c. expected skip_reason='second reason here', got: $EV13_REASON2"
fi

echo ""
echo "=== WS-EV-14: WORKFLOW_DOCS_NOT_NEEDED with CJK dud ('スキップする') → rejected ==="

REPO=$(setup_repo)
SID="ev14-$$"
write_state "$SID" "$(ALL_COMPLETE_EXCEPT docs "$SID")"

MARK_JSON=$(build_mark_json 'echo "<<WORKFLOW_DOCS_NOT_NEEDED: スキップする>>"' "$SID")
MARK_OUT=$(run_mark "$MARK_JSON")

EV14_STATUS=$(read_state_status "$SID" "docs")
if [ "$EV14_STATUS" = "pending" ]; then
    pass "WS-EV-14a. CJK dud → docs stays pending"
else
    fail "WS-EV-14a. expected docs=pending, got: $EV14_STATUS"
fi

if echo "$MARK_OUT" | grep -qi "placeholder"; then
    pass "WS-EV-14b. output mentions 'placeholder'"
else
    fail "WS-EV-14b. expected output to contain 'placeholder' (case-insensitive), got: $MARK_OUT"
fi

echo ""
echo "=== WS-EV-15: WORKFLOW_DOCS_NOT_NEEDED with '>' in reason → rejected ==="

REPO=$(setup_repo)
SID="ev15-$$"
write_state "$SID" "$(ALL_COMPLETE_EXCEPT docs "$SID")"

MARK_JSON=$(build_mark_json 'echo "<<WORKFLOW_DOCS_NOT_NEEDED: a>b>>"' "$SID")
MARK_OUT=$(run_mark "$MARK_JSON")

EV15_STATUS=$(read_state_status "$SID" "docs")
if [ "$EV15_STATUS" = "pending" ]; then
    pass "WS-EV-15a. reason containing '>' → docs stays pending"
else
    fail "WS-EV-15a. expected docs=pending, got: $EV15_STATUS"
fi

if echo "$MARK_OUT" | grep -qiE "malformed|contains no|'>'|>"; then
    pass "WS-EV-15b. output contains helpful rejection message"
else
    fail "WS-EV-15b. expected 'malformed'/'contains no'/\"'>'\" hint, got: $MARK_OUT"
fi

echo ""
echo "=== WS-EV-16: WORKFLOW_DOCS_NOT_NEEDED with leading whitespace → trimmed ==="

REPO=$(setup_repo)
SID="ev16-$$"
write_state "$SID" "$(ALL_COMPLETE_EXCEPT docs "$SID")"

MARK_JSON=$(build_mark_json 'echo "<<WORKFLOW_DOCS_NOT_NEEDED:   leading space reason>>"' "$SID")
MARK_OUT=$(run_mark "$MARK_JSON")

expect_state_step "WS-EV-16a. leading-space reason → docs=complete" \
    "$SID" "docs" "complete"

EV16_REASON=$(read_state_field "$SID" "docs" "skip_reason")
if [ "$EV16_REASON" = "leading space reason" ]; then
    pass "WS-EV-16b. skip_reason trimmed of leading whitespace"
else
    fail "WS-EV-16b. expected skip_reason='leading space reason', got: $EV16_REASON"
fi

echo ""
echo "=== WS-EV-17: legacy bare WORKFLOW_DOCS_NOT_NEEDED (no reason) → rejected ==="

REPO=$(setup_repo)
SID="ev17-$$"
write_state "$SID" "$(ALL_COMPLETE_EXCEPT docs "$SID")"

MARK_JSON=$(build_mark_json 'echo "<<WORKFLOW_DOCS_NOT_NEEDED>>"' "$SID")
MARK_OUT=$(run_mark "$MARK_JSON")

EV17_STATUS=$(read_state_status "$SID" "docs")
if [ "$EV17_STATUS" = "pending" ]; then
    pass "WS-EV-17a. bare form → docs stays pending"
else
    fail "WS-EV-17a. expected docs=pending, got: $EV17_STATUS"
fi

if echo "$MARK_OUT" | grep -qiE "reason|malformed"; then
    pass "WS-EV-17b. output hints at new format ('reason'/'malformed')"
else
    fail "WS-EV-17b. expected 'reason' or 'malformed' hint, got: $MARK_OUT"
fi

# ===========================================================================
# Results
# ===========================================================================

echo ""
echo "=== Results ==="
if [ "$ERRORS" -eq 0 ]; then
    echo "All tests passed!"
else
    echo "$ERRORS test(s) failed"
    exit 1
fi
