#!/bin/bash
# TDD tests for <<WORKFLOW_USER_VERIFIED>> marker (UV-N1, UV-F1..F5, UV-E1..E3, UV-I1)
# Features NOT YET IMPLEMENTED — some tests are expected to FAIL.
set -euo pipefail

DOTFILES_DIR="$(cd "$(dirname "$0")/.." && pwd)"
MARK_HOOK="$DOTFILES_DIR/claude-global/hooks/workflow-mark.js"
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

expect_state_step() {
    local desc="$1" repo="$2" sid="$3" step="$4" expected="$5"
    local actual
    actual=$(read_state_status "$repo" "$sid" "$step")
    if [ "$actual" = "$expected" ]; then pass "$desc"
    else fail "$desc — expected steps.$step.status=$expected, got: $actual"; fi
}

expect_no_state_change() {
    local desc="$1" repo="$2" sid="$3" step="$4" expected_unchanged="$5"
    local actual
    actual=$(read_state_status "$repo" "$sid" "$step")
    if [ "$actual" = "$expected_unchanged" ]; then pass "$desc"
    else fail "$desc — expected steps.$step.status to remain $expected_unchanged, got: $actual"; fi
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

run_mark_hook() {
    local repo="$1" json="$2"
    echo "$json" | CLAUDE_PROJECT_DIR="$repo" node "$MARK_HOOK" 2>/dev/null || true
}

# Build hook input JSON for a Bash tool invocation
build_uv_json() {
    local cmd="$1" sid="${2:-test-session}" exit_code="${3:-0}"
    local esc=${cmd//\\/\\\\}
    esc=${esc//\"/\\\"}
    printf '{"tool_name":"Bash","tool_input":{"command":"%s"},"tool_response":{"exit_code":%s,"stdout":"%s\\n","stderr":""},"session_id":"%s"}' \
        "$esc" "$exit_code" "$esc" "$sid"
}

# ---------------------------------------------------------------------------
# UV-N1: echo "<<WORKFLOW_USER_VERIFIED>>" → user_verification=complete
# Expected FAIL before implementation
# ---------------------------------------------------------------------------

echo ""
echo "=== workflow-mark: UV-N1 — WORKFLOW_USER_VERIFIED normal case ==="

REPO=$(setup_repo)
write_state "$REPO" "test-session" "$(ALL_PENDING_JSON test-session)"
UV_N1_JSON=$(build_uv_json 'echo "<<WORKFLOW_USER_VERIFIED>>"')
run_mark_hook "$REPO" "$UV_N1_JSON" >/dev/null
expect_state_step "UV-N1. echo \"<<WORKFLOW_USER_VERIFIED>>\" → user_verification=complete" \
    "$REPO" "test-session" "user_verification" "complete"

# ---------------------------------------------------------------------------
# UV-F1: two spaces → no match
# Expected PASS before implementation (unrecognized command, state unchanged)
# ---------------------------------------------------------------------------

echo ""
echo "=== workflow-mark: UV-F1 — two spaces → no match ==="

REPO=$(setup_repo)
write_state "$REPO" "test-session" "$(ALL_PENDING_JSON test-session)"
UV_F1_JSON=$(build_uv_json 'echo  "<<WORKFLOW_USER_VERIFIED>>"')
run_mark_hook "$REPO" "$UV_F1_JSON" >/dev/null
expect_no_state_change "UV-F1. echo  (two spaces) → user_verification stays pending" \
    "$REPO" "test-session" "user_verification" "pending"

# ---------------------------------------------------------------------------
# UV-F2: prefix chain → no match
# Expected PASS before implementation
# ---------------------------------------------------------------------------

echo ""
echo "=== workflow-mark: UV-F2 — prefix chain → no match ==="

REPO=$(setup_repo)
write_state "$REPO" "test-session" "$(ALL_PENDING_JSON test-session)"
UV_F2_JSON=$(build_uv_json 'cd /tmp && echo "<<WORKFLOW_USER_VERIFIED>>"')
run_mark_hook "$REPO" "$UV_F2_JSON" >/dev/null
expect_no_state_change "UV-F2. cd /tmp && echo → user_verification stays pending" \
    "$REPO" "test-session" "user_verification" "pending"

# ---------------------------------------------------------------------------
# UV-F3: redirect → no match
# Expected PASS before implementation
# ---------------------------------------------------------------------------

echo ""
echo "=== workflow-mark: UV-F3 — redirect → no match ==="

REPO=$(setup_repo)
write_state "$REPO" "test-session" "$(ALL_PENDING_JSON test-session)"
UV_F3_JSON=$(build_uv_json 'echo "<<WORKFLOW_USER_VERIFIED>>" > /tmp/out')
run_mark_hook "$REPO" "$UV_F3_JSON" >/dev/null
expect_no_state_change "UV-F3. redirect → user_verification stays pending" \
    "$REPO" "test-session" "user_verification" "pending"

# ---------------------------------------------------------------------------
# UV-F4: unrelated command → no match
# Expected PASS before implementation
# ---------------------------------------------------------------------------

echo ""
echo "=== workflow-mark: UV-F4 — unrelated command → no match ==="

REPO=$(setup_repo)
write_state "$REPO" "test-session" "$(ALL_PENDING_JSON test-session)"
UV_F4_JSON=$(build_uv_json 'cat /tmp/verified.txt')
run_mark_hook "$REPO" "$UV_F4_JSON" >/dev/null
expect_no_state_change "UV-F4. cat command → user_verification stays pending" \
    "$REPO" "test-session" "user_verification" "pending"

# ---------------------------------------------------------------------------
# UV-F5: single-quoted → no match (DQ only)
# Expected PASS before implementation
# ---------------------------------------------------------------------------

echo ""
echo "=== workflow-mark: UV-F5 — single-quoted → no match ==="

REPO=$(setup_repo)
write_state "$REPO" "test-session" "$(ALL_PENDING_JSON test-session)"
UV_F5_JSON='{"tool_name":"Bash","tool_input":{"command":"echo '"'"'<<WORKFLOW_USER_VERIFIED>>'"'"'"},"tool_response":{"exit_code":0,"stdout":"<<WORKFLOW_USER_VERIFIED>>\n","stderr":""},"session_id":"test-session"}'
run_mark_hook "$REPO" "$UV_F5_JSON" >/dev/null
expect_no_state_change "UV-F5. single-quoted → user_verification stays pending (DQ only)" \
    "$REPO" "test-session" "user_verification" "pending"

# ---------------------------------------------------------------------------
# UV-E1: exit_code=1 → no change
# Expected PASS before implementation
# ---------------------------------------------------------------------------

echo ""
echo "=== workflow-mark: UV-E1 — exit_code=1 → no change ==="

REPO=$(setup_repo)
write_state "$REPO" "test-session" "$(ALL_PENDING_JSON test-session)"
UV_E1_JSON=$(build_uv_json 'echo "<<WORKFLOW_USER_VERIFIED>>"' "test-session" "1")
run_mark_hook "$REPO" "$UV_E1_JSON" >/dev/null
expect_no_state_change "UV-E1. exit_code=1 → user_verification stays pending" \
    "$REPO" "test-session" "user_verification" "pending"

# ---------------------------------------------------------------------------
# UV-E2: no session_id → no change AND stdout contains "additionalContext"
# UV-E2a: no change — Expected PASS before implementation
# UV-E2b: additionalContext in output — Expected FAIL before implementation
# ---------------------------------------------------------------------------

echo ""
echo "=== workflow-mark: UV-E2 — no session_id ==="

REPO=$(setup_repo)
write_state "$REPO" "test-session" "$(ALL_PENDING_JSON test-session)"

UV_E2_CMD='echo "<<WORKFLOW_USER_VERIFIED>>"'
UV_E2_ESC=${UV_E2_CMD//\"/\\\"}
UV_E2_JSON=$(printf '{"tool_name":"Bash","tool_input":{"command":"%s"},"tool_response":{"exit_code":0,"stdout":"%s\\n","stderr":""}}' "$UV_E2_ESC" "$UV_E2_ESC")

UV_E2_OUT=$(echo "$UV_E2_JSON" | CLAUDE_PROJECT_DIR="$REPO" env -u CLAUDE_ENV_FILE node "$MARK_HOOK" 2>/dev/null || true)

# UV-E2a: user_verification stays pending (no session_id means no write)
UV_E2A_STATUS=$(read_state_status "$REPO" "test-session" "user_verification")
if [ "$UV_E2A_STATUS" = "pending" ]; then
    pass "UV-E2a. no session_id → user_verification stays pending"
else
    fail "UV-E2a. no session_id → user_verification changed to $UV_E2A_STATUS (expected pending)"
fi

# UV-E2b: output must contain "additionalContext" (JSON key or message)
if echo "$UV_E2_OUT" | grep -q "additionalContext"; then
    pass "UV-E2b. no session_id → stdout contains additionalContext"
else
    fail "UV-E2b. no session_id → stdout does not contain additionalContext (got: $UV_E2_OUT)"
fi

# ---------------------------------------------------------------------------
# UV-E3: tool_name=Write → no change
# Expected PASS before implementation
# ---------------------------------------------------------------------------

echo ""
echo "=== workflow-mark: UV-E3 — tool_name=Write → no change ==="

REPO=$(setup_repo)
write_state "$REPO" "test-session" "$(ALL_PENDING_JSON test-session)"
UV_E3_JSON='{"tool_name":"Write","tool_input":{"file_path":"/tmp/foo","content":"<<WORKFLOW_USER_VERIFIED>>"},"tool_response":{"success":true},"session_id":"test-session"}'
run_mark_hook "$REPO" "$UV_E3_JSON" >/dev/null
expect_no_state_change "UV-E3. tool_name=Write → user_verification stays pending" \
    "$REPO" "test-session" "user_verification" "pending"

# ---------------------------------------------------------------------------
# UV-I1: applied twice → user_verification=complete (idempotent)
# Expected FAIL before implementation
# ---------------------------------------------------------------------------

echo ""
echo "=== workflow-mark: UV-I1 — idempotent (applied twice) ==="

REPO=$(setup_repo)
write_state "$REPO" "test-session" "$(ALL_PENDING_JSON test-session)"
UV_I1_JSON=$(build_uv_json 'echo "<<WORKFLOW_USER_VERIFIED>>"')
run_mark_hook "$REPO" "$UV_I1_JSON" >/dev/null
run_mark_hook "$REPO" "$UV_I1_JSON" >/dev/null
expect_state_step "UV-I1. applied twice → user_verification=complete (idempotent)" \
    "$REPO" "test-session" "user_verification" "complete"

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
