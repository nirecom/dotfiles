#!/bin/bash
# TDD tests for session-start.js initial state file creation (T-D1-1 through T-D1-7, T-MIG-1..2)
# Features NOT YET IMPLEMENTED — some tests are expected to FAIL.
set -euo pipefail

DOTFILES_DIR="$(cd "$(dirname "$0")/.." && pwd)"
SESSION_START="$DOTFILES_DIR/claude-global/hooks/session-start.js"
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

get_state_file() {
    local sid="$1"
    echo "$WORKFLOW_DIR/${sid}.json"
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

# ---------------------------------------------------------------------------
# T-D1-1: session-start creates initial state file with all steps pending
# Expected FAIL before implementation (feature not yet implemented)
# ---------------------------------------------------------------------------

echo ""
echo "=== session-start: T-D1-1 — creates initial state file ==="

REPO=$(setup_repo)
SID="tdd1-$(printf '%04x%04x' $RANDOM $RANDOM)"
ENV_FILE="$TMPDIR_BASE/claude.env"

echo "{\"session_id\":\"$SID\"}" | CLAUDE_PROJECT_DIR="$REPO" CLAUDE_ENV_FILE="$ENV_FILE" CLAUDE_WORKFLOW_DIR="$WORKFLOW_DIR" node "$SESSION_START" 2>/dev/null || true

STATE_FILE=$(get_state_file "$SID")

if [ ! -f "$STATE_FILE" ]; then
    fail "T-D1-1. session-start did not create $WORKFLOW_DIR/$SID.json"
else
    # Check all 7 steps are present and have status "pending"
    STEPS_OK=$(node -e "
try {
  const s = JSON.parse(require('fs').readFileSync(process.argv[1], 'utf8'));
  const steps = ['research','plan','write_tests','code','verify','docs','user_verification'];
  const all = steps.every(k => s.steps && s.steps[k] && s.steps[k].status === 'pending');
  process.exit(all ? 0 : 1);
} catch (e) { process.exit(1); }
" "$STATE_FILE" 2>/dev/null; echo $?)
    if [ "$STEPS_OK" = "0" ]; then
        pass "T-D1-1. session-start created state file with all 7 steps pending"
    else
        fail "T-D1-1. state file exists but steps are not all pending — $(node -e "try{const s=JSON.parse(require('fs').readFileSync(process.argv[1],'utf8'));console.log(JSON.stringify(s.steps||{}));}catch(e){console.log('parse error');}" "$STATE_FILE" 2>/dev/null)"
    fi
fi

# ---------------------------------------------------------------------------
# T-D1-2: session-start does NOT overwrite existing state file
# ---------------------------------------------------------------------------

echo ""
echo "=== session-start: T-D1-2 — does not overwrite existing state ==="

SID2="tdd2-$(printf '%04x%04x' $RANDOM $RANDOM)"
ENV_FILE2="$TMPDIR_BASE/claude2.env"

# Pre-write a state file with research=complete
PRE_STATE='{
  "version": 1,
  "session_id": "'"$SID2"'",
  "created_at": "2026-04-11T10:00:00.000Z",
  "steps": {
    "research":          {"status": "complete", "updated_at": "2026-04-11T10:01:00.000Z"},
    "plan":              {"status": "pending", "updated_at": null},
    "write_tests":       {"status": "pending", "updated_at": null},
    "code":              {"status": "pending", "updated_at": null},
    "verify":            {"status": "pending", "updated_at": null},
    "docs":              {"status": "pending", "updated_at": null},
    "user_verification": {"status": "pending", "updated_at": null}
  }
}'
write_state "$SID2" "$PRE_STATE"

REPO2=$(setup_repo)
echo "{\"session_id\":\"$SID2\"}" | CLAUDE_PROJECT_DIR="$REPO2" CLAUDE_ENV_FILE="$ENV_FILE2" CLAUDE_WORKFLOW_DIR="$WORKFLOW_DIR" node "$SESSION_START" 2>/dev/null || true

RESEARCH_STATUS=$(read_state_status "$SID2" "research")
if [ "$RESEARCH_STATUS" = "complete" ]; then
    pass "T-D1-2. session-start did not overwrite existing state (research still complete)"
else
    fail "T-D1-2. session-start overwrote existing state — research.status=$RESEARCH_STATUS (expected complete)"
fi

# ---------------------------------------------------------------------------
# T-D1-3: session-start with no session_id creates no state file
# ---------------------------------------------------------------------------

echo ""
echo "=== session-start: T-D1-3 — no session_id → no state file ==="

REPO3=$(setup_repo)
ENV_FILE3="$TMPDIR_BASE/claude3.env"

# Count files before and after — no new file should be created
COUNT_BEFORE=$(ls "$WORKFLOW_DIR/"*.json 2>/dev/null | wc -l)
echo '{}' | CLAUDE_PROJECT_DIR="$REPO3" CLAUDE_ENV_FILE="$ENV_FILE3" CLAUDE_WORKFLOW_DIR="$WORKFLOW_DIR" node "$SESSION_START" 2>/dev/null || true
COUNT_AFTER=$(ls "$WORKFLOW_DIR/"*.json 2>/dev/null | wc -l)

if [ "$COUNT_BEFORE" = "$COUNT_AFTER" ]; then
    pass "T-D1-3. no session_id → no .json file created in $WORKFLOW_DIR/"
else
    fail "T-D1-3. session-start created a state file even though session_id was absent"
fi

# ---------------------------------------------------------------------------
# T-D1-4: CLAUDE_PROJECT_DIR なし (non-git dir) でも sessionId があれば state が作成される
# ---------------------------------------------------------------------------

echo ""
echo "=== session-start: T-D1-4 — CLAUDE_PROJECT_DIR absent → state still created ==="

SID4="tdd-d1-4-$(printf '%04x%04x' $RANDOM $RANDOM)"
ENV_FILE4="$TMPDIR_BASE/claude4.env"

echo "{\"session_id\":\"$SID4\"}" \
  | CLAUDE_WORKFLOW_DIR="$WORKFLOW_DIR" CLAUDE_ENV_FILE="$ENV_FILE4" \
    node "$SESSION_START" 2>/dev/null || true

STATE_FILE4="$WORKFLOW_DIR/${SID4}.json"
if [ -f "$STATE_FILE4" ]; then
    pass "T-D1-4. no CLAUDE_PROJECT_DIR → state file created in WORKFLOW_DIR"
else
    fail "T-D1-4. no CLAUDE_PROJECT_DIR → state file NOT created at $STATE_FILE4"
fi

# ---------------------------------------------------------------------------
# T-D1-5 (new): os.homedir() path used when CLAUDE_WORKFLOW_DIR is unset
# ---------------------------------------------------------------------------

echo ""
echo "=== session-start: T-D1-5 — os.homedir() fallback ==="

SID5="tdd-d1-5-$(printf '%04x%04x' $RANDOM $RANDOM)"
ENV_FILE5="$TMPDIR_BASE/claude5.env"
FAKE_HOME="$TMPDIR_BASE/fakehome-$$"
mkdir -p "$FAKE_HOME"

# Unset CLAUDE_WORKFLOW_DIR to exercise the os.homedir() path.
# Set both HOME and USERPROFILE so os.homedir() is redirected on both Unix and Windows.
T_D1_5_EXIT=0
env -u CLAUDE_WORKFLOW_DIR HOME="$FAKE_HOME" USERPROFILE="$FAKE_HOME" bash -c "
  echo '{\"session_id\":\"$SID5\"}' \
    | CLAUDE_ENV_FILE=\"$ENV_FILE5\" node \"$SESSION_START\" >/dev/null 2>&1
" || T_D1_5_EXIT=$?

if [ "$T_D1_5_EXIT" = "0" ]; then
    pass "T-D1-5a. os.homedir() fallback → exit 0"
else
    fail "T-D1-5a. os.homedir() fallback → exit $T_D1_5_EXIT"
fi

# Check file existence via Node.js to avoid shell/Node.js path format mismatch on Windows.
T_D1_5B_RESULT=$(HOME="$FAKE_HOME" USERPROFILE="$FAKE_HOME" node -e "
const os = require('os'); const path = require('path'); const fs = require('fs');
const p = path.join(os.homedir(), '.claude', 'projects', 'workflow', '$SID5.json');
console.log(fs.existsSync(p) ? 'ok' : 'missing:' + p);
" 2>/dev/null || echo "ERROR")
if [ "$T_D1_5B_RESULT" = "ok" ]; then
    pass "T-D1-5b. os.homedir() fallback → state created at expected path"
else
    fail "T-D1-5b. os.homedir() fallback → state NOT found at $FAKE_HOME/.claude/projects/workflow/${SID5}.json ($T_D1_5B_RESULT)"
fi

# ---------------------------------------------------------------------------
# T-D1-6: CLAUDE_WORKFLOW_DIR が存在しない状態で session-start は crash しない
# ---------------------------------------------------------------------------

echo ""
echo "=== session-start: T-D1-6 — missing workflow dir → no crash ==="

SID6="tdd-d1-6-$(printf '%04x%04x' $RANDOM $RANDOM)"
ENV_FILE6="$TMPDIR_BASE/claude6.env"
MISSING_WF_DIR="$TMPDIR_BASE/nonexistent-wf-$$"
# Do NOT mkdir

T_D1_6_EXIT=0
echo "{\"session_id\":\"$SID6\"}" \
  | CLAUDE_WORKFLOW_DIR="$MISSING_WF_DIR" CLAUDE_ENV_FILE="$ENV_FILE6" \
    node "$SESSION_START" 2>/dev/null || T_D1_6_EXIT=$?

if [ "$T_D1_6_EXIT" = "0" ]; then
    pass "T-D1-6. missing workflow dir → exit 0 (no crash)"
else
    fail "T-D1-6. missing workflow dir → non-zero exit: $T_D1_6_EXIT"
fi

# ---------------------------------------------------------------------------
# T-D1-7: CLAUDE_WORKFLOW_DIR override → state written to override path
# ---------------------------------------------------------------------------

echo ""
echo "=== session-start: T-D1-7 — CLAUDE_WORKFLOW_DIR override ==="

SID7="tdd-d1-7-$(printf '%04x%04x' $RANDOM $RANDOM)"
ENV_FILE7="$TMPDIR_BASE/claude7.env"
OVERRIDE_DIR="$TMPDIR_BASE/override-wf-$$"
mkdir -p "$OVERRIDE_DIR"

echo "{\"session_id\":\"$SID7\"}" \
  | CLAUDE_WORKFLOW_DIR="$OVERRIDE_DIR" CLAUDE_ENV_FILE="$ENV_FILE7" \
    node "$SESSION_START" 2>/dev/null || true

STATE_FILE7="$OVERRIDE_DIR/${SID7}.json"
if [ -f "$STATE_FILE7" ]; then
    pass "T-D1-7. CLAUDE_WORKFLOW_DIR override → state created at override path"
else
    fail "T-D1-7. CLAUDE_WORKFLOW_DIR override → state NOT found at $STATE_FILE7"
fi

# ---------------------------------------------------------------------------
# T-MIG-1: migration block deletes old .git/workflow/ files
# ---------------------------------------------------------------------------

echo ""
echo "=== session-start: T-MIG-1 — old .git/workflow/ migration ==="

MIG_REPO=$(setup_repo)
MIG_SID="mig-1-$(printf '%04x%04x' $RANDOM $RANDOM)"
MIG_ENV="$TMPDIR_BASE/mig1.env"

# Pre-place old-style state file in .git/workflow/
MIG_GITDIR=$(git -C "$MIG_REPO" rev-parse --git-dir)
mkdir -p "$MIG_REPO/$MIG_GITDIR/workflow"
printf '{"version":1,"session_id":"%s","created_at":"2026-01-01T00:00:00.000Z","steps":{}}' "$MIG_SID" \
  > "$MIG_REPO/$MIG_GITDIR/workflow/${MIG_SID}.json"

echo "{\"session_id\":\"$MIG_SID\"}" \
  | CLAUDE_PROJECT_DIR="$MIG_REPO" CLAUDE_ENV_FILE="$MIG_ENV" \
    CLAUDE_WORKFLOW_DIR="$WORKFLOW_DIR" node "$SESSION_START" 2>/dev/null || true

OLD_FILE="$MIG_REPO/$MIG_GITDIR/workflow/${MIG_SID}.json"
if [ ! -f "$OLD_FILE" ]; then
    pass "T-MIG-1. old .git/workflow/ state file deleted by migration"
else
    fail "T-MIG-1. old .git/workflow/ state file still exists: $OLD_FILE"
fi

# ---------------------------------------------------------------------------
# T-MIG-2: migration block is idempotent (second run → no crash)
# ---------------------------------------------------------------------------

echo ""
echo "=== session-start: T-MIG-2 — migration idempotency ==="

MIG2_REPO=$(setup_repo)
MIG2_SID="mig-2-$(printf '%04x%04x' $RANDOM $RANDOM)"
MIG2_ENV="$TMPDIR_BASE/mig2.env"

# Run twice — second time there is no old file
for i in 1 2; do
    T_MIG2_EXIT=0
    echo "{\"session_id\":\"$MIG2_SID\"}" \
      | CLAUDE_PROJECT_DIR="$MIG2_REPO" CLAUDE_ENV_FILE="$MIG2_ENV" \
        CLAUDE_WORKFLOW_DIR="$WORKFLOW_DIR" node "$SESSION_START" 2>/dev/null \
      || T_MIG2_EXIT=$?
    if [ "$T_MIG2_EXIT" != "0" ]; then
        fail "T-MIG-2. session-start run $i failed with exit $T_MIG2_EXIT"
        break
    fi
done
pass "T-MIG-2. migration block idempotent (two runs → exit 0)"

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
