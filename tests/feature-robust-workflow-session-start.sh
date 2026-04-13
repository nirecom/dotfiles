#!/bin/bash
# TDD tests for session-start.js initial state file creation (T-D1-1 through T-D1-5)
# Features NOT YET IMPLEMENTED — some tests are expected to FAIL.
set -euo pipefail

DOTFILES_DIR="$(cd "$(dirname "$0")/.." && pwd)"
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

get_state_file() {
    local repo="$1" sid="$2"
    local gitdir
    gitdir=$(git -C "$repo" rev-parse --git-dir)
    echo "$repo/$gitdir/workflow/${sid}.json"
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
# T-D1-1: session-start creates initial state file with all steps pending
# Expected FAIL before implementation (feature not yet implemented)
# ---------------------------------------------------------------------------

echo ""
echo "=== session-start: T-D1-1 — creates initial state file ==="

REPO=$(setup_repo)
SID="tdd1-$(printf '%04x%04x' $RANDOM $RANDOM)"
ENV_FILE="$TMPDIR_BASE/claude.env"

echo "{\"session_id\":\"$SID\"}" | CLAUDE_PROJECT_DIR="$REPO" CLAUDE_ENV_FILE="$ENV_FILE" node "$SESSION_START" 2>/dev/null || true

STATE_FILE=$(get_state_file "$REPO" "$SID")

if [ ! -f "$STATE_FILE" ]; then
    fail "T-D1-1. session-start did not create .git/workflow/$SID.json"
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
# Expected PASS before implementation (no-op: session-start doesn't write state yet)
# ---------------------------------------------------------------------------

echo ""
echo "=== session-start: T-D1-2 — does not overwrite existing state ==="

REPO2=$(setup_repo)
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
write_state "$REPO2" "$SID2" "$PRE_STATE"

echo "{\"session_id\":\"$SID2\"}" | CLAUDE_PROJECT_DIR="$REPO2" CLAUDE_ENV_FILE="$ENV_FILE2" node "$SESSION_START" 2>/dev/null || true

RESEARCH_STATUS=$(read_state_status "$REPO2" "$SID2" "research")
if [ "$RESEARCH_STATUS" = "complete" ]; then
    pass "T-D1-2. session-start did not overwrite existing state (research still complete)"
else
    fail "T-D1-2. session-start overwrote existing state — research.status=$RESEARCH_STATUS (expected complete)"
fi

# ---------------------------------------------------------------------------
# T-D1-3: session-start with no session_id creates no state file
# Expected PASS before implementation
# ---------------------------------------------------------------------------

echo ""
echo "=== session-start: T-D1-3 — no session_id → no state file ==="

REPO3=$(setup_repo)
ENV_FILE3="$TMPDIR_BASE/claude3.env"

echo '{}' | CLAUDE_PROJECT_DIR="$REPO3" CLAUDE_ENV_FILE="$ENV_FILE3" node "$SESSION_START" 2>/dev/null || true

if [ ! -d "$REPO3/.git/workflow" ] || [ -z "$(ls -A "$REPO3/.git/workflow/" 2>/dev/null)" ]; then
    pass "T-D1-3. no session_id → no .json file created in .git/workflow/"
else
    fail "T-D1-3. session-start created a state file even though session_id was absent"
fi

# ---------------------------------------------------------------------------
# T-D1-4: session-start without CLAUDE_PROJECT_DIR exits 0 (fail-safe)
# Expected PASS before implementation
# ---------------------------------------------------------------------------

echo ""
echo "=== session-start: T-D1-4 — no CLAUDE_PROJECT_DIR → exit 0 ==="

ENV_FILE4="$TMPDIR_BASE/claude4.env"

# Run from a temp dir (not a git repo) with no CLAUDE_PROJECT_DIR
T_D1_4_EXIT=0
env -u CLAUDE_PROJECT_DIR bash -c "
    cd \"$TMPDIR_BASE\" && echo '{\"session_id\":\"tdd4-noproj\"}' | CLAUDE_ENV_FILE=\"$ENV_FILE4\" node \"$SESSION_START\" >/dev/null 2>&1
" || T_D1_4_EXIT=$?

if [ "$T_D1_4_EXIT" = "0" ]; then
    pass "T-D1-4. no CLAUDE_PROJECT_DIR → exit code 0 (fail-safe)"
else
    fail "T-D1-4. no CLAUDE_PROJECT_DIR → non-zero exit code: $T_D1_4_EXIT"
fi

# ---------------------------------------------------------------------------
# T-D1-5: session-start with non-git CLAUDE_PROJECT_DIR → exit 0, no state
# Expected PASS before implementation
# ---------------------------------------------------------------------------

echo ""
echo "=== session-start: T-D1-5 — non-git CLAUDE_PROJECT_DIR → fail-safe ==="

NON_GIT_DIR="$TMPDIR_BASE/not-a-git-dir"
mkdir -p "$NON_GIT_DIR"
ENV_FILE5="$TMPDIR_BASE/claude5.env"

T_D1_5_EXIT=0
echo '{"session_id":"tdd5-nongit"}' | CLAUDE_PROJECT_DIR="$NON_GIT_DIR" CLAUDE_ENV_FILE="$ENV_FILE5" node "$SESSION_START" 2>/dev/null || T_D1_5_EXIT=$?

if [ "$T_D1_5_EXIT" = "0" ]; then
    pass "T-D1-5a. non-git CLAUDE_PROJECT_DIR → exit code 0 (fail-safe)"
else
    fail "T-D1-5a. non-git CLAUDE_PROJECT_DIR → non-zero exit code: $T_D1_5_EXIT"
fi

# No state file should be created anywhere in NON_GIT_DIR
if [ ! -d "$NON_GIT_DIR/.git" ] && [ -z "$(find "$NON_GIT_DIR" -name '*.json' 2>/dev/null | head -1)" ]; then
    pass "T-D1-5b. non-git dir → no state file created"
else
    fail "T-D1-5b. non-git dir → unexpected files created in $NON_GIT_DIR"
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
