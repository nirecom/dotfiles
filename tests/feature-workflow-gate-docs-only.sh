#!/bin/bash
# Test suite for workflow-gate.js docs-only shortcut:
#   isDocsOnlyStaged(repoDir) — returns true only when ALL staged files are docs/*.md
#   docs-only bypass logic — skips all steps except user_verification
set -euo pipefail

DOTFILES_DIR="$(cd "$(dirname "$0")/.." && pwd)"
# On Windows Git Bash, pwd returns /c/git/... style paths. node -e cannot use those
# directly in require(). We use a temp .js file with path.resolve() to work around this.
ERRORS=0

fail() { echo "FAIL: $1"; ERRORS=$((ERRORS + 1)); }
pass() { echo "PASS: $1"; }

# Portable timeout wrapper (macOS has no timeout command)
run_with_timeout() {
  if command -v timeout >/dev/null 2>&1; then
    timeout 180 "$@"
  else
    perl -e 'alarm 180; exec @ARGV' -- "$@"
  fi
}

# ---------------------------------------------------------------------------
# Temporary directories
# ---------------------------------------------------------------------------

TMPDIR="$(mktemp -d)"
export CLAUDE_WORKFLOW_DIR="$TMPDIR/workflow"
mkdir -p "$TMPDIR/workflow"
trap "rm -rf \"\$TMPDIR\"" EXIT

# ---------------------------------------------------------------------------
# Node helper script: calls isDocsOnlyStaged via path.resolve (Windows-safe)
# ---------------------------------------------------------------------------

IS_DOCS_ONLY_SCRIPT="$TMPDIR/is-docs-only.js"
cat > "$IS_DOCS_ONLY_SCRIPT" << 'JSEOF'
const path = require('path');
const hookPath = path.resolve(process.argv[2]);
const repoDir  = process.argv[3];
let result;
try {
  const m = require(hookPath);
  if (typeof m.isDocsOnlyStaged !== 'function') {
    process.stderr.write('isDocsOnlyStaged is not exported from ' + hookPath + '\n');
    process.stdout.write('MISSING');
    process.exit(0);
  }
  result = m.isDocsOnlyStaged(repoDir);
} catch (e) {
  process.stderr.write('isDocsOnlyStaged call failed: ' + e.message + '\n');
  process.stdout.write('ERROR');
  process.exit(0);
}
process.stdout.write(String(result));
JSEOF

# ---------------------------------------------------------------------------
# Setup a bare git repo for Section A unit tests
# ---------------------------------------------------------------------------

REPO="$(mktemp -d)"
git -C "$REPO" init -q
git -C "$REPO" config user.email "test@example.com"
git -C "$REPO" config user.name "Test"
git -C "$REPO" commit --allow-empty -q -m "init"

# Helper: call isDocsOnlyStaged for REPO via the helper script
call_is_docs_only() {
  node "$IS_DOCS_ONLY_SCRIPT" "$DOTFILES_DIR/claude-global/hooks/workflow-gate.js" "$REPO"
}

# Helper: call isDocsOnlyStaged for an arbitrary path
call_is_docs_only_dir() {
  local dir="$1"
  node "$IS_DOCS_ONLY_SCRIPT" "$DOTFILES_DIR/claude-global/hooks/workflow-gate.js" "$dir"
}

# Helper: reset staged changes in REPO
reset_staged() {
  git -C "$REPO" reset HEAD -- . 2>/dev/null || true
  # Remove any untracked files we created in the prior test
  git -C "$REPO" clean -fdq 2>/dev/null || true
}

# ---------------------------------------------------------------------------
# Section A: isDocsOnlyStaged unit tests (broad integration: real git repo)
# ---------------------------------------------------------------------------

echo ""
echo "=== Section A: isDocsOnlyStaged unit tests ==="

# A1: single docs/*.md file → true
reset_staged
mkdir -p "$REPO/docs"
echo "x" > "$REPO/docs/todo.md"
git -C "$REPO" add docs/todo.md
result=$(call_is_docs_only)
if [ "$result" = "true" ]; then pass "A1: single docs/todo.md → true"; else fail "A1: single docs/todo.md — expected true, got '$result'"; fi

# A2: multiple docs/*.md files → true
reset_staged
mkdir -p "$REPO/docs"
echo "x" > "$REPO/docs/foo.md"
echo "x" > "$REPO/docs/bar.md"
git -C "$REPO" add docs/foo.md docs/bar.md
result=$(call_is_docs_only)
if [ "$result" = "true" ]; then pass "A2: docs/foo.md + docs/bar.md → true"; else fail "A2: docs/foo.md + docs/bar.md — expected true, got '$result'"; fi

# A3: nested docs/history/2026.md → true
reset_staged
mkdir -p "$REPO/docs/history"
echo "x" > "$REPO/docs/history/2026.md"
git -C "$REPO" add docs/history/2026.md
result=$(call_is_docs_only)
if [ "$result" = "true" ]; then pass "A3: nested docs/history/2026.md → true"; else fail "A3: nested docs/history/2026.md — expected true, got '$result'"; fi

# A4: docs/foo.md + src/bar.js mixed → false
reset_staged
mkdir -p "$REPO/docs" "$REPO/src"
echo "x" > "$REPO/docs/foo.md"
echo "x" > "$REPO/src/bar.js"
git -C "$REPO" add docs/foo.md src/bar.js
result=$(call_is_docs_only)
if [ "$result" = "false" ]; then pass "A4: docs/foo.md + src/bar.js mixed → false"; else fail "A4: docs/foo.md + src/bar.js mixed — expected false, got '$result'"; fi

# A5: README.md (root level) → false
reset_staged
echo "x" > "$REPO/README.md"
git -C "$REPO" add README.md
result=$(call_is_docs_only)
if [ "$result" = "false" ]; then pass "A5: README.md at root → false"; else fail "A5: README.md at root — expected false, got '$result'"; fi

# A6: CLAUDE.md (root level) → false
reset_staged
echo "x" > "$REPO/CLAUDE.md"
git -C "$REPO" add CLAUDE.md
result=$(call_is_docs_only)
if [ "$result" = "false" ]; then pass "A6: CLAUDE.md at root → false"; else fail "A6: CLAUDE.md at root — expected false, got '$result'"; fi

# A7: claude-global/skills/foo/SKILL.md → false
reset_staged
mkdir -p "$REPO/claude-global/skills/foo"
echo "x" > "$REPO/claude-global/skills/foo/SKILL.md"
git -C "$REPO" add claude-global/skills/foo/SKILL.md
result=$(call_is_docs_only)
if [ "$result" = "false" ]; then pass "A7: claude-global/skills/foo/SKILL.md → false"; else fail "A7: claude-global/skills/foo/SKILL.md — expected false, got '$result'"; fi

# A8: empty staged (nothing staged) → false
reset_staged
result=$(call_is_docs_only)
if [ "$result" = "false" ]; then pass "A8: empty staged → false"; else fail "A8: empty staged — expected false, got '$result'"; fi

# A9: docs/diagram.svg only (non-.md extension) → false
reset_staged
mkdir -p "$REPO/docs"
echo "x" > "$REPO/docs/diagram.svg"
git -C "$REPO" add docs/diagram.svg
result=$(call_is_docs_only)
if [ "$result" = "false" ]; then pass "A9: docs/diagram.svg → false (extension check)"; else fail "A9: docs/diagram.svg — expected false, got '$result'"; fi

# A10: non-git repo (/tmp/notgit) → false (fail-safe)
result=$(call_is_docs_only_dir "/tmp/notgit")
if [ "$result" = "false" ]; then pass "A10: non-git repo → false (fail-safe)"; else fail "A10: non-git repo — expected false, got '$result'"; fi

# A11: idempotency — docs/todo.md staged, call twice → both true and equal
reset_staged
mkdir -p "$REPO/docs"
echo "x" > "$REPO/docs/todo.md"
git -C "$REPO" add docs/todo.md
result1=$(call_is_docs_only)
result2=$(call_is_docs_only)
if [ "$result1" = "true" ] && [ "$result2" = "true" ] && [ "$result1" = "$result2" ]; then
  pass "A11 idempotency: docs/todo.md staged, 2 calls → both true and equal"
else
  fail "A11 idempotency: expected true+true, got '$result1' and '$result2'"
fi

# ---------------------------------------------------------------------------
# Section B: workflow-gate full broad integration
# ---------------------------------------------------------------------------

echo ""
echo "=== Section B: workflow-gate broad integration (docs-only bypass) ==="

GATE_HOOK="$DOTFILES_DIR/claude-global/hooks/workflow-gate.js"

# Helper: write workflow state file (user_verification has given status, all others complete)
make_state() {
  local sid="$1"
  local user_ver_status="$2"
  cat > "$TMPDIR/workflow/${sid}.json" << EOF
{
  "version": 1,
  "session_id": "$sid",
  "created_at": "2026-01-01T00:00:00.000Z",
  "steps": {
    "research":         { "status": "complete",        "updated_at": "2026-01-01T00:00:00.000Z" },
    "plan":             { "status": "complete",        "updated_at": "2026-01-01T00:00:00.000Z" },
    "write_tests":      { "status": "complete",        "updated_at": "2026-01-01T00:00:00.000Z" },
    "run_tests":        { "status": "complete",        "updated_at": "2026-01-01T00:00:00.000Z" },
    "review_security":  { "status": "complete",        "updated_at": "2026-01-01T00:00:00.000Z" },
    "docs":             { "status": "complete",        "updated_at": "2026-01-01T00:00:00.000Z" },
    "user_verification":{ "status": "$user_ver_status","updated_at": "2026-01-01T00:00:00.000Z" }
  }
}
EOF
}

# Helper: write workflow state file with all steps having the same status
make_state_all() {
  local sid="$1"
  local all_status="$2"
  cat > "$TMPDIR/workflow/${sid}.json" << EOF
{
  "version": 1,
  "session_id": "$sid",
  "created_at": "2026-01-01T00:00:00.000Z",
  "steps": {
    "research":         { "status": "$all_status", "updated_at": "2026-01-01T00:00:00.000Z" },
    "plan":             { "status": "$all_status", "updated_at": "2026-01-01T00:00:00.000Z" },
    "write_tests":      { "status": "$all_status", "updated_at": "2026-01-01T00:00:00.000Z" },
    "run_tests":        { "status": "$all_status", "updated_at": "2026-01-01T00:00:00.000Z" },
    "review_security":  { "status": "$all_status", "updated_at": "2026-01-01T00:00:00.000Z" },
    "docs":             { "status": "$all_status", "updated_at": "2026-01-01T00:00:00.000Z" },
    "user_verification":{ "status": "$all_status", "updated_at": "2026-01-01T00:00:00.000Z" }
  }
}
EOF
}

# Helper: run workflow-gate with git commit command in given REPO
run_gate() {
  local sid="$1"
  local repo="$2"
  printf '{"tool_name":"Bash","tool_input":{"command":"git commit -m test"},"session_id":"%s"}' "$sid" \
    | CLAUDE_PROJECT_DIR="$repo" node "$GATE_HOOK"
}

# Helper: extract decision from gate JSON output
get_decision() {
  local output="$1"
  printf '%s' "$output" | node -e "let d='';process.stdin.on('data',c=>d+=c).on('end',()=>console.log(JSON.parse(d).decision))"
}

# Helper: extract reason from gate JSON output
get_reason() {
  local output="$1"
  printf '%s' "$output" | node -e "let d='';process.stdin.on('data',c=>d+=c).on('end',()=>console.log(JSON.parse(d).reason||''))"
}

# Create a fresh git repo for Section B tests
B_REPO="$(mktemp -d)"
git -C "$B_REPO" init -q
git -C "$B_REPO" config user.email "test@example.com"
git -C "$B_REPO" config user.name "Test"
git -C "$B_REPO" commit --allow-empty -q -m "init"

# Reset B_REPO staged state
reset_b_staged() {
  git -C "$B_REPO" reset HEAD -- . 2>/dev/null || true
  git -C "$B_REPO" clean -fdq 2>/dev/null || true
}

# B1: docs/todo.md staged, user_verification=complete, all others complete → approve
reset_b_staged
SID_B1="b1-session-$$"
make_state "$SID_B1" "complete"
mkdir -p "$B_REPO/docs"
echo "x" > "$B_REPO/docs/todo.md"
git -C "$B_REPO" add docs/todo.md
output=$(run_gate "$SID_B1" "$B_REPO")
decision=$(get_decision "$output")
if [ "$decision" = "approve" ]; then pass "B1: docs-only + all complete → approve"; else fail "B1: docs-only + all complete — expected approve, got '$decision'"; fi

# B2: docs/todo.md staged, ALL pending → block with docs-only message
reset_b_staged
SID_B2="b2-session-$$"
make_state_all "$SID_B2" "pending"
mkdir -p "$B_REPO/docs"
echo "x" > "$B_REPO/docs/todo.md"
git -C "$B_REPO" add docs/todo.md
output=$(run_gate "$SID_B2" "$B_REPO")
decision=$(get_decision "$output")
reason=$(get_reason "$output")
if [ "$decision" = "block" ]; then
  if echo "$reason" | grep -q "docs-only commit"; then
    pass "B2: docs-only + all pending → block with docs-only message"
  else
    fail "B2: docs-only + all pending — blocked but reason missing 'docs-only commit': '$reason'"
  fi
else
  fail "B2: docs-only + all pending — expected block, got '$decision'"
fi

# B3: docs/todo.md staged, user_verification=pending, all others complete → block with docs-only message
reset_b_staged
SID_B3="b3-session-$$"
make_state "$SID_B3" "pending"
mkdir -p "$B_REPO/docs"
echo "x" > "$B_REPO/docs/todo.md"
git -C "$B_REPO" add docs/todo.md
output=$(run_gate "$SID_B3" "$B_REPO")
decision=$(get_decision "$output")
reason=$(get_reason "$output")
if [ "$decision" = "block" ]; then
  if echo "$reason" | grep -q "docs-only commit"; then
    pass "B3: docs-only + user_verification pending → block with docs-only message"
  else
    fail "B3: docs-only + user_verification pending — blocked but reason missing 'docs-only commit': '$reason'"
  fi
else
  fail "B3: docs-only + user_verification pending — expected block, got '$decision'"
fi

# B4: docs/todo.md + src/bar.js mixed, all complete → approve
reset_b_staged
SID_B4="b4-session-$$"
make_state_all "$SID_B4" "complete"
mkdir -p "$B_REPO/docs" "$B_REPO/src"
echo "x" > "$B_REPO/docs/todo.md"
echo "x" > "$B_REPO/src/bar.js"
git -C "$B_REPO" add docs/todo.md src/bar.js
output=$(run_gate "$SID_B4" "$B_REPO")
decision=$(get_decision "$output")
if [ "$decision" = "approve" ]; then pass "B4: mixed staged + all complete → approve"; else fail "B4: mixed staged + all complete — expected approve, got '$decision'"; fi

# B5: docs/todo.md + src/bar.js mixed, run_tests=pending, others complete → block mentioning run_tests
reset_b_staged
SID_B5="b5-session-$$"
cat > "$TMPDIR/workflow/${SID_B5}.json" << EOF
{
  "version": 1,
  "session_id": "$SID_B5",
  "created_at": "2026-01-01T00:00:00.000Z",
  "steps": {
    "research":         { "status": "complete", "updated_at": "2026-01-01T00:00:00.000Z" },
    "plan":             { "status": "complete", "updated_at": "2026-01-01T00:00:00.000Z" },
    "write_tests":      { "status": "complete", "updated_at": "2026-01-01T00:00:00.000Z" },
    "run_tests":        { "status": "pending",  "updated_at": "2026-01-01T00:00:00.000Z" },
    "review_security":  { "status": "complete", "updated_at": "2026-01-01T00:00:00.000Z" },
    "docs":             { "status": "complete", "updated_at": "2026-01-01T00:00:00.000Z" },
    "user_verification":{ "status": "complete", "updated_at": "2026-01-01T00:00:00.000Z" }
  }
}
EOF
mkdir -p "$B_REPO/docs" "$B_REPO/src"
echo "x" > "$B_REPO/docs/todo.md"
echo "x" > "$B_REPO/src/bar.js"
git -C "$B_REPO" add docs/todo.md src/bar.js
output=$(run_gate "$SID_B5" "$B_REPO")
decision=$(get_decision "$output")
reason=$(get_reason "$output")
if [ "$decision" = "block" ]; then
  if echo "$reason" | grep -q "run_tests"; then
    pass "B5: mixed + run_tests pending → block mentioning run_tests"
  else
    fail "B5: mixed + run_tests pending — blocked but reason missing 'run_tests': '$reason'"
  fi
else
  fail "B5: mixed + run_tests pending — expected block, got '$decision'"
fi

# B6: empty staged, all pending → block (docs-only bypass should NOT trigger)
reset_b_staged
SID_B6="b6-session-$$"
make_state_all "$SID_B6" "pending"
output=$(run_gate "$SID_B6" "$B_REPO")
decision=$(get_decision "$output")
reason=$(get_reason "$output")
if [ "$decision" = "block" ]; then
  # Normal workflow block should list multiple steps (e.g. research)
  if echo "$reason" | grep -q "research"; then
    pass "B6: empty staged + all pending → block (no docs-only bypass)"
  else
    fail "B6: empty staged + all pending — blocked but unexpected reason (no 'research'): '$reason'"
  fi
else
  fail "B6: empty staged + all pending — expected block, got '$decision'"
fi

# B7: idempotency — docs/todo.md staged, user_verification=complete; 2 calls → both approve, state unchanged
reset_b_staged
SID_B7="b7-session-$$"
make_state "$SID_B7" "complete"
mkdir -p "$B_REPO/docs"
echo "x" > "$B_REPO/docs/todo.md"
git -C "$B_REPO" add docs/todo.md
output1=$(run_gate "$SID_B7" "$B_REPO")
decision1=$(get_decision "$output1")
state_before=$(cat "$TMPDIR/workflow/${SID_B7}.json")
output2=$(run_gate "$SID_B7" "$B_REPO")
decision2=$(get_decision "$output2")
state_after=$(cat "$TMPDIR/workflow/${SID_B7}.json")
if [ "$decision1" = "approve" ] && [ "$decision2" = "approve" ]; then
  if [ "$state_before" = "$state_after" ]; then
    pass "B7 idempotency: 2x approve, state file unchanged"
  else
    fail "B7 idempotency: both approve but state file changed between calls"
  fi
else
  fail "B7 idempotency: expected approve+approve, got '$decision1' and '$decision2'"
fi

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------

echo ""
if [ "$ERRORS" -eq 0 ]; then
  echo "All tests passed."
  exit 0
else
  echo "$ERRORS test(s) failed."
  exit 1
fi
