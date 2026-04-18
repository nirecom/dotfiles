#!/usr/bin/env bash
# Contract tests for make-plan research escalation feature
# Target files (expected to FAIL until implementation is complete):
#   $HOME/.claude/skills/make-plan/SKILL.md
#   $HOME/.claude/agents/planner.md
#   $HOME/.claude/agents/reviewer.md
# Exit 0 always — this is a contract test, not a CI gate yet.

# Timeout guard: if running without the sentinel, re-exec under timeout
if [ -z "$_TIMEOUT_WRAPPED" ]; then
    export _TIMEOUT_WRAPPED=1
    if command -v timeout >/dev/null 2>&1; then
        exec timeout 120 bash "$0" "$@"
    else
        exec perl -e 'alarm 120; exec @ARGV' -- bash "$0" "$@"
    fi
fi

SKILL_MD="$HOME/.claude/skills/make-plan/SKILL.md"
PLANNER_MD="$HOME/.claude/agents/planner.md"
REVIEWER_MD="$HOME/.claude/agents/reviewer.md"

PASS=0
FAIL=0

pass() {
    echo "PASS: $1"
    PASS=$((PASS + 1))
}

fail() {
    echo "FAIL: $1"
    FAIL=$((FAIL + 1))
}

# assert_contains FILE PATTERN DESCRIPTION
# Greps FILE for PATTERN (extended regex). Prints PASS/FAIL.
assert_contains() {
    local file="$1"
    local pattern="$2"
    local desc="$3"

    if [ ! -f "$file" ]; then
        fail "$desc (file not found: $file)"
        return 1
    fi

    if grep -qE "$pattern" "$file"; then
        pass "$desc"
        return 0
    else
        fail "$desc (pattern not found: $pattern)"
        return 1
    fi
}

echo "=== make-plan research escalation contract tests ==="
echo ""

# ---------------------------------------------------------------------------
# Normal cases
# ---------------------------------------------------------------------------
echo "--- Normal ---"

# N1: SKILL_MD checks
assert_contains "$SKILL_MD" "NEEDS_RESEARCH" \
    "N1a: SKILL_MD contains literal NEEDS_RESEARCH"

assert_contains "$SKILL_MD" "skill:" \
    "N1b: SKILL_MD contains 'skill:'"

assert_contains "$SKILL_MD" "question:" \
    "N1c: SKILL_MD contains 'question:'"

assert_contains "$SKILL_MD" "reason:" \
    "N1d: SKILL_MD contains 'reason:'"

assert_contains "$SKILL_MD" "deep-research" \
    "N1e: SKILL_MD contains 'deep-research'"

assert_contains "$SKILL_MD" "Research complete" \
    "N1f: SKILL_MD contains 'Research complete'"

# N2: PLANNER_MD checks
assert_contains "$PLANNER_MD" "NEEDS_RESEARCH" \
    "N2a: PLANNER_MD contains NEEDS_RESEARCH"

assert_contains "$PLANNER_MD" '\[research:' \
    "N2b: PLANNER_MD contains '[research:'"

assert_contains "$PLANNER_MD" "Research Findings \(from this session\)" \
    "N2c: PLANNER_MD contains 'Research Findings (from this session)'"

# N3: Cross-file consistency checks

# N3a: all three files contain NEEDS_RESEARCH
if grep -qE "NEEDS_RESEARCH" "$SKILL_MD" 2>/dev/null && \
   grep -qE "NEEDS_RESEARCH" "$PLANNER_MD" 2>/dev/null && \
   grep -qE "NEEDS_RESEARCH" "$REVIEWER_MD" 2>/dev/null; then
    pass "N3a: all three files contain NEEDS_RESEARCH"
else
    fail "N3a: all three files contain NEEDS_RESEARCH"
fi

# N3b: 'skill: deep-research' in SKILL_MD and PLANNER_MD at minimum
if grep -qE "skill:[[:space:]]*deep-research" "$SKILL_MD" 2>/dev/null && \
   grep -qE "skill:[[:space:]]*deep-research" "$PLANNER_MD" 2>/dev/null; then
    pass "N3b: 'skill: deep-research' appears in SKILL_MD and PLANNER_MD"
else
    fail "N3b: 'skill: deep-research' must appear in SKILL_MD and PLANNER_MD"
fi

# N3c: both SKILL_MD and PLANNER_MD contain 'question:'
if grep -qE "question:" "$SKILL_MD" 2>/dev/null && \
   grep -qE "question:" "$PLANNER_MD" 2>/dev/null; then
    pass "N3c: SKILL_MD and PLANNER_MD both contain 'question:'"
else
    fail "N3c: SKILL_MD and PLANNER_MD both contain 'question:'"
fi

# N3d: both SKILL_MD and PLANNER_MD contain 'reason:'
if grep -qE "reason:" "$SKILL_MD" 2>/dev/null && \
   grep -qE "reason:" "$PLANNER_MD" 2>/dev/null; then
    pass "N3d: SKILL_MD and PLANNER_MD both contain 'reason:'"
else
    fail "N3d: SKILL_MD and PLANNER_MD both contain 'reason:'"
fi

# N3e: both PLANNER_MD and REVIEWER_MD contain '[research:'
if grep -qE '\[research:' "$PLANNER_MD" 2>/dev/null && \
   grep -qE '\[research:' "$REVIEWER_MD" 2>/dev/null; then
    pass "N3e: PLANNER_MD and REVIEWER_MD both contain '[research:'"
else
    fail "N3e: PLANNER_MD and REVIEWER_MD both contain '[research:'"
fi

# N3f: both PLANNER_MD and REVIEWER_MD contain 'Research Findings (from this session)'
if grep -qF "Research Findings (from this session)" "$PLANNER_MD" 2>/dev/null && \
   grep -qF "Research Findings (from this session)" "$REVIEWER_MD" 2>/dev/null; then
    pass "N3f: PLANNER_MD and REVIEWER_MD both contain 'Research Findings (from this session)'"
else
    fail "N3f: PLANNER_MD and REVIEWER_MD both contain 'Research Findings (from this session)'"
fi

echo ""
# ---------------------------------------------------------------------------
# Error cases
# ---------------------------------------------------------------------------
echo "--- Error ---"

# E1a: SKILL_MD handles empty question: (look for 'empty' or 'non-empty' near 'question')
assert_contains "$SKILL_MD" "(empty|non-empty).{0,60}question|question.{0,60}(empty|non-empty)" \
    "E1a: SKILL_MD contains handling for empty 'question:'"

# E1b: SKILL_MD handles whitespace-only reason: (look for 'whitespace' or 'non-empty' near 'reason')
assert_contains "$SKILL_MD" "(whitespace|non-empty).{0,60}reason|reason.{0,60}(whitespace|non-empty)" \
    "E1b: SKILL_MD contains handling for whitespace-only 'reason:'"

# E1c: SKILL_MD handles unsupported skill value
assert_contains "$SKILL_MD" "unsupported|v1" \
    "E1c: SKILL_MD contains handling for unsupported skill value"

# E2: SKILL_MD: malformed retry does not consume research_rounds
assert_contains "$SKILL_MD" "malformed.{0,120}research_rounds|research_rounds.{0,120}malformed" \
    "E2: SKILL_MD contains text indicating malformed retry does not consume research_rounds"

# E3: SKILL_MD: escalation message for budget exhaustion
assert_contains "$SKILL_MD" "(budget|escalat).{0,120}research|research.{0,120}(budget|escalat)" \
    "E3: SKILL_MD contains escalation message for budget exhaustion"

echo ""
# ---------------------------------------------------------------------------
# Edge cases
# ---------------------------------------------------------------------------
echo "--- Edge ---"

# Ed1a: SKILL_MD contains 'pending reviewer concerns'
assert_contains "$SKILL_MD" "pending reviewer concerns" \
    "Ed1a: SKILL_MD contains 'pending reviewer concerns'"

# Ed1b: SKILL_MD contains 'initial-draft'
assert_contains "$SKILL_MD" "initial-draft" \
    "Ed1b: SKILL_MD contains 'initial-draft'"

# Ed2: SKILL_MD: NEEDS_RESEARCH does not consume revision_rounds
assert_contains "$SKILL_MD" "revision_rounds" \
    "Ed2: SKILL_MD contains text about revision_rounds (NEEDS_RESEARCH does not consume it)"

# Ed3: SKILL_MD: plan_complete is not emitted on escalation
assert_contains "$SKILL_MD" "plan_complete" \
    "Ed3: SKILL_MD contains 'plan_complete' (contract: not emitted on escalation)"

# Ed4: SKILL_MD contains 'first non-whitespace'
assert_contains "$SKILL_MD" "first non-whitespace" \
    "Ed4: SKILL_MD contains 'first non-whitespace'"

echo ""
# ---------------------------------------------------------------------------
# Idempotency cases
# ---------------------------------------------------------------------------
echo "--- Idempotency ---"

# I1: SKILL_MD contains text that counters are not reset
assert_contains "$SKILL_MD" "reset" \
    "I1: SKILL_MD contains text that counters are not reset"

# I2: REVIEWER_MD contains a pattern matching [a-z0-9-] (kebab-case token in brackets)
assert_contains "$REVIEWER_MD" '\[[a-z0-9-]+\]' \
    "I2: REVIEWER_MD contains kebab-case token in brackets (e.g. [research:...])"

echo ""
echo "=== Summary ==="
echo "PASS: $PASS  FAIL: $FAIL"

exit 0
