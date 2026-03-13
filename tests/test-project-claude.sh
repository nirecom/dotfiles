#!/bin/bash
# tests/test-project-claude.sh — project CLAUDE.md reorganization verification
set -euo pipefail

DOTFILES_DIR="$(cd "$(dirname "$0")/.." && pwd)"
CLAUDE_MD="$DOTFILES_DIR/CLAUDE.md"
RULES_DIR="$DOTFILES_DIR/.claude/rules"
ERRORS=0

fail() { echo "FAIL: $1"; ERRORS=$((ERRORS + 1)); }
pass() { echo "PASS: $1"; }

# --- File structure ---

# CLAUDE.md is slim (10 lines or fewer)
LINE_COUNT=$(wc -l < "$CLAUDE_MD")
[ "$LINE_COUNT" -le 10 ] && pass "CLAUDE.md is slim ($LINE_COUNT lines)" || fail "CLAUDE.md too long ($LINE_COUNT lines)"

# .claude/rules/patterns.md exists
[ -f "$RULES_DIR/patterns.md" ] && pass "rules/patterns.md exists" || fail "rules/patterns.md missing"

# .claude/rules/patterns.md is non-empty
[ -s "$RULES_DIR/patterns.md" ] && pass "rules/patterns.md is non-empty" || fail "rules/patterns.md is empty"

# --- CLAUDE.md content ---

# CLAUDE.md references docs/
grep -q "docs/" "$CLAUDE_MD" && pass "CLAUDE.md references docs/" || fail "CLAUDE.md missing docs/ reference"

# CLAUDE.md references .claude/rules/
grep -q '\.claude/rules/' "$CLAUDE_MD" && pass "CLAUDE.md references .claude/rules/" || fail "CLAUDE.md missing .claude/rules/ reference"

# CLAUDE.md has no Installation code block (delegated to README.md)
if grep -q '```bash' "$CLAUDE_MD" || grep -q '```powershell' "$CLAUDE_MD"; then
  fail "CLAUDE.md still contains Installation code blocks"
else
  pass "CLAUDE.md has no Installation code blocks"
fi

# CLAUDE.md has no Shell Configuration section (delegated to docs/)
if grep -q '### Shell Configuration' "$CLAUDE_MD"; then
  fail "CLAUDE.md still contains Shell Configuration section"
else
  pass "CLAUDE.md has no Shell Configuration section"
fi

# --- rules/patterns.md content ---

grep -q "Defensive loading" "$RULES_DIR/patterns.md" && pass "patterns.md has Defensive loading" || fail "patterns.md missing Defensive loading"
grep -qE 'Cross-platform|\$OSDIST' "$RULES_DIR/patterns.md" && pass "patterns.md has Cross-platform/OSDIST" || fail "patterns.md missing Cross-platform/OSDIST"

# --- Delegated info exists in canonical locations ---

grep -q "install.sh" "$DOTFILES_DIR/README.md" && pass "README.md has install.sh" || fail "README.md missing install.sh"
grep -q "Shell Configuration" "$DOTFILES_DIR/docs/architecture.md" && pass "docs/architecture.md has Shell Configuration" || fail "docs/architecture.md missing Shell Configuration"
grep -q "Defensive loading" "$DOTFILES_DIR/docs/architecture.md" && pass "docs/architecture.md has Defensive loading" || fail "docs/architecture.md missing Defensive loading"

# --- Results ---
echo ""
if [ "$ERRORS" -eq 0 ]; then
  echo "All tests passed!"
else
  echo "$ERRORS test(s) failed"
  exit 1
fi
