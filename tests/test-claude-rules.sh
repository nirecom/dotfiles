#!/bin/bash
# tests/test-claude-rules.sh — claude-global rules reorganization verification
set -euo pipefail

DOTFILES_DIR="$(cd "$(dirname "$0")/.." && pwd)"
CLAUDE_DIR="$DOTFILES_DIR/claude-global"
ERRORS=0

fail() { echo "FAIL: $1"; ERRORS=$((ERRORS + 1)); }
pass() { echo "PASS: $1"; }

# --- File structure ---

# rules/ has exactly 6 files
RULES_COUNT=$(ls "$CLAUDE_DIR"/rules/*.md 2>/dev/null | wc -l)
[ "$RULES_COUNT" -eq 6 ] && pass "rules/ has 6 files" || fail "rules/ has $RULES_COUNT files (expected 6)"

# Expected rule files exist
for f in coding shell-commands git workflow privacy docs-lifecycle; do
  [ -f "$CLAUDE_DIR/rules/$f.md" ] && pass "rules/$f.md exists" || fail "rules/$f.md missing"
done

# Expected command files exist
for f in update-docs start-task complete-task update-instruction; do
  [ -f "$CLAUDE_DIR/commands/$f.md" ] && pass "commands/$f.md exists" || fail "commands/$f.md missing"
done

# Deleted files must not exist
for f in update-dotfiles-docs update-langchain-docs update-langchain-architecture \
         update-langchain-progress update-langchain-ops start-langchain-task complete-langchain-task; do
  [ ! -f "$CLAUDE_DIR/commands/$f.md" ] && pass "commands/$f.md deleted" || fail "commands/$f.md still exists"
done

# CLAUDE.md is pointer-only (10 lines or fewer)
LINE_COUNT=$(wc -l < "$CLAUDE_DIR/CLAUDE.md")
[ "$LINE_COUNT" -le 10 ] && pass "CLAUDE.md is slim ($LINE_COUNT lines)" || fail "CLAUDE.md too long ($LINE_COUNT lines)"

# CLAUDE.md contains no rule content
if grep -qiE '(curl\.exe|git -C|Co-Authored|\.context-private|pre-commit)' "$CLAUDE_DIR/CLAUDE.md"; then
  fail "CLAUDE.md still contains rule content"
else
  pass "CLAUDE.md has no rule content"
fi

# symlink: ~/.claude/rules/ is accessible with .md files
if [ -d "$HOME/.claude/rules" ] && [ "$(ls "$HOME/.claude/rules/"*.md 2>/dev/null | wc -l)" -ge 1 ]; then
  pass "~/.claude/rules/ accessible via symlink"
else
  fail "~/.claude/rules/ not accessible (check symlink)"
fi

# --- Content verification ---

# rules/coding.md has Co-Authored-By rule
grep -q "Co-Authored" "$CLAUDE_DIR/rules/coding.md" && pass "coding.md has Co-Authored rule" || fail "coding.md missing Co-Authored rule"

# rules/docs-lifecycle.md has LangChain path
grep -q "ai-specs" "$CLAUDE_DIR/rules/docs-lifecycle.md" && pass "docs-lifecycle.md has LangChain path" || fail "docs-lifecycle.md missing LangChain path"

# commands/update-docs.md has project detection
grep -qi "langchain\|project detect\|プロジェクト" "$CLAUDE_DIR/commands/update-docs.md" && pass "update-docs.md has project detection" || fail "update-docs.md missing project detection"

# commands/start-task.md is generic (not langchain-specific in title)
if grep -qi "^#.*langchain" "$CLAUDE_DIR/commands/start-task.md"; then
  fail "start-task.md is still langchain-specific"
else
  pass "start-task.md is generic"
fi

# --- Abnormal case checks ---

# No non-.md files in rules/ (Claude Code only reads .md)
NON_MD=$(find "$CLAUDE_DIR/rules/" -type f ! -name "*.md" ! -name ".gitkeep" 2>/dev/null | wc -l)
[ "$NON_MD" -eq 0 ] && pass "rules/ has no non-.md files" || fail "rules/ has $NON_MD unexpected files"

# Rule files are non-empty
for f in "$CLAUDE_DIR"/rules/*.md; do
  [ -s "$f" ] && pass "$(basename "$f") is non-empty" || fail "$(basename "$f") is empty"
done

# Command files are non-empty
for f in update-docs start-task complete-task; do
  [ -s "$CLAUDE_DIR/commands/$f.md" ] && pass "commands/$f.md is non-empty" || fail "commands/$f.md is empty"
done

# No duplicate content across rule files (same non-trivial line in 2+ files)
DUPES=$(cat "$CLAUDE_DIR"/rules/*.md | grep -v '^$' | grep -v '^#' | grep -v '^-' | sort | uniq -d | wc -l)
[ "$DUPES" -eq 0 ] && pass "no duplicate content across rules" || fail "$DUPES duplicate lines across rule files"

# CLAUDE.md references rules/ and commands/ (functioning as pointer)
grep -q "rules/" "$CLAUDE_DIR/CLAUDE.md" && pass "CLAUDE.md references rules/" || fail "CLAUDE.md missing rules/ reference"
grep -q "commands/" "$CLAUDE_DIR/CLAUDE.md" && pass "CLAUDE.md references commands/" || fail "CLAUDE.md missing commands/ reference"

# --- Results ---
echo ""
if [ "$ERRORS" -eq 0 ]; then
  echo "All tests passed!"
else
  echo "$ERRORS test(s) failed"
  exit 1
fi
