#!/bin/bash
# tests/claude-rules.sh — verify rules and skills are well-formed and loadable
set -euo pipefail

DOTFILES_DIR="$(cd "$(dirname "$0")/.." && pwd)"
GLOBAL_DIR="$DOTFILES_DIR/claude-global"
PROJECT_RULES="$DOTFILES_DIR/.claude/rules"
ERRORS=0

fail() { echo "FAIL: $1"; ERRORS=$((ERRORS + 1)); }
pass() { echo "PASS: $1"; }

echo "=== Global Rules ==="

# rules/ has exactly 6 files
RULES_COUNT=$(ls "$GLOBAL_DIR"/rules/*.md 2>/dev/null | wc -l)
[ "$RULES_COUNT" -eq 6 ] && pass "rules/ has 6 files" || fail "rules/ has $RULES_COUNT files (expected 6)"

# Expected rule files exist
for f in coding shell-commands git workflow privacy docs-lifecycle; do
  [ -f "$GLOBAL_DIR/rules/$f.md" ] && pass "rules/$f.md exists" || fail "rules/$f.md missing"
done

# Every rule file must start with a markdown heading
for f in "$GLOBAL_DIR"/rules/*.md; do
  name=$(basename "$f")
  head -1 "$f" | grep -q '^# ' && pass "$name starts with heading" || fail "$name missing heading"
done

# Rules must be instructions, not procedures (no ## Procedure section)
for f in "$GLOBAL_DIR"/rules/*.md; do
  name=$(basename "$f")
  if grep -q '^## Procedure' "$f"; then
    fail "$name contains ## Procedure (should be a skill, not a rule)"
  else
    pass "$name has no procedure section"
  fi
done

# No non-.md files in rules/ (Claude Code only reads .md)
NON_MD=$(find "$GLOBAL_DIR/rules/" -type f ! -name "*.md" ! -name ".gitkeep" 2>/dev/null | wc -l)
[ "$NON_MD" -eq 0 ] && pass "rules/ has no non-.md files" || fail "rules/ has $NON_MD unexpected files"

# Rule files are non-empty
for f in "$GLOBAL_DIR"/rules/*.md; do
  name=$(basename "$f")
  [ -s "$f" ] && pass "$name is non-empty" || fail "$name is empty"
done

# No rule file exceeds 50 lines (rules should be concise)
for f in "$GLOBAL_DIR"/rules/*.md "$PROJECT_RULES"/*.md; do
  name=$(basename "$f")
  lines=$(wc -l < "$f")
  [ "$lines" -le 50 ] && pass "$name is concise ($lines lines)" || fail "$name too long ($lines lines, max 50)"
done

# No duplicate content across rule files (same non-trivial line in 2+ files)
DUPES=$(cat "$GLOBAL_DIR"/rules/*.md | grep -v '^$' | grep -v '^#' | grep -v '^-' | grep -v '^|' | grep -v '^```' | sort | uniq -d | wc -l)
[ "$DUPES" -eq 0 ] && pass "no duplicate content across rules" || fail "$DUPES duplicate lines across rule files"

# coding.md: key rules present
grep -q 'private information' "$GLOBAL_DIR/rules/coding.md" && pass "coding: private info rule" || fail "coding: missing private info rule"
grep -q '\.env' "$GLOBAL_DIR/rules/coding.md" && pass "coding: .env rule" || fail "coding: missing .env rule"
grep -q 'English' "$GLOBAL_DIR/rules/coding.md" && pass "coding: English rule" || fail "coding: missing English rule"
grep -q 'Co-Authored-By' "$GLOBAL_DIR/rules/coding.md" && pass "coding: Co-Authored-By rule" || fail "coding: missing Co-Authored-By rule"

# shell-commands.md: curl.exe and single-line rules
grep -q 'curl\.exe' "$GLOBAL_DIR/rules/shell-commands.md" && pass "shell: curl.exe rule" || fail "shell: missing curl.exe rule"
grep -q 'single line' "$GLOBAL_DIR/rules/shell-commands.md" && pass "shell: single line rule" || fail "shell: missing single line rule"

# git.md: git -C and separate calls rules
grep -q 'git -C' "$GLOBAL_DIR/rules/git.md" && pass "git: git -C rule" || fail "git: missing git -C rule"
grep -q 'separate.*sequential\|sequential.*separate' "$GLOBAL_DIR/rules/git.md" && pass "git: separate calls rule" || fail "git: missing separate calls rule"

# workflow.md: testing, diff, cross-platform rules
grep -q 'test' "$GLOBAL_DIR/rules/workflow.md" && pass "workflow: testing rule" || fail "workflow: missing testing rule"
grep -q 'diff' "$GLOBAL_DIR/rules/workflow.md" && pass "workflow: diff rule" || fail "workflow: missing diff rule"
grep -q 'Cross-Platform' "$GLOBAL_DIR/rules/workflow.md" && pass "workflow: cross-platform rule" || fail "workflow: missing cross-platform rule"

# privacy.md: .context-private rules
grep -q '\.context-private' "$GLOBAL_DIR/rules/privacy.md" && pass "privacy: .context-private rule" || fail "privacy: missing .context-private rule"
grep -q 'gitignore' "$GLOBAL_DIR/rules/privacy.md" && pass "privacy: gitignore mention" || fail "privacy: missing gitignore mention"

# docs-lifecycle.md: path resolution and update rules
grep -q 'architecture\.md' "$GLOBAL_DIR/rules/docs-lifecycle.md" && pass "docs-lifecycle: architecture.md" || fail "docs-lifecycle: missing architecture.md"
grep -q 'LangChain' "$GLOBAL_DIR/rules/docs-lifecycle.md" && pass "docs-lifecycle: LangChain path" || fail "docs-lifecycle: missing LangChain path"
grep -q 'gather.*propose.*confirm\|confirm.*apply' "$GLOBAL_DIR/rules/docs-lifecycle.md" && pass "docs-lifecycle: update cycle" || fail "docs-lifecycle: missing update cycle"

echo ""
echo "=== Global Skills (commands/) ==="

# Expected command files exist
for f in update-docs start-task complete-task update-instruction; do
  [ -f "$GLOBAL_DIR/commands/$f.md" ] && pass "commands/$f.md exists" || fail "commands/$f.md missing"
done

# Deleted files must not exist
for f in update-dotfiles-docs update-langchain-docs update-langchain-architecture \
         update-langchain-progress update-langchain-ops start-langchain-task complete-langchain-task; do
  [ ! -f "$GLOBAL_DIR/commands/$f.md" ] && pass "commands/$f.md deleted" || fail "commands/$f.md still exists"
done

# Every skill file must have a description on line 1 (plain text, not a heading)
for f in "$GLOBAL_DIR"/commands/*.md; do
  name=$(basename "$f")
  first_line=$(head -1 "$f")
  [ -n "$first_line" ] && pass "$name has description" || fail "$name missing description on line 1"
done

# Skills must have a ## Procedure section
for f in "$GLOBAL_DIR"/commands/*.md; do
  name=$(basename "$f")
  grep -q '^## Procedure' "$f" && pass "$name has procedure" || fail "$name missing ## Procedure"
done

# Skills must have a ## Rules section
for f in "$GLOBAL_DIR"/commands/*.md; do
  name=$(basename "$f")
  grep -q '^## Rules' "$f" && pass "$name has rules" || fail "$name missing ## Rules"
done

# Command files are non-empty
for f in update-docs start-task complete-task update-instruction; do
  [ -s "$GLOBAL_DIR/commands/$f.md" ] && pass "commands/$f.md is non-empty" || fail "commands/$f.md is empty"
done

# No skill file exceeds 100 lines
for f in "$GLOBAL_DIR"/commands/*.md; do
  name=$(basename "$f")
  lines=$(wc -l < "$f")
  [ "$lines" -le 100 ] && pass "$name is reasonable ($lines lines)" || fail "$name too long ($lines lines, max 100)"
done

# update-docs.md: project detection and both project types
grep -q 'LangChain' "$GLOBAL_DIR/commands/update-docs.md" && pass "update-docs: LangChain" || fail "update-docs: missing LangChain"
grep -q 'General projects' "$GLOBAL_DIR/commands/update-docs.md" && pass "update-docs: General projects" || fail "update-docs: missing General projects"

# start-task.md: generic (not langchain-specific)
first_line=$(head -1 "$GLOBAL_DIR/commands/start-task.md")
if echo "$first_line" | grep -qi 'langchain'; then
  fail "start-task: description is langchain-specific"
else
  pass "start-task: description is generic"
fi

# complete-task.md: has update-docs reference
grep -q 'update-docs' "$GLOBAL_DIR/commands/complete-task.md" && pass "complete-task: references update-docs" || fail "complete-task: missing update-docs reference"

# update-instruction.md: targets instruction.md
grep -q 'instruction\.md' "$GLOBAL_DIR/commands/update-instruction.md" && pass "update-instruction: targets instruction.md" || fail "update-instruction: missing instruction.md target"

echo ""
echo "=== Project Rules (.claude/rules/) ==="

# .claude/rules/patterns.md exists and is non-empty
[ -f "$PROJECT_RULES/patterns.md" ] && pass "rules/patterns.md exists" || fail "rules/patterns.md missing"
[ -s "$PROJECT_RULES/patterns.md" ] && pass "rules/patterns.md is non-empty" || fail "rules/patterns.md is empty"

# patterns.md heading
head -1 "$PROJECT_RULES/patterns.md" | grep -q '^# ' && pass "patterns.md starts with heading" || fail "patterns.md missing heading"

# patterns.md: defensive loading and cross-platform
grep -q 'Defensive loading' "$PROJECT_RULES/patterns.md" && pass "patterns: defensive loading" || fail "patterns: missing defensive loading"
grep -qE 'Cross-platform|\$OSDIST' "$PROJECT_RULES/patterns.md" && pass "patterns: OSDIST branching" || fail "patterns: missing OSDIST branching"

echo ""
echo "=== CLAUDE.md Pointer Integrity ==="

# Global CLAUDE.md: pointer-only, references rules/ and commands/
GLOBAL_LINES=$(wc -l < "$GLOBAL_DIR/CLAUDE.md")
[ "$GLOBAL_LINES" -le 10 ] && pass "global CLAUDE.md is slim ($GLOBAL_LINES lines)" || fail "global CLAUDE.md too long ($GLOBAL_LINES lines)"
grep -q 'rules/' "$GLOBAL_DIR/CLAUDE.md" && pass "global CLAUDE.md references rules/" || fail "global CLAUDE.md missing rules/ reference"
grep -q 'commands/' "$GLOBAL_DIR/CLAUDE.md" && pass "global CLAUDE.md references commands/" || fail "global CLAUDE.md missing commands/ reference"

# CLAUDE.md contains no rule content
if grep -qiE '(curl\.exe|git -C|Co-Authored|\.context-private|pre-commit)' "$GLOBAL_DIR/CLAUDE.md"; then
  fail "global CLAUDE.md still contains rule content"
else
  pass "global CLAUDE.md has no rule content"
fi

# Project CLAUDE.md: pointer-only, references .claude/rules/ and docs/
PROJECT_LINES=$(wc -l < "$DOTFILES_DIR/CLAUDE.md")
[ "$PROJECT_LINES" -le 10 ] && pass "project CLAUDE.md is slim ($PROJECT_LINES lines)" || fail "project CLAUDE.md too long ($PROJECT_LINES lines)"
grep -q '\.claude/rules/' "$DOTFILES_DIR/CLAUDE.md" && pass "project CLAUDE.md references .claude/rules/" || fail "project CLAUDE.md missing .claude/rules/ reference"
grep -q 'docs/' "$DOTFILES_DIR/CLAUDE.md" && pass "project CLAUDE.md references docs/" || fail "project CLAUDE.md missing docs/ reference"

# Project CLAUDE.md has no Installation code block (delegated to README.md)
if grep -q '```bash' "$DOTFILES_DIR/CLAUDE.md" || grep -q '```powershell' "$DOTFILES_DIR/CLAUDE.md"; then
  fail "project CLAUDE.md still contains Installation code blocks"
else
  pass "project CLAUDE.md has no Installation code blocks"
fi

# Project CLAUDE.md has no Shell Configuration section (delegated to docs/)
if grep -q '### Shell Configuration' "$DOTFILES_DIR/CLAUDE.md"; then
  fail "project CLAUDE.md still contains Shell Configuration section"
else
  pass "project CLAUDE.md has no Shell Configuration section"
fi

echo ""
echo "=== Cross-Cutting Checks ==="

# No private info patterns in committed rule/skill files
for f in "$GLOBAL_DIR"/rules/*.md "$GLOBAL_DIR"/commands/*.md "$PROJECT_RULES"/*.md; do
  name=$(basename "$f")
  if grep -qiE '(password|api_key|secret_key|access_token)=' "$f"; then
    fail "$name contains potential secret"
  fi
done
pass "no secrets detected in rules/skills"

# .context-private/ is git-ignored (may be in local or global gitignore)
if git -C "$DOTFILES_DIR" check-ignore -q .context-private/ 2>/dev/null; then
  pass ".context-private is git-ignored"
else
  fail ".context-private is not git-ignored"
fi

# Delegated info exists in canonical locations
grep -q "install.sh" "$DOTFILES_DIR/README.md" && pass "README.md has install.sh" || fail "README.md missing install.sh"
grep -q "Shell Configuration" "$DOTFILES_DIR/docs/architecture.md" && pass "docs/architecture.md has Shell Configuration" || fail "docs/architecture.md missing Shell Configuration"
grep -q "Defensive loading" "$DOTFILES_DIR/docs/architecture.md" && pass "docs/architecture.md has Defensive loading" || fail "docs/architecture.md missing Defensive loading"

# Symlink: ~/.claude/ is accessible and has rules
if [ -d "$HOME/.claude/rules" ] && [ "$(ls "$HOME/.claude/rules/"*.md 2>/dev/null | wc -l)" -ge 1 ]; then
  pass "~/.claude/rules/ accessible"
else
  fail "~/.claude/rules/ not accessible (check symlink)"
fi

# --- Results ---
echo ""
if [ "$ERRORS" -eq 0 ]; then
  echo "All tests passed!"
else
  echo "$ERRORS test(s) failed"
  exit 1
fi
