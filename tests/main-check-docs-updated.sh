#!/bin/bash
# Test suite for check-docs-updated.js PreToolUse hook
set -euo pipefail

DOTFILES_DIR="$(cd "$(dirname "$0")/.." && pwd)"
HOOK="$DOTFILES_DIR/claude-global/hooks/check-docs-updated.js"
ERRORS=0

fail() { echo "FAIL: $1"; ERRORS=$((ERRORS + 1)); }
pass() { echo "PASS: $1"; }

# --- Unit tests: non-commit commands (no git repo needed) ---

run_hook() {
    local json="$1"
    echo "$json" | node "$HOOK" 2>/dev/null
}

expect_approve() {
    local desc="$1" json="$2"
    local result
    result=$(run_hook "$json")
    if echo "$result" | grep -q '"approve"'; then pass "$desc"
    else fail "$desc — expected approve, got: $result"; fi
}

expect_block() {
    local desc="$1" json="$2"
    local result
    result=$(run_hook "$json")
    if echo "$result" | grep -q '"block"'; then pass "$desc"
    else fail "$desc — expected block, got: $result"; fi
}

echo "=== Non-commit commands (should approve) ==="
expect_approve "git status" '{"tool_name":"Bash","tool_input":{"command":"git status"}}'
expect_approve "git push" '{"tool_name":"Bash","tool_input":{"command":"git push"}}'
expect_approve "git diff" '{"tool_name":"Bash","tool_input":{"command":"git diff"}}'
expect_approve "ls command" '{"tool_name":"Bash","tool_input":{"command":"ls -la"}}'
expect_approve "non-Bash tool" '{"tool_name":"Read","tool_input":{"file_path":"README.md"}}'

echo ""
echo "=== Edge cases ==="
expect_approve "missing tool_input" '{"tool_name":"Bash"}'
expect_approve "empty command" '{"tool_name":"Bash","tool_input":{"command":""}}'
expect_approve "invalid JSON" 'NOT JSON'

# --- Integration tests: real git repo with staging ---

TMPDIR_BASE=$(mktemp -d)
trap 'rm -rf "$TMPDIR_BASE"' EXIT

setup_repo() {
    local repo="$TMPDIR_BASE/repo-$RANDOM"
    mkdir -p "$repo/docs" "$repo/src"
    git -C "$repo" init -q
    git -C "$repo" config user.email "test@test.com"
    git -C "$repo" config user.name "Test"
    echo "init" > "$repo/README.md"
    echo "# history" > "$repo/docs/history.md"
    git -C "$repo" add -A
    git -C "$repo" commit -q -m "initial"
    echo "$repo"
}

run_hook_in_repo() {
    local repo="$1" json="$2"
    echo "$json" | HOOK_CWD="$repo" node "$HOOK" 2>/dev/null
}

expect_block_repo() {
    local desc="$1" repo="$2" json="$3"
    local result
    result=$(run_hook_in_repo "$repo" "$json")
    if echo "$result" | grep -q '"block"'; then pass "$desc"
    else fail "$desc — expected block, got: $result"; fi
}

expect_approve_repo() {
    local desc="$1" repo="$2" json="$3"
    local result
    result=$(run_hook_in_repo "$repo" "$json")
    if echo "$result" | grep -q '"approve"'; then pass "$desc"
    else fail "$desc — expected approve, got: $result"; fi
}

COMMIT_JSON='{"tool_name":"Bash","tool_input":{"command":"git commit -m \"update\""}}'

echo ""
echo "=== Integration: code change without docs (should block) ==="
REPO=$(setup_repo)
echo "new code" > "$REPO/src/app.js"
git -C "$REPO" add src/app.js
expect_block_repo "src change, no docs staged" "$REPO" "$COMMIT_JSON"

echo ""
echo "=== Integration: code change with docs (should approve) ==="
REPO=$(setup_repo)
echo "new code" > "$REPO/src/app.js"
echo "updated" >> "$REPO/docs/history.md"
git -C "$REPO" add src/app.js docs/history.md
expect_approve_repo "src + docs staged" "$REPO" "$COMMIT_JSON"

echo ""
echo "=== Integration: docs-only change (should approve) ==="
REPO=$(setup_repo)
echo "updated" >> "$REPO/docs/history.md"
git -C "$REPO" add docs/history.md
expect_approve_repo "docs-only change" "$REPO" "$COMMIT_JSON"

echo ""
echo "=== Integration: tests-only change (should approve) ==="
REPO=$(setup_repo)
mkdir -p "$REPO/tests"
echo "test" > "$REPO/tests/test.sh"
git -C "$REPO" add tests/test.sh
expect_approve_repo "tests-only change" "$REPO" "$COMMIT_JSON"

echo ""
echo "=== Integration: config-only change (should approve) ==="
REPO=$(setup_repo)
echo "setting" > "$REPO/.gitignore"
git -C "$REPO" add .gitignore
expect_approve_repo "config-only (no src)" "$REPO" "$COMMIT_JSON"

echo ""
echo "=== Integration: nothing staged (should approve) ==="
REPO=$(setup_repo)
expect_approve_repo "nothing staged" "$REPO" "$COMMIT_JSON"

echo ""
echo "=== Integration: git -C path commit (should detect) ==="
REPO=$(setup_repo)
echo "new code" > "$REPO/src/app.js"
git -C "$REPO" add src/app.js
expect_block_repo "git -C commit" "$REPO" "{\"tool_name\":\"Bash\",\"tool_input\":{\"command\":\"git -C $REPO commit -m \\\"update\\\"\"}}"

echo ""
echo "=== Integration: multiple code files with docs (should approve) ==="
REPO=$(setup_repo)
echo "code1" > "$REPO/src/a.js"
echo "code2" > "$REPO/src/b.js"
echo "updated" >> "$REPO/docs/history.md"
git -C "$REPO" add src/a.js src/b.js docs/history.md
expect_approve_repo "multiple src + docs" "$REPO" "$COMMIT_JSON"

echo ""
echo "=== Integration: mixed exempt and code without docs (should block) ==="
REPO=$(setup_repo)
echo "test" > "$REPO/.gitignore"
mkdir -p "$REPO/tests"
echo "test" > "$REPO/tests/test.sh"
echo "code" > "$REPO/src/app.js"
git -C "$REPO" add .gitignore tests/test.sh src/app.js
expect_block_repo "exempt + code, no docs" "$REPO" "$COMMIT_JSON"

echo ""
echo "=== Results ==="
if [ "$ERRORS" -eq 0 ]; then
    echo "All tests passed!"
else
    echo "$ERRORS test(s) failed"
    exit 1
fi
