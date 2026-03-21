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
    if echo "$result" | grep -q '"block"'; then
        if echo "$result" | grep -q '/update-docs'; then pass "$desc"
        else fail "$desc — block missing /update-docs guidance, got: $result"; fi
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
expect_approve "whitespace-only command" '{"tool_name":"Bash","tool_input":{"command":"   \t  "}}'
expect_approve "invalid JSON" 'NOT JSON'

# --- Integration tests: real git repo with staging ---

TMPDIR_BASE=$(mktemp -d)
trap 'rm -rf "$TMPDIR_BASE"' EXIT

setup_repo() {
    local repo="$TMPDIR_BASE/repo-$RANDOM"
    mkdir -p "$repo/docs" "$repo/src"
    git -C "$repo" init -q
    git -C "$repo" config user.email "test@example.com"
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
    if echo "$result" | grep -q '"block"'; then
        if echo "$result" | grep -q '/update-docs'; then pass "$desc"
        else fail "$desc — block missing /update-docs guidance, got: $result"; fi
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
echo "=== Integration: .md outside docs only (should approve — counts as doc change) ==="
REPO=$(setup_repo)
mkdir -p "$REPO/projects/engineering"
echo "spec" > "$REPO/projects/engineering/architecture.md"
git -C "$REPO" add projects/engineering/architecture.md
expect_approve_repo "md outside docs — single file" "$REPO" "$COMMIT_JSON"

echo ""
echo "=== Integration: multiple .md outside docs (should approve) ==="
REPO=$(setup_repo)
mkdir -p "$REPO/projects/engineering/langchain" "$REPO/notes"
echo "ops" > "$REPO/projects/engineering/langchain/ops.md"
echo "todo" > "$REPO/projects/engineering/todo.md"
echo "note" > "$REPO/notes/meeting.md"
git -C "$REPO" add projects/ notes/
expect_approve_repo "md outside docs — multiple dirs" "$REPO" "$COMMIT_JSON"

echo ""
echo "=== Integration: code + .md outside docs (should approve — md satisfies doc req) ==="
REPO=$(setup_repo)
mkdir -p "$REPO/projects"
echo "spec" > "$REPO/projects/spec.md"
echo "code" > "$REPO/src/app.js"
git -C "$REPO" add projects/spec.md src/app.js
expect_approve_repo "code + md outside docs" "$REPO" "$COMMIT_JSON"

echo ""
echo "=== Integration: deeply nested .md (should approve) ==="
REPO=$(setup_repo)
mkdir -p "$REPO/projects/engineering/langchain/architecture"
echo "detail" > "$REPO/projects/engineering/langchain/architecture/overview.md"
git -C "$REPO" add projects/
expect_approve_repo "deeply nested md" "$REPO" "$COMMIT_JSON"

echo ""
echo "=== Integration: uppercase .MD extension (should approve — counts as doc) ==="
REPO=$(setup_repo)
mkdir -p "$REPO/projects"
echo "spec" > "$REPO/projects/DESIGN.MD"
git -C "$REPO" add projects/DESIGN.MD
expect_approve_repo "uppercase .MD extension" "$REPO" "$COMMIT_JSON"

echo ""
echo "=== Integration: code + only .md outside docs (should approve — md satisfies doc req) ==="
REPO=$(setup_repo)
echo "code" > "$REPO/src/app.js"
echo "spec" > "$REPO/CHANGELOG.md"
git -C "$REPO" add src/app.js CHANGELOG.md
expect_approve_repo "code + root .md (no docs/ change)" "$REPO" "$COMMIT_JSON"

echo ""
echo "=== Integration: idempotency — repeated hook calls produce same result ==="
REPO=$(setup_repo)
echo "new code" > "$REPO/src/app.js"
git -C "$REPO" add src/app.js
result1=$(run_hook_in_repo "$REPO" "$COMMIT_JSON")
result2=$(run_hook_in_repo "$REPO" "$COMMIT_JSON")
if [ "$result1" = "$result2" ]; then pass "idempotent block"
else fail "idempotent block — results differ: $result1 vs $result2"; fi

REPO=$(setup_repo)
mkdir -p "$REPO/projects"
echo "doc" > "$REPO/projects/notes.md"
git -C "$REPO" add projects/notes.md
result1=$(run_hook_in_repo "$REPO" "$COMMIT_JSON")
result2=$(run_hook_in_repo "$REPO" "$COMMIT_JSON")
if [ "$result1" = "$result2" ]; then pass "idempotent approve"
else fail "idempotent approve — results differ: $result1 vs $result2"; fi

# --- ai-specs fallback integration tests ---
# Helper: set up a pair of repos (source + sibling ai-specs)
setup_repo_pair() {
    local base="$TMPDIR_BASE/pair-$RANDOM"
    local repo="$base/llm-infra-check"
    local aispecs="$base/ai-specs"
    mkdir -p "$repo/src" "$aispecs/projects/engineering/llm-infra-check"

    # Init source repo
    git -C "$repo" init -q
    git -C "$repo" config user.email "test@example.com"
    git -C "$repo" config user.name "Test"
    echo "init" > "$repo/README.md"
    git -C "$repo" add -A
    git -C "$repo" commit -q -m "initial"

    # Init ai-specs repo
    git -C "$aispecs" init -q
    git -C "$aispecs" config user.email "test@example.com"
    git -C "$aispecs" config user.name "Test"
    echo "init" > "$aispecs/README.md"
    echo "# arch" > "$aispecs/projects/engineering/llm-infra-check/architecture.md"
    git -C "$aispecs" add -A
    git -C "$aispecs" commit -q -m "initial"

    echo "$repo $aispecs"
}

echo ""
echo "=== ai-specs fallback: external docs staged (should approve) ==="
read -r REPO AISPECS <<< "$(setup_repo_pair)"
echo "new code" > "$REPO/src/app.js"
git -C "$REPO" add src/app.js
echo "updated" >> "$AISPECS/projects/engineering/llm-infra-check/architecture.md"
git -C "$AISPECS" add projects/engineering/llm-infra-check/architecture.md
expect_approve_repo "external docs staged" "$REPO" "$COMMIT_JSON"

echo ""
echo "=== ai-specs fallback: external docs unstaged (should approve) ==="
read -r REPO AISPECS <<< "$(setup_repo_pair)"
echo "new code" > "$REPO/src/app.js"
git -C "$REPO" add src/app.js
echo "unstaged change" >> "$AISPECS/projects/engineering/llm-infra-check/architecture.md"
expect_approve_repo "external docs unstaged" "$REPO" "$COMMIT_JSON"

echo ""
echo "=== ai-specs fallback: no external docs changes (should block) ==="
read -r REPO AISPECS <<< "$(setup_repo_pair)"
echo "new code" > "$REPO/src/app.js"
git -C "$REPO" add src/app.js
expect_block_repo "no external docs changes" "$REPO" "$COMMIT_JSON"

echo ""
echo "=== ai-specs fallback: no sibling ai-specs dir (should block) ==="
SOLO_BASE="$TMPDIR_BASE/solo-$RANDOM"
REPO="$SOLO_BASE/my-project"
mkdir -p "$REPO/src"
git -C "$REPO" init -q
git -C "$REPO" config user.email "test@example.com"
git -C "$REPO" config user.name "Test"
echo "init" > "$REPO/README.md"
git -C "$REPO" add -A
git -C "$REPO" commit -q -m "initial"
echo "new code" > "$REPO/src/app.js"
git -C "$REPO" add src/app.js
expect_block_repo "no sibling ai-specs" "$REPO" "$COMMIT_JSON"

echo ""
echo "=== ai-specs fallback: same-name dir not found in ai-specs (should block) ==="
PAIR_BASE="$TMPDIR_BASE/pair-$RANDOM"
REPO="$PAIR_BASE/some-other-repo"
AISPECS="$PAIR_BASE/ai-specs"
mkdir -p "$REPO/src" "$AISPECS"
git -C "$REPO" init -q
git -C "$REPO" config user.email "test@example.com"
git -C "$REPO" config user.name "Test"
echo "init" > "$REPO/README.md"
git -C "$REPO" add -A
git -C "$REPO" commit -q -m "initial"
git -C "$AISPECS" init -q
git -C "$AISPECS" config user.email "test@example.com"
git -C "$AISPECS" config user.name "Test"
echo "init" > "$AISPECS/README.md"
git -C "$AISPECS" add -A
git -C "$AISPECS" commit -q -m "initial"
echo "new code" > "$REPO/src/app.js"
git -C "$REPO" add src/app.js
expect_block_repo "same-name dir not found" "$REPO" "$COMMIT_JSON"

echo ""
echo "=== ai-specs fallback: no code changes, exempt only (should approve) ==="
read -r REPO AISPECS <<< "$(setup_repo_pair)"
mkdir -p "$REPO/tests"
echo "test" > "$REPO/tests/test.sh"
git -C "$REPO" add tests/test.sh
expect_approve_repo "exempt only, ai-specs sibling exists" "$REPO" "$COMMIT_JSON"

echo ""
echo "=== ai-specs fallback: changes outside same-name dir (should block) ==="
read -r REPO AISPECS <<< "$(setup_repo_pair)"
echo "new code" > "$REPO/src/app.js"
git -C "$REPO" add src/app.js
mkdir -p "$AISPECS/projects/engineering/other-project"
echo "unrelated" > "$AISPECS/projects/engineering/other-project/notes.md"
git -C "$AISPECS" add projects/engineering/other-project/notes.md
expect_block_repo "changes outside same-name dir" "$REPO" "$COMMIT_JSON"

echo ""
echo "=== ai-specs fallback: multiple matching dirs, one has changes (should approve) ==="
read -r REPO AISPECS <<< "$(setup_repo_pair)"
echo "new code" > "$REPO/src/app.js"
git -C "$REPO" add src/app.js
mkdir -p "$AISPECS/archive/llm-infra-check"
echo "old" > "$AISPECS/archive/llm-infra-check/readme.md"
git -C "$AISPECS" add archive/
git -C "$AISPECS" commit -q -m "add archive"
echo "updated" >> "$AISPECS/projects/engineering/llm-infra-check/architecture.md"
git -C "$AISPECS" add projects/engineering/llm-infra-check/architecture.md
expect_approve_repo "multiple dirs, one has changes" "$REPO" "$COMMIT_JSON"

echo ""
echo "=== ai-specs fallback: filename with spaces (should block without docs) ==="
read -r REPO AISPECS <<< "$(setup_repo_pair)"
echo "new code" > "$REPO/src/my app.js"
git -C "$REPO" add "src/my app.js"
expect_block_repo "filename with spaces, no docs" "$REPO" "$COMMIT_JSON"

echo ""
echo "=== ai-specs fallback: filename with spaces + external docs (should approve) ==="
read -r REPO AISPECS <<< "$(setup_repo_pair)"
echo "new code" > "$REPO/src/my app.js"
git -C "$REPO" add "src/my app.js"
echo "updated" >> "$AISPECS/projects/engineering/llm-infra-check/architecture.md"
git -C "$AISPECS" add projects/engineering/llm-infra-check/architecture.md
expect_approve_repo "filename with spaces + external docs" "$REPO" "$COMMIT_JSON"

echo ""
echo "=== ai-specs fallback: local docs take priority over ai-specs (should approve) ==="
read -r REPO AISPECS <<< "$(setup_repo_pair)"
mkdir -p "$REPO/docs"
echo "# history" > "$REPO/docs/history.md"
git -C "$REPO" add docs/
git -C "$REPO" commit -q -m "add local docs"
echo "new code" > "$REPO/src/app.js"
echo "updated" >> "$REPO/docs/history.md"
git -C "$REPO" add src/app.js docs/history.md
expect_approve_repo "local docs priority" "$REPO" "$COMMIT_JSON"

echo ""
echo "=== Results ==="
if [ "$ERRORS" -eq 0 ]; then
    echo "All tests passed!"
else
    echo "$ERRORS test(s) failed"
    exit 1
fi
