#!/usr/bin/env bash
# Tests for workflow-gate.js: resolveRepoDir, hasStagedTestChanges, hasStagedDocChanges
# Branch: fix/workflow-gate-unix-path

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
HOOK_UNIX="$REPO_ROOT/claude-global/hooks/workflow-gate.js"

# Convert to Windows path for Node.js require
if command -v cygpath >/dev/null 2>&1; then
  HOOK_WIN="$(cygpath -w "$HOOK_UNIX")"
else
  HOOK_WIN="$HOOK_UNIX"
fi

PASS=0
FAIL=0
ERRORS=()

run_with_timeout() {
  if command -v timeout >/dev/null 2>&1; then
    timeout 120 "$@"
  else
    perl -e 'alarm 120; exec @ARGV' -- "$@"
  fi
}

assert_eq() {
  local name="$1" expected="$2" actual="$3"
  if [ "$expected" = "$actual" ]; then
    echo "  PASS: $name"
    PASS=$((PASS + 1))
  else
    echo "  FAIL: $name"
    echo "    expected: $expected"
    echo "    actual:   $actual"
    FAIL=$((FAIL + 1))
    ERRORS+=("$name")
  fi
}

assert_true() {
  local name="$1" actual="$2"
  if [ "$actual" = "true" ]; then
    echo "  PASS: $name"
    PASS=$((PASS + 1))
  else
    echo "  FAIL: $name (expected true, got '$actual')"
    FAIL=$((FAIL + 1))
    ERRORS+=("$name")
  fi
}

assert_false() {
  local name="$1" actual="$2"
  if [ "$actual" = "false" ]; then
    echo "  PASS: $name"
    PASS=$((PASS + 1))
  else
    echo "  FAIL: $name (expected false, got '$actual')"
    FAIL=$((FAIL + 1))
    ERRORS+=("$name")
  fi
}

assert_contains() {
  local name="$1" expected="$2" actual="$3"
  if echo "$actual" | grep -q "$expected"; then
    echo "  PASS: $name"
    PASS=$((PASS + 1))
  else
    echo "  FAIL: $name"
    echo "    expected to contain: $expected"
    echo "    actual: $actual"
    FAIL=$((FAIL + 1))
    ERRORS+=("$name")
  fi
}

# Helper: run Node with HOOK_PATH env var to avoid escaping issues
node_hook() {
  HOOK_PATH="$HOOK_WIN" run_with_timeout node "$@"
}

# ============================================================
# Section A: resolveRepoDir unit tests
# ============================================================
echo ""
echo "=== A. resolveRepoDir unit tests ==="

# N1: /c/git/dotfiles -> C:\git\dotfiles
result=$(node_hook --input-type=module <<'EOF'
import { createRequire } from 'module';
const require = createRequire(import.meta.url);
const { resolveRepoDir } = require(process.env.HOOK_PATH);
process.stdout.write(resolveRepoDir('git -C /c/git/dotfiles commit -m "msg"'));
EOF
)
assert_eq 'N1: /c/path -> C:\git\dotfiles' 'C:\git\dotfiles' "$result"

# N2: /C/Users/foo/bar -> C:\Users\foo\bar
result=$(node_hook --input-type=module <<'EOF'
import { createRequire } from 'module';
const require = createRequire(import.meta.url);
const { resolveRepoDir } = require(process.env.HOOK_PATH);
process.stdout.write(resolveRepoDir('git -C /C/Users/foo/bar commit -m "msg"'));
EOF
)
assert_eq 'N2: /C/path (uppercase) -> C:\Users\foo\bar' 'C:\Users\foo\bar' "$result"

# N3: C:\git\dotfiles -> C:\git\dotfiles (no conversion)
result=$(node_hook --input-type=module <<'EOF'
import { createRequire } from 'module';
const require = createRequire(import.meta.url);
const { resolveRepoDir } = require(process.env.HOOK_PATH);
process.stdout.write(resolveRepoDir('git -C C:\\git\\dotfiles commit -m "msg"'));
EOF
)
assert_eq 'N3: Windows path unchanged' 'C:\git\dotfiles' "$result"

# N4: no -C flag -> process.cwd()
expected_cwd=$(node --input-type=module <<'EOF'
process.stdout.write(process.cwd());
EOF
)
result=$(node_hook --input-type=module <<'EOF'
import { createRequire } from 'module';
const require = createRequire(import.meta.url);
const { resolveRepoDir } = require(process.env.HOOK_PATH);
process.stdout.write(resolveRepoDir('git commit -m "msg"'));
EOF
)
assert_eq "N4: no -C -> process.cwd()" "$expected_cwd" "$result"

# E1: /c -> C:\
result=$(node_hook --input-type=module <<'EOF'
import { createRequire } from 'module';
const require = createRequire(import.meta.url);
const { resolveRepoDir } = require(process.env.HOOK_PATH);
process.stdout.write(resolveRepoDir('git -C /c commit -m "msg"'));
EOF
)
assert_eq 'E1: /c -> C:\' 'C:\' "$result"

# E2: /git/dotfiles (no drive letter) -> unchanged
result=$(node_hook --input-type=module <<'EOF'
import { createRequire } from 'module';
const require = createRequire(import.meta.url);
const { resolveRepoDir } = require(process.env.HOOK_PATH);
process.stdout.write(resolveRepoDir('git -C /git/dotfiles commit -m "msg"'));
EOF
)
assert_eq "E2: /git/dotfiles (no drive) -> unchanged" '/git/dotfiles' "$result"

# E3: . -> .
result=$(node_hook --input-type=module <<'EOF'
import { createRequire } from 'module';
const require = createRequire(import.meta.url);
const { resolveRepoDir } = require(process.env.HOOK_PATH);
process.stdout.write(resolveRepoDir('git -C . commit -m "msg"'));
EOF
)
assert_eq "E3: dot path -> dot" '.' "$result"

# ============================================================
# Section B: hasStagedTestChanges / hasStagedDocChanges integration
# ============================================================
echo ""
echo "=== B. hasStagedTestChanges / hasStagedDocChanges integration tests ==="

TMPDIR_BASE="$(mktemp -d)"
trap 'rm -rf "$TMPDIR_BASE"' EXIT

setup_repo() {
  local dir="$1"
  git -C "$dir" init -q
  git -C "$dir" config user.email "test@example.com"
  git -C "$dir" config user.name "Test User"
}

to_win_path() {
  local p="$1"
  if command -v cygpath >/dev/null 2>&1; then
    cygpath -w "$p"
  else
    echo "$p"
  fi
}

# I1: tests/foo.sh staged -> hasStagedTestChanges = true
REPO1="$TMPDIR_BASE/repo1"
mkdir -p "$REPO1"
setup_repo "$REPO1"
mkdir -p "$REPO1/tests"
echo "# test" > "$REPO1/tests/foo.sh"
git -C "$REPO1" add "tests/foo.sh"
REPO1_WIN="$(to_win_path "$REPO1")"

result=$(HOOK_PATH="$HOOK_WIN" REPO_DIR="$REPO1_WIN" run_with_timeout node --input-type=module <<'EOF'
import { createRequire } from 'module';
const require = createRequire(import.meta.url);
const { hasStagedTestChanges } = require(process.env.HOOK_PATH);
process.stdout.write(String(hasStagedTestChanges(process.env.REPO_DIR)));
EOF
)
assert_true "I1: tests/foo.sh staged -> hasStagedTestChanges=true" "$result"

# I2: docs/ops.md staged -> hasStagedDocChanges = true
REPO2="$TMPDIR_BASE/repo2"
mkdir -p "$REPO2"
setup_repo "$REPO2"
mkdir -p "$REPO2/docs"
echo "# ops" > "$REPO2/docs/ops.md"
git -C "$REPO2" add "docs/ops.md"
REPO2_WIN="$(to_win_path "$REPO2")"

result=$(HOOK_PATH="$HOOK_WIN" REPO_DIR="$REPO2_WIN" run_with_timeout node --input-type=module <<'EOF'
import { createRequire } from 'module';
const require = createRequire(import.meta.url);
const { hasStagedDocChanges } = require(process.env.HOOK_PATH);
process.stdout.write(String(hasStagedDocChanges(process.env.REPO_DIR)));
EOF
)
assert_true "I2: docs/ops.md staged -> hasStagedDocChanges=true" "$result"

# I3: src/main.js only -> both false
REPO3="$TMPDIR_BASE/repo3"
mkdir -p "$REPO3"
setup_repo "$REPO3"
mkdir -p "$REPO3/src"
echo "// main" > "$REPO3/src/main.js"
git -C "$REPO3" add "src/main.js"
REPO3_WIN="$(to_win_path "$REPO3")"

result_test=$(HOOK_PATH="$HOOK_WIN" REPO_DIR="$REPO3_WIN" run_with_timeout node --input-type=module <<'EOF'
import { createRequire } from 'module';
const require = createRequire(import.meta.url);
const { hasStagedTestChanges } = require(process.env.HOOK_PATH);
process.stdout.write(String(hasStagedTestChanges(process.env.REPO_DIR)));
EOF
)
result_doc=$(HOOK_PATH="$HOOK_WIN" REPO_DIR="$REPO3_WIN" run_with_timeout node --input-type=module <<'EOF'
import { createRequire } from 'module';
const require = createRequire(import.meta.url);
const { hasStagedDocChanges } = require(process.env.HOOK_PATH);
process.stdout.write(String(hasStagedDocChanges(process.env.REPO_DIR)));
EOF
)
assert_false "I3: src/main.js only -> hasStagedTestChanges=false" "$result_test"
assert_false "I3: src/main.js only -> hasStagedDocChanges=false" "$result_doc"

# ============================================================
# Section C: error handling
# ============================================================
echo ""
echo "=== C. error handling tests ==="

# Er1: nonexistent cwd -> false + stderr warning
STDERR_TMP="$TMPDIR_BASE/stderr_er1.txt"
result=$(HOOK_PATH="$HOOK_WIN" REPO_DIR="/nonexistent/path/that/does/not/exist" \
  run_with_timeout node --input-type=module 2>"$STDERR_TMP" <<'EOF'
import { createRequire } from 'module';
const require = createRequire(import.meta.url);
const { hasStagedTestChanges } = require(process.env.HOOK_PATH);
process.stdout.write(String(hasStagedTestChanges(process.env.REPO_DIR)));
EOF
)
stderr_out=$(cat "$STDERR_TMP")

assert_false "Er1: nonexistent cwd -> hasStagedTestChanges=false" "$result"
assert_contains "Er1: nonexistent cwd -> stderr warning contains 'workflow-gate'" "workflow-gate" "$stderr_out"

# ============================================================
# Summary
# ============================================================
echo ""
echo "=============================="
echo "Results: PASS=$PASS FAIL=$FAIL"
if [ "${#ERRORS[@]}" -gt 0 ]; then
  echo "Failed tests:"
  for e in "${ERRORS[@]}"; do
    echo "  - $e"
  done
fi
echo "=============================="

if [ "$FAIL" -gt 0 ]; then
  exit 1
fi
exit 0
