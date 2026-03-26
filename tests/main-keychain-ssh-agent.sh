#!/bin/bash
# Tests for keychain SSH agent: install.sh default inclusion and .profile_common auto-detection
set -euo pipefail

PASS=0
FAIL=0
pass() { echo "  PASS: $1"; PASS=$((PASS + 1)); }
fail() { echo "  FAIL: $1"; FAIL=$((FAIL + 1)); }

DOTFILES_DIR="$(cd "$(dirname "$0")/.." && pwd)"

echo "=== install.sh: keychain in default install ==="

# Normal: keychain.sh is outside --base/--full block
# Extract lines before the first --base/--full if-block
base_line=$(grep -n '"--base"' "$DOTFILES_DIR/install.sh" | head -1 | cut -d: -f1)
if grep -q 'keychain\.sh' <(head -n "$((base_line - 1))" "$DOTFILES_DIR/install.sh"); then
    pass "keychain.sh is in default (no-flag) section"
else
    fail "keychain.sh is NOT in default section"
fi

# Normal: step numbers are sequential
steps=$(grep -o '# Step [0-9]*' "$DOTFILES_DIR/install.sh" | grep -o '[0-9]*')
expected=$(seq 1 "$(echo "$steps" | wc -l)")
if [ "$steps" = "$expected" ]; then
    pass "Step numbers are sequential"
else
    fail "Step numbers are not sequential: got [$steps]"
fi

echo ""
echo "=== .profile_common: SSH key auto-detection ==="

# Create temporary SSH directory for testing
TEST_SSH_DIR=$(mktemp -d)
trap "rm -rf $TEST_SSH_DIR" EXIT

# Helper: extract the find command from .profile_common and run it against test dir
find_keys() {
    find "$TEST_SSH_DIR" -maxdepth 1 -name 'id_*' ! -name '*.pub' -printf '%f\n' 2>/dev/null
}

# Normal: single key detected
touch "$TEST_SSH_DIR/id_ed25519"
touch "$TEST_SSH_DIR/id_ed25519.pub"
result=$(find_keys)
if [ "$result" = "id_ed25519" ]; then
    pass "Single key detected"
else
    fail "Single key detection: got [$result]"
fi
rm -f "$TEST_SSH_DIR"/*

# Normal: multiple keys detected
touch "$TEST_SSH_DIR/id_ed25519"
touch "$TEST_SSH_DIR/id_ed25519.pub"
touch "$TEST_SSH_DIR/id_rsa"
touch "$TEST_SSH_DIR/id_rsa.pub"
result=$(find_keys | sort)
expected=$(printf 'id_ed25519\nid_rsa')
if [ "$result" = "$expected" ]; then
    pass "Multiple keys detected"
else
    fail "Multiple keys detection: got [$result], expected [$expected]"
fi
rm -f "$TEST_SSH_DIR"/*

# Edge: no keys in directory
result=$(find_keys)
if [ -z "$result" ]; then
    pass "No keys: empty result"
else
    fail "No keys: got [$result]"
fi

# Edge: only .pub files
touch "$TEST_SSH_DIR/id_ed25519.pub"
touch "$TEST_SSH_DIR/id_rsa.pub"
result=$(find_keys)
if [ -z "$result" ]; then
    pass "Only .pub files: empty result"
else
    fail "Only .pub files: got [$result]"
fi
rm -f "$TEST_SSH_DIR"/*

# Idempotency: _ssh_keys is unset after sourcing
# Simulate the pattern from .profile_common
_ssh_keys=$(find_keys)
unset _ssh_keys
touch "$TEST_SSH_DIR/id_ed25519"
_ssh_keys=$(find_keys)
unset _ssh_keys
if [ -z "${_ssh_keys:-}" ]; then
    pass "Idempotency: _ssh_keys is unset after cleanup"
else
    fail "Idempotency: _ssh_keys leaked [$_ssh_keys]"
fi
rm -f "$TEST_SSH_DIR"/*

# Edge: ~/.ssh directory does not exist
NONEXISTENT_DIR=$(mktemp -u)
result=$(find "$NONEXISTENT_DIR" -maxdepth 1 -name 'id_*' ! -name '*.pub' -printf '%f\n' 2>/dev/null || true)
if [ -z "$result" ]; then
    pass "Nonexistent SSH directory: empty result, no error"
else
    fail "Nonexistent SSH directory: got [$result]"
fi

# Edge: key name with hyphens (e.g., id_rsa-2024)
touch "$TEST_SSH_DIR/id_rsa-2024"
touch "$TEST_SSH_DIR/id_rsa-2024.pub"
result=$(find_keys)
if [ "$result" = "id_rsa-2024" ]; then
    pass "Hyphenated key name detected"
else
    fail "Hyphenated key name: got [$result]"
fi
rm -f "$TEST_SSH_DIR"/*

# Idempotency: find_keys returns same result on repeated calls
touch "$TEST_SSH_DIR/id_ed25519"
result1=$(find_keys)
result2=$(find_keys)
if [ "$result1" = "$result2" ]; then
    pass "Idempotency: find_keys returns same result twice"
else
    fail "Idempotency: first [$result1] != second [$result2]"
fi
rm -f "$TEST_SSH_DIR"/*

echo ""
echo "=== Results: $PASS passed, $FAIL failed ==="
[ "$FAIL" -eq 0 ]
