#!/bin/bash
# tests/main-install-guards.sh
# TDD: Tests for platform guards in install.sh files across dotfiles, dotfiles-private, and agents.
# These tests are expected to FAIL until the platform guard implementation is in place.
# Syntax tests (S-group) should PASS immediately as the source files are valid bash.

DOTFILES_DIR="c:/git/dotfiles"
PRIVATE_DIR="c:/git/dotfiles-private"
AGENTS_DIR="c:/git/agents"

PASS=0
FAIL=0

run_with_timeout() {
    if command -v timeout >/dev/null 2>&1; then
        timeout "${1}" "${@:2}"
    else
        local t="$1"; shift
        perl -e "alarm $t; exec @ARGV" -- "$@"
    fi
}

ok() {
    echo "PASS: $1"
    PASS=$((PASS + 1))
}

fail() {
    echo "FAIL: $1"
    FAIL=$((FAIL + 1))
}

check() {
    local name="$1"
    if eval "$2"; then
        ok "$name"
    else
        fail "$name"
    fi
}

echo "=== Platform Guard Tests (TDD) ==="
echo ""

# ---------------------------------------------------------------------------
# S: Syntax checks — must pass now AND after implementation
# ---------------------------------------------------------------------------
echo "--- S: Syntax checks ---"

if run_with_timeout 120 bash -n "$DOTFILES_DIR/install.sh" 2>/dev/null; then
    ok "S1: dotfiles/install.sh bash syntax"
else
    fail "S1: dotfiles/install.sh bash syntax"
fi

if run_with_timeout 120 bash -n "$PRIVATE_DIR/install.sh" 2>/dev/null; then
    ok "S2: dotfiles-private/install.sh bash syntax"
else
    fail "S2: dotfiles-private/install.sh bash syntax"
fi

if run_with_timeout 120 bash -n "$AGENTS_DIR/install.sh" 2>/dev/null; then
    ok "S3: agents/install.sh bash syntax"
else
    fail "S3: agents/install.sh bash syntax"
fi

echo ""

# ---------------------------------------------------------------------------
# G: Guard presence — static grep checks (TDD: FAIL until implementation)
# ---------------------------------------------------------------------------
echo "--- G: Guard presence (static grep) ---"

for repo_label in "dotfiles:$DOTFILES_DIR" "dotfiles-private:$PRIVATE_DIR" "agents:$AGENTS_DIR"; do
    label="${repo_label%%:*}"
    path="${repo_label#*:}"
    script="$path/install.sh"

    check "G1: $label/install.sh contains MINGW guard" \
        "grep -q 'MINGW' '$script'"

    check "G2: $label/install.sh contains MSYS guard" \
        "grep -q 'MSYS' '$script'"

    check "G3: $label/install.sh contains CYGWIN guard" \
        "grep -q 'CYGWIN' '$script'"

    # The _uname_s variable must be passed as a printf argument via %s placeholder,
    # not embedded directly in the format string — guards against format string injection.
    # Verify: the printf line uses %s AND _uname_s appears on the same line.
    check "G4: $label/install.sh printf uses %s placeholder for _uname_s (format injection safe)" \
        "grep -qE 'printf.*%s.*_uname_s' '$script'"
done

echo ""

# ---------------------------------------------------------------------------
# B: Behavioral guard tests — subprocess with fake uname binary
# ---------------------------------------------------------------------------
echo "--- B: Behavioral guard tests ---"

# Helper: run a single behavioral test
# Usage: test_guard <label> <repo_path> <uname_output> <expect_blocked: yes|no>
test_guard() {
    local label="$1"
    local script="$2"
    local uname_out="$3"
    local expect_blocked="$4"

    local _tmpdir
    _tmpdir=$(mktemp -d)

    # Create fake uname that returns the desired platform string
    cat > "$_tmpdir/uname" << FAKEUNAME
#!/bin/bash
echo "$uname_out"
FAKEUNAME
    chmod +x "$_tmpdir/uname"

    local output exit_code
    output=$(PATH="$_tmpdir:$PATH" run_with_timeout 5 bash "$script" 2>&1)
    exit_code=$?

    rm -rf "$_tmpdir"

    if [ "$expect_blocked" = "yes" ]; then
        # Expect: exit code 1 AND output mentions Windows or install.ps1
        if [ "$exit_code" -eq 1 ] && (echo "$output" | grep -qi "windows\|install\.ps1\|\.ps1\|not supported\|windows detected"); then
            ok "$label → blocked (exit=$exit_code, message found)"
        else
            fail "$label → expected block (exit=1 + Windows msg), got exit=$exit_code, output: $(echo "$output" | head -3)"
        fi
    else
        # Expect: output does NOT contain the Windows guard error
        if ! (echo "$output" | grep -qi "windows\|install\.ps1\|\.ps1 instead"); then
            ok "$label → guard passed (no Windows error in output)"
        else
            fail "$label → unexpected Windows guard message for non-Windows platform"
        fi
    fi
}

# dotfiles/install.sh behavioral tests
test_guard "B1: dotfiles MINGW64_NT-10.0" "$DOTFILES_DIR/install.sh" "MINGW64_NT-10.0" "yes"
test_guard "B2: dotfiles MSYS_NT-10.0"   "$DOTFILES_DIR/install.sh" "MSYS_NT-10.0"    "yes"
test_guard "B3: dotfiles CYGWIN_NT-10.0" "$DOTFILES_DIR/install.sh" "CYGWIN_NT-10.0"  "yes"
test_guard "B4: dotfiles Linux"          "$DOTFILES_DIR/install.sh" "Linux"            "no"
test_guard "B5: dotfiles Darwin"         "$DOTFILES_DIR/install.sh" "Darwin"           "no"
test_guard "B6: dotfiles MINGW%special"  "$DOTFILES_DIR/install.sh" "MINGW%special"    "yes"

# dotfiles-private/install.sh behavioral tests
test_guard "B7: dotfiles-private MINGW64_NT-10.0" "$PRIVATE_DIR/install.sh" "MINGW64_NT-10.0" "yes"
test_guard "B8: dotfiles-private MSYS_NT-10.0"    "$PRIVATE_DIR/install.sh" "MSYS_NT-10.0"    "yes"
test_guard "B9: dotfiles-private CYGWIN_NT-10.0"  "$PRIVATE_DIR/install.sh" "CYGWIN_NT-10.0"  "yes"
test_guard "B10: dotfiles-private Linux"           "$PRIVATE_DIR/install.sh" "Linux"           "no"
test_guard "B11: dotfiles-private Darwin"          "$PRIVATE_DIR/install.sh" "Darwin"          "no"
test_guard "B12: dotfiles-private MINGW%special"   "$PRIVATE_DIR/install.sh" "MINGW%special"   "yes"

# agents/install.sh behavioral tests
test_guard "B13: agents MINGW64_NT-10.0" "$AGENTS_DIR/install.sh" "MINGW64_NT-10.0" "yes"
test_guard "B14: agents MSYS_NT-10.0"    "$AGENTS_DIR/install.sh" "MSYS_NT-10.0"    "yes"
test_guard "B15: agents CYGWIN_NT-10.0"  "$AGENTS_DIR/install.sh" "CYGWIN_NT-10.0"  "yes"
test_guard "B16: agents Linux"           "$AGENTS_DIR/install.sh" "Linux"            "no"
test_guard "B17: agents Darwin"          "$AGENTS_DIR/install.sh" "Darwin"           "no"
test_guard "B18: agents MINGW%special"   "$AGENTS_DIR/install.sh" "MINGW%special"    "yes"

echo ""

# ---------------------------------------------------------------------------
# I: Idempotency — running the guard twice produces the same exit code
# ---------------------------------------------------------------------------
echo "--- I: Idempotency ---"

for repo_label in "dotfiles:$DOTFILES_DIR" "dotfiles-private:$PRIVATE_DIR" "agents:$AGENTS_DIR"; do
    label="${repo_label%%:*}"
    path="${repo_label#*:}"
    script="$path/install.sh"

    _tmpdir=$(mktemp -d)
    cat > "$_tmpdir/uname" << 'FAKEUNAME'
#!/bin/bash
echo "MINGW64_NT-10.0"
FAKEUNAME
    chmod +x "$_tmpdir/uname"

    ec1=0; ec2=0
    PATH="$_tmpdir:$PATH" run_with_timeout 5 bash "$script" >/dev/null 2>&1 || ec1=$?
    PATH="$_tmpdir:$PATH" run_with_timeout 5 bash "$script" >/dev/null 2>&1 || ec2=$?
    rm -rf "$_tmpdir"

    if [ "$ec1" -eq "$ec2" ] && [ "$ec1" -eq 1 ]; then
        ok "I1: $label/install.sh MINGW guard idempotent (both runs exit 1)"
    else
        fail "I1: $label/install.sh MINGW guard idempotent: run1=$ec1 run2=$ec2 (expected both=1)"
    fi
done

echo ""

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------
TOTAL=$((PASS + FAIL))
echo "=== Results: $PASS/$TOTAL passed ==="

if [ "$FAIL" -gt 0 ]; then
    echo "NOTE: FAIL is expected for unimplemented guard features (TDD style)."
    echo "      Syntax tests (S-group) should pass. G/B/I tests will pass after implementation."
    exit 1
fi

exit 0
