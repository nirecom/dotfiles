#!/bin/bash
# tests/main-gh.sh - Tests for install/linux/gh.sh
# Tests are divided into:
#   - Static: grep-based pattern checks (run even when script does not exist)
#   - Dynamic: actual script execution with mocked commands
# Dynamic tests skip gracefully when the source script is not yet created.

set -euo pipefail

DOTFILES_DIR="$(cd "$(dirname "$0")/.." && pwd)"
SCRIPT="$DOTFILES_DIR/install/linux/gh.sh"

PASS=0
FAIL=0
SKIP=0

pass() { echo "  PASS: $1"; PASS=$((PASS + 1)); }
fail() { echo "  FAIL: $1"; FAIL=$((FAIL + 1)); }
skip() { echo "  SKIP: $1 (source script not yet created)"; SKIP=$((SKIP + 1)); }

# Global temp dir — cleaned on exit
TMPBASE=$(mktemp -d)
trap "rm -rf '$TMPBASE'" EXIT

# ---------------------------------------------------------------------------
# Portable timeout wrapper (macOS lacks the timeout command)
# ---------------------------------------------------------------------------
run_with_timeout() {
    if command -v timeout >/dev/null 2>&1; then
        timeout 120 "$@"
    else
        perl -e 'alarm 120; exec @ARGV' -- "$@"
    fi
}

# ---------------------------------------------------------------------------
# Helper: create a mock command directory with stub binaries.
# Usage: make_mock_env <dir>
# Writes no-op stubs for apt, apt-get, sudo, dpkg, gpg, curl, brew, tee.
# Does NOT create a gh binary (caller adds it separately if needed).
# ---------------------------------------------------------------------------
make_mock_env() {
    local mock_dir="$1"
    mkdir -p "$mock_dir"

    # Mock: sudo — transparently runs remaining args
    cat > "$mock_dir/sudo" <<'EOF'
#!/bin/bash
shift 0; "$@" 2>/dev/null || true
EOF

    # Mock: apt and apt-get — no-op installs
    for cmd in apt apt-get; do
        cat > "$mock_dir/$cmd" <<'EOF'
#!/bin/bash
exit 0
EOF
    done

    # Mock: dpkg, gpg, curl, brew, tee — all no-op
    for cmd in dpkg gpg curl brew tee; do
        cat > "$mock_dir/$cmd" <<'EOF'
#!/bin/bash
exit 0
EOF
    done

    chmod +x "$mock_dir"/*
}

# ---------------------------------------------------------------------------
# Helper: create a fake gh binary in a directory.
# Usage: make_gh_mock <dir>
# ---------------------------------------------------------------------------
make_gh_mock() {
    local dir="$1"
    cat > "$dir/gh" <<'EOF'
#!/bin/bash
if [ "${1:-}" = "--version" ]; then echo "gh version 2.40.0 (2024-01-01)"; fi
exit 0
EOF
    chmod +x "$dir/gh"
}


# ---------------------------------------------------------------------------
# STATIC TESTS — check source file patterns (skip if file not created yet)
# ---------------------------------------------------------------------------

echo "=== Static: source script structure ==="

if [ ! -f "$SCRIPT" ]; then
    skip "gh.sh does not exist — all static tests skipped"
else
    # Normal: sources colors.sh
    if grep -q 'source.*colors\.sh' "$SCRIPT" || grep -q '\. .*colors\.sh' "$SCRIPT"; then
        pass "sources colors.sh"
    else
        fail "does not source colors.sh"
    fi

    # Normal: sources detectos.sh
    if grep -q 'source.*detectos\.sh' "$SCRIPT" || grep -q '\. .*detectos\.sh' "$SCRIPT"; then
        pass "sources detectos.sh"
    else
        fail "does not source detectos.sh"
    fi

    # Normal: has idempotency check (command -v gh)
    if grep -q 'command -v gh\|type gh' "$SCRIPT"; then
        pass "has idempotency check for gh"
    else
        fail "missing idempotency check"
    fi

    # Normal: prints 'already installed' message
    if grep -q 'already installed' "$SCRIPT"; then
        pass "has already-installed message"
    else
        fail "missing already-installed message"
    fi

    # Normal: handles ubuntu case
    if grep -qE '"ubuntu"|ubuntu\)' "$SCRIPT"; then
        pass "handles ubuntu case"
    else
        fail "missing ubuntu case"
    fi

    # Normal: handles macos case
    if grep -qE '"macos"|macos\)' "$SCRIPT"; then
        pass "handles macos case"
    else
        fail "missing macos case"
    fi

    # Normal: uses brew install for macOS
    if grep -q 'brew install' "$SCRIPT"; then
        pass "uses brew install for macOS"
    else
        fail "missing brew install for macOS"
    fi

    # Error: handles unsupported OS (exits 1)
    if grep -q 'exit 1' "$SCRIPT"; then
        pass "has exit 1 for unsupported OS"
    else
        fail "missing exit 1 for unsupported OS"
    fi

    # Normal: uses C_GRAY for already-installed output
    if grep -q 'C_GRAY' "$SCRIPT"; then
        pass "uses C_GRAY for already-installed output"
    else
        fail "missing C_GRAY color variable"
    fi

    # Normal: uses C_GREEN for success output
    if grep -q 'C_GREEN' "$SCRIPT"; then
        pass "uses C_GREEN for success output"
    else
        fail "missing C_GREEN color variable"
    fi
fi

echo ""

# ---------------------------------------------------------------------------
# DYNAMIC TESTS — run the actual script with mocked environment
# All dynamic tests skip gracefully when the script doesn't exist yet.
# ---------------------------------------------------------------------------

# GH-L-2: When gh is already installed, prints gray "already installed" and exits 0
echo "=== GH-L-2: Already installed — prints gray message, exits 0 ==="
if [ ! -f "$SCRIPT" ]; then
    skip "GH-L-2"
else
    MOCK_L2="$TMPBASE/mock_l2"
    make_mock_env "$MOCK_L2"
    make_gh_mock "$MOCK_L2"  # gh IS present

    output=$(run_with_timeout env PATH="$MOCK_L2:$PATH" DOTFILES_DIR="$DOTFILES_DIR" bash "$SCRIPT" 2>&1) \
        && exit_code=$? || exit_code=$?

    if [ "$exit_code" -eq 0 ] && echo "$output" | grep -qi 'already installed'; then
        pass "GH-L-2: exits 0 and prints 'already installed'"
    else
        fail "GH-L-2: exit_code=$exit_code output=[$output]"
    fi
fi

echo ""

# GH-L-3: Idempotency — running twice when already installed produces same output
echo "=== GH-L-3: Idempotency — running twice gives same output ==="
if [ ! -f "$SCRIPT" ]; then
    skip "GH-L-3"
else
    MOCK_L3="$TMPBASE/mock_l3"
    make_mock_env "$MOCK_L3"
    make_gh_mock "$MOCK_L3"  # gh IS present

    out1=$(run_with_timeout env PATH="$MOCK_L3:$PATH" DOTFILES_DIR="$DOTFILES_DIR" bash "$SCRIPT" 2>&1) || true
    out2=$(run_with_timeout env PATH="$MOCK_L3:$PATH" DOTFILES_DIR="$DOTFILES_DIR" bash "$SCRIPT" 2>&1) || true

    if [ "$out1" = "$out2" ]; then
        pass "GH-L-3: output is identical on second run"
    else
        fail "GH-L-3: first=[$out1] second=[$out2]"
    fi
fi

echo ""

# GH-L-1: When gh is not installed (ubuntu), script installs and exits 0
# Strategy: run gh.sh via a wrapper script that overrides the `command` builtin
# using a shell function. The wrapper intercepts `command -v gh` and makes it
# fail (simulating gh not being installed), while letting all other `command`
# calls pass through. After apt "install", the wrapper allows gh to succeed.
echo "=== GH-L-1: Not installed (ubuntu) — installs and exits 0 ==="
if [ ! -f "$SCRIPT" ]; then
    skip "GH-L-1"
else
    MOCK_L1="$TMPBASE/mock_l1"
    make_mock_env "$MOCK_L1"

    # State file: 0 = gh not yet "installed", 1 = installed
    GH_STATE_L1="$TMPBASE/gh_state_l1"
    echo "0" > "$GH_STATE_L1"

    # Mock apt: when called with "install", flip the state so gh is "available"
    cat > "$MOCK_L1/apt" <<EOF
#!/bin/bash
if echo "\$@" | grep -q 'install'; then
    echo "1" > "$GH_STATE_L1"
fi
exit 0
EOF
    chmod +x "$MOCK_L1/apt"

    # Wrapper script: sources gh.sh with 'command' overridden to intercept gh checks
    GH_WRAPPER_L1="$TMPBASE/gh_wrapper_l1.sh"
    cat > "$GH_WRAPPER_L1" <<EOF
#!/bin/bash
# Override 'command' builtin: make 'command -v gh' fail until apt runs
GH_STATE="$GH_STATE_L1"
command() {
    if [ "\${1:-}" = "-v" ] && [ "\${2:-}" = "gh" ]; then
        state=\$(cat "\$GH_STATE" 2>/dev/null || echo 0)
        if [ "\$state" -eq 0 ]; then
            return 1  # gh not found
        fi
        echo "gh"
        return 0
    fi
    builtin command "\$@"
}
export -f command

# Override gh itself: fails until state=1
gh() {
    state=\$(cat "$GH_STATE_L1" 2>/dev/null || echo 0)
    if [ "\$state" -eq 0 ]; then exit 127; fi
    if [ "\${1:-}" = "--version" ]; then echo "gh version 2.40.0 (2024-01-01)"; fi
    return 0
}
export -f gh

export PATH="$MOCK_L1:\$PATH"
export DOTFILES_DIR="$DOTFILES_DIR"

source "$SCRIPT"
EOF
    chmod +x "$GH_WRAPPER_L1"

    output=$(run_with_timeout bash "$GH_WRAPPER_L1" 2>&1) \
        && exit_code=$? || exit_code=$?

    if [ "$exit_code" -eq 0 ]; then
        pass "GH-L-1: exits 0 on successful install (ubuntu)"
    else
        fail "GH-L-1: exits $exit_code; output=[$output]"
    fi
fi

echo ""

# GH-L-4: When OS is unsupported, exits 1 with error
# gh.sh sources detectos.sh which calls uname to determine OSDIST.
# We mock uname to return an unrecognized OS name so detectos.sh sets OSDIST="".
# The case statement in gh.sh falls through to "*" which exits 1.
# We also simulate gh not installed (via command override) so we reach the case.
echo "=== GH-L-4: Unsupported OS — exits 1 with error on stderr ==="
if [ ! -f "$SCRIPT" ]; then
    skip "GH-L-4"
else
    MOCK_L4="$TMPBASE/mock_l4"
    make_mock_env "$MOCK_L4"

    # Mock uname to return an unrecognized OS string so detectos.sh leaves OSDIST empty
    cat > "$MOCK_L4/uname" <<'EOF'
#!/bin/bash
# Return unrecognized OS name — detectos.sh will set OSDIST=""
echo "UnknownOS"
EOF
    chmod +x "$MOCK_L4/uname"

    # Wrapper: override command so gh appears not installed
    GH_WRAPPER_L4="$TMPBASE/gh_wrapper_l4.sh"
    cat > "$GH_WRAPPER_L4" <<EOF
#!/bin/bash
command() {
    if [ "\${1:-}" = "-v" ] && [ "\${2:-}" = "gh" ]; then
        return 1  # gh not found
    fi
    builtin command "\$@"
}
export -f command
export PATH="$MOCK_L4:\$PATH"
export DOTFILES_DIR="$DOTFILES_DIR"
source "$SCRIPT"
EOF
    chmod +x "$GH_WRAPPER_L4"

    output=$(run_with_timeout bash "$GH_WRAPPER_L4" 2>&1) \
        && exit_code=$? || exit_code=$?

    if [ "$exit_code" -ne 0 ]; then
        pass "GH-L-4: exits non-zero for unsupported OS (exit_code=$exit_code)"
    else
        fail "GH-L-4: expected non-zero exit but got 0; output=[$output]"
    fi
fi

echo ""

# GH-L-5: When stdout is piped (not a TTY), ANSI codes are absent from output
echo "=== GH-L-5: Piped stdout — no ANSI escape codes in output ==="
if [ ! -f "$SCRIPT" ]; then
    skip "GH-L-5"
else
    MOCK_L5="$TMPBASE/mock_l5"
    make_mock_env "$MOCK_L5"
    make_gh_mock "$MOCK_L5"  # gh IS present (triggers already-installed path)

    # Pipe to cat to simulate non-TTY stdout
    output=$(run_with_timeout env PATH="$MOCK_L5:$PATH" DOTFILES_DIR="$DOTFILES_DIR" bash "$SCRIPT" 2>&1 | cat)

    # ANSI escape codes start with ESC (\033[)
    if printf '%s' "$output" | grep -qP '\x1b\[' 2>/dev/null \
       || printf '%s' "$output" | grep -q $'\033'; then
        fail "GH-L-5: ANSI codes found in piped output: [$output]"
    else
        pass "GH-L-5: no ANSI codes when stdout is piped"
    fi
fi

echo ""

# ---------------------------------------------------------------------------
# SUMMARY
# ---------------------------------------------------------------------------
echo "=== Results: $PASS passed, $FAIL failed, $SKIP skipped ==="
[ "$FAIL" -eq 0 ] || exit 1
