#!/bin/bash
# tests/main-brew-bootstrap.sh - Tests for install/linux/brew-bootstrap.sh
# Tests: install/linux/brew-bootstrap.sh, install.sh
# Tags: installer, homebrew, brew-bootstrap, macos, scope:common, pwsh-not-required
#
# The unit under test does NOT exist yet (issue #331 fix). It is a SOURCED unit
# defining a `brew_bootstrap` function. Static + dynamic tests skip gracefully
# until the file is created (fail-before-fix: pending-implementation skips are
# the current-state signal for this brand-new unit).
#
# L3 gap (what this test does NOT catch):
# - Warm-path branch SELECTION between /opt/homebrew (Apple Silicon) and
#   /usr/local (Intel) cannot be faithfully forced in CI: the unit hardcodes
#   those absolute paths, which cannot be mocked under a temp dir without root.
#   The best-effort warm-path test below only runs on a real Mac that already
#   has brew at one of those prefixes; it cannot assert which branch was taken.
# - Cold path (real Rosetta install via softwareupdate + the official Homebrew
#   installer via curl) is destructive/network-bound and only verifiable on real
#   macOS hardware with Homebrew genuinely absent.
# Closest-to-action mitigation: this gap is checked at WORKFLOW_USER_VERIFIED
# preflight via bin/check-verification-gate.sh category: installer.

set -euo pipefail

DOTFILES_DIR="$(cd "$(dirname "$0")/.." && pwd)"
SCRIPT="$DOTFILES_DIR/install/linux/brew-bootstrap.sh"

PASS=0
FAIL=0
SKIP=0

pass() { echo "  PASS: $1"; PASS=$((PASS + 1)); }
fail() { echo "  FAIL: $1"; FAIL=$((FAIL + 1)); }
skip() { echo "  SKIP: $1 (source unit not yet created)"; SKIP=$((SKIP + 1)); }

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
# STATIC TESTS — assert the planned contract of brew-bootstrap.sh.
# Table-driven: one required grep pattern per row (test-design table pattern).
# Skips entirely when the unit does not exist yet.
# ---------------------------------------------------------------------------
echo "=== Static: brew-bootstrap.sh planned contract ==="
if [ ! -f "$SCRIPT" ]; then
    skip "static contract checks — brew-bootstrap.sh absent"
else
    while IFS='|' read -r name pattern; do
        [[ -z "$name" || "$name" =~ ^[[:space:]]*# ]] && continue
        name="${name//[[:space:]]/}"
        pattern="${pattern#"${pattern%%[![:space:]]*}"}"   # ltrim
        pattern="${pattern%"${pattern##*[![:space:]]}"}"   # rtrim
        if grep -Eq "$pattern" "$SCRIPT"; then
            pass "static: $name"
        else
            fail "static: $name (pattern /$pattern/ not found)"
        fi
    done <<'TABLE'
defines-brew_bootstrap-function | brew_bootstrap\s*\(\)
sources-detectos               | detectos\.sh
non-macos-returns-0            | return 0
prints-bootstrap-header        | Bootstrapping Homebrew
warm-apple-silicon-path        | /opt/homebrew/bin/brew
warm-intel-path                | /usr/local/bin/brew
applies-shellenv               | brew shellenv
cold-rosetta-precheck          | pgrep.*oahd
cold-rosetta-install           | softwareupdate --install-rosetta
cold-official-installer        | Homebrew/install
TABLE

    # Guard: sourcing must NOT auto-invoke brew_bootstrap. Flag a top-level
    # (column-0) bare call, which would violate "source has no side effect".
    if grep -Eq '^brew_bootstrap[[:space:]]*$' "$SCRIPT"; then
        fail "static: sourcing auto-invokes brew_bootstrap (top-level call present)"
    else
        pass "static: no top-level auto-invocation of brew_bootstrap"
    fi
fi
echo ""

# ---------------------------------------------------------------------------
# DYNAMIC BB-1: non-macOS is a no-op — returns 0, prints no header, invokes no
# installer commands (softwareupdate / curl / brew never called).
# uname is mocked to report Linux so the unit-sourced detectos.sh yields a
# non-macOS OSDIST. Fully hermetic; runs on any host once the unit exists.
# ---------------------------------------------------------------------------
echo "=== BB-1: non-macOS — no-op, returns 0, no installer calls ==="
if [ ! -f "$SCRIPT" ]; then
    skip "BB-1"
else
    MOCK="$TMPBASE/bb1"
    mkdir -p "$MOCK"
    cat > "$MOCK/uname" <<'EOF'
#!/bin/bash
case "${1:-}" in
    -m) echo "x86_64" ;;
    *)  echo "Linux" ;;
esac
EOF
    for cmd in softwareupdate curl brew pgrep; do
        cat > "$MOCK/$cmd" <<EOF
#!/bin/bash
echo "$cmd" >> "$MOCK/CALLED"
exit 0
EOF
    done
    chmod +x "$MOCK"/*

    output=$(run_with_timeout env PATH="$MOCK:$PATH" DOTFILES_DIR="$DOTFILES_DIR" \
        bash -c 'source "$1"; brew_bootstrap; echo "RC=$?"' _ "$SCRIPT" 2>&1) \
        && exit_code=$? || exit_code=$?

    ok=1
    echo "$output" | grep -q 'RC=0' || ok=0
    [ "$exit_code" -eq 0 ] || ok=0
    if echo "$output" | grep -qi 'Bootstrapping Homebrew'; then ok=0; fi
    if [ -f "$MOCK/CALLED" ]; then ok=0; fi

    if [ "$ok" -eq 1 ]; then
        pass "BB-1: non-macOS returns 0, no header, no installer invocation"
    else
        called="$( [ -f "$MOCK/CALLED" ] && tr '\n' ',' < "$MOCK/CALLED" || echo none )"
        fail "BB-1: exit=$exit_code called=[$called] output=[$output]"
    fi
fi
echo ""

# ---------------------------------------------------------------------------
# DYNAMIC BB-2: idempotent source — sourcing defines the function but does NOT
# invoke it. Verified by declare -F plus absence of any installer sentinel.
# uname mocked to Darwin so an accidental invocation WOULD hit the brew sentinel.
# ---------------------------------------------------------------------------
echo "=== BB-2: sourcing does not auto-invoke brew_bootstrap ==="
if [ ! -f "$SCRIPT" ]; then
    skip "BB-2"
else
    MOCK="$TMPBASE/bb2"
    mkdir -p "$MOCK"
    cat > "$MOCK/uname" <<'EOF'
#!/bin/bash
case "${1:-}" in
    -m) echo "arm64" ;;
    *)  echo "Darwin" ;;
esac
EOF
    cat > "$MOCK/brew" <<EOF
#!/bin/bash
echo "brew" >> "$MOCK/CALLED"
exit 0
EOF
    chmod +x "$MOCK"/*

    output=$(run_with_timeout env PATH="$MOCK:$PATH" DOTFILES_DIR="$DOTFILES_DIR" \
        bash -c 'source "$1"; declare -F brew_bootstrap >/dev/null && echo DEFINED' _ "$SCRIPT" 2>&1) \
        && exit_code=$? || exit_code=$?

    if echo "$output" | grep -q 'DEFINED' && [ ! -f "$MOCK/CALLED" ]; then
        pass "BB-2: function defined, not invoked at source time"
    else
        called="$( [ -f "$MOCK/CALLED" ] && cat "$MOCK/CALLED" || echo none )"
        fail "BB-2: exit=$exit_code called=[$called] output=[$output]"
    fi
fi
echo ""

# ---------------------------------------------------------------------------
# DYNAMIC BB-3: warm-path best-effort (real macOS with brew already present).
# Cannot mock the hardcoded absolute paths without root, so this only runs on a
# real Mac that already has brew at /opt/homebrew or /usr/local. The real
# contract asserted here is that shellenv was APPLIED (HOMEBREW_PREFIX exported
# into the caller's env). The decorative "--- Bootstrapping Homebrew ---" header
# is intentionally NOT asserted here: it renders only under a TTY (C_BOLD is
# empty when stdout is captured), and its presence in the source is already
# covered by the static contract table above. Skips on Linux CI / a Mac without
# brew. See the file-header L3 gap block.
# ---------------------------------------------------------------------------
echo "=== BB-3: warm-path applies shellenv (real macOS only) ==="
if [ ! -f "$SCRIPT" ]; then
    skip "BB-3"
elif [ "$(uname -s)" != "Darwin" ]; then
    echo "  SKIP: BB-3 (not macOS — warm-path is hardware-specific)"
    SKIP=$((SKIP + 1))
elif [ ! -x /opt/homebrew/bin/brew ] && [ ! -x /usr/local/bin/brew ]; then
    echo "  SKIP: BB-3 (no brew at a known absolute prefix on this host)"
    SKIP=$((SKIP + 1))
else
    output=$(run_with_timeout env DOTFILES_DIR="$DOTFILES_DIR" \
        bash -c 'source "$1"; brew_bootstrap; echo "PREFIX=${HOMEBREW_PREFIX:-unset}"' _ "$SCRIPT" 2>&1) \
        && exit_code=$? || exit_code=$?

    if [ "$exit_code" -eq 0 ] && echo "$output" | grep -q 'PREFIX=/'; then
        pass "BB-3: warm-path exports HOMEBREW_PREFIX (shellenv applied)"
    else
        fail "BB-3: exit=$exit_code output=[$output]"
    fi
fi
echo ""

# ---------------------------------------------------------------------------
# SKIPPED-Because — scenarios not implementable at this test layer.
# ---------------------------------------------------------------------------
# SKIPPED: cold path — real Rosetta install (softwareupdate --install-rosetta)
#   followed by the official Homebrew installer via curl, then shellenv.
# Because: destructive + network-bound + requires Homebrew genuinely absent;
#   L2 cannot run the real installer without mutating the host.
# L3 gap: only a fresh real Mac (Apple Silicon and Intel separately) exercises
#   the Rosetta precheck branch and the correct post-install shellenv prefix.
#
# SKIPPED: warm-path branch SELECTION (Apple Silicon /opt/homebrew vs Intel
#   /usr/local) when both/neither are present.
# Because: the unit hardcodes those absolute paths; a temp-dir mock is never on
#   the code path, and writing under /opt or /usr/local needs root in CI.
# L3 gap: real hardware of each architecture is required to confirm the correct
#   prefix is chosen and shellenv exported.

# ---------------------------------------------------------------------------
# SUMMARY
# ---------------------------------------------------------------------------
echo "=== Results: $PASS passed, $FAIL failed, $SKIP skipped ==="
[ "$FAIL" -eq 0 ] || exit 1
