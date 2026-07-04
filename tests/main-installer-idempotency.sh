#!/bin/bash
# Test: installer scripts have idempotency checks (skip if already installed)
# Each script under install/linux/ that installs a tool should check before reinstalling.
# Tests: install/linux/brew-bootstrap.sh, install/linux/brew-git.sh, install.sh
# Tags: installer, idempotency, homebrew, orthogonality, scope:common, pwsh-not-required
#
# FAIL-BEFORE-FIX (issue #331): the Rosetta idempotency check is asserted against
# install/linux/brew-bootstrap.sh, which does NOT exist until the #331 fix lands
# (the Rosetta + official-installer logic moves out of brew-git.sh into the new
# shared brew-bootstrap.sh unit). Until then this assertion FAILS — that is the
# fail-before-fix signal, not a test defect.

set -e
PASS=0
FAIL=0
: "${DOTFILES_DIR:=$(cd "$(dirname "$0")/.." && pwd)}"
SCRIPT_DIR="$DOTFILES_DIR/install/linux"

fail() {
    echo "FAIL: $1"
    FAIL=$((FAIL + 1))
}

pass() {
    echo "PASS: $1"
    PASS=$((PASS + 1))
}

# brew-bootstrap.sh: owns the Rosetta (pgrep oahd) precheck after #331 moved the
# bootstrap logic out of brew-git.sh into the shared sourced unit.
# (Retargeted from brew-git.sh — see file header fail-before-fix note.)
if grep -q 'pgrep.*oahd\|arch -x86_64\|rosetta.*installed' "$SCRIPT_DIR/brew-bootstrap.sh" 2>/dev/null; then
    pass "brew-bootstrap.sh has Rosetta idempotency check"
else
    fail "brew-bootstrap.sh missing Rosetta idempotency check (fail-before-fix: brew-bootstrap.sh not yet created)"
fi

# brew-git.sh: retains the git idempotency check (git install stays local to it).
if grep -q 'brew list git\|type git\|command -v git' "$SCRIPT_DIR/brew-git.sh"; then
    pass "brew-git.sh has git idempotency check"
else
    fail "brew-git.sh missing git idempotency check"
fi

# brew-git.sh must NOT contain Rosetta logic post-refactor (moved to brew-bootstrap.sh)
# FAIL-BEFORE-FIX: brew-git.sh retains Rosetta logic until Step 3 of #331 lands.
if grep -qE 'pgrep.*oahd|softwareupdate.*rosetta' "$SCRIPT_DIR/brew-git.sh" 2>/dev/null; then
    fail "brew-git.sh still contains Rosetta logic (fail-before-fix: Step 3 refactor not yet landed)"
else
    pass "brew-git.sh Rosetta logic correctly removed"
fi

# tmux.sh: should check if tmux is already installed
if grep -q 'type tmux\|command -v tmux\|brew list tmux' "$SCRIPT_DIR/tmux.sh"; then
    pass "tmux.sh has idempotency check"
else
    fail "tmux.sh missing idempotency check"
fi

# source-highlight.sh: should check if source-highlight is already installed
if grep -q 'type source-highlight\|command -v source-highlight\|brew list source-highlight' "$SCRIPT_DIR/source-highlight.sh"; then
    pass "source-highlight.sh has idempotency check"
else
    fail "source-highlight.sh missing idempotency check"
fi

# vim.sh: should check if pathogen.vim already exists before downloading
if grep -q 'pathogen.vim' "$SCRIPT_DIR/vim.sh" && grep -q '\-f.*pathogen\|! -f.*pathogen\|-e.*pathogen' "$SCRIPT_DIR/vim.sh"; then
    pass "vim.sh has pathogen idempotency check"
else
    fail "vim.sh missing pathogen idempotency check"
fi

# git-completion.sh: filename in check must not have typo (git-comletion vs git-completion)
if grep -q 'git-comletion' "$SCRIPT_DIR/git-completion.sh"; then
    fail "git-completion.sh has typo 'git-comletion' (missing 'p')"
else
    pass "git-completion.sh has correct filename in check"
fi

# keychain.sh: should already have check (verify it's still there)
if grep -q 'command -v keychain\|type keychain' "$SCRIPT_DIR/keychain.sh"; then
    pass "keychain.sh has idempotency check"
else
    fail "keychain.sh missing idempotency check"
fi

# starship.sh: should already have check (verify it's still there)
if grep -q 'type starship\|command -v starship' "$SCRIPT_DIR/starship.sh"; then
    pass "starship.sh has idempotency check"
else
    fail "starship.sh missing idempotency check"
fi

# source-highlight.sh: must not exec $SHELL (blocks subsequent install steps)
if grep -q 'exec.*SHELL' "$SCRIPT_DIR/source-highlight.sh"; then
    fail "source-highlight.sh contains exec \$SHELL (blocks install.sh flow)"
else
    pass "source-highlight.sh does not exec \$SHELL"
fi

# install.sh: base block must not have macOS special case (orthogonality with install.ps1)
# Post-#331 the brew bootstrap is invoked UNCONDITIONALLY from install.sh
# (`source .../brew-bootstrap.sh; brew_bootstrap`) with the macOS guard living
# INSIDE the sourced unit — so this assertion must continue to hold.
INSTALL_SH="$DOTFILES_DIR/install.sh"
if grep -q 'OSDIST.*macos' "$INSTALL_SH"; then
    fail "install.sh has macOS special case in base block (breaks orthogonality)"
else
    pass "install.sh has no macOS special case (orthogonal with install.ps1)"
fi

# install.sh must source and call brew_bootstrap (unconditional wiring, post-fix)
# FAIL-BEFORE-FIX: install.sh does not wire up brew_bootstrap until Step 2 lands.
INSTALL_SH="$DOTFILES_DIR/install.sh"
if grep -q 'brew-bootstrap\.sh' "$INSTALL_SH" && grep -q 'brew_bootstrap' "$INSTALL_SH"; then
    pass "install.sh sources and calls brew_bootstrap (unconditional)"
else
    fail "install.sh missing brew_bootstrap wiring (fail-before-fix: Step 2 not yet landed)"
fi

echo ""
echo "Results: $PASS passed, $FAIL failed"
[ "$FAIL" -eq 0 ]
