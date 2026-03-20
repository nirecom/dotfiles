#!/bin/bash
# Test: installer scripts have idempotency checks (skip if already installed)
# Each script under install/linux/ that installs a tool should check before reinstalling.

set -e
PASS=0
FAIL=0
SCRIPT_DIR=~/dotfiles/install/linux

fail() {
    echo "FAIL: $1"
    FAIL=$((FAIL + 1))
}

pass() {
    echo "PASS: $1"
    PASS=$((PASS + 1))
}

# brew-git.sh: should check Rosetta (pgrep oahd) and git (brew list git)
if grep -q 'pgrep.*oahd\|arch -x86_64\|rosetta.*installed' "$SCRIPT_DIR/brew-git.sh"; then
    pass "brew-git.sh has Rosetta idempotency check"
else
    fail "brew-git.sh missing Rosetta idempotency check"
fi

if grep -q 'brew list git\|type git\|command -v git' "$SCRIPT_DIR/brew-git.sh"; then
    pass "brew-git.sh has git idempotency check"
else
    fail "brew-git.sh missing git idempotency check"
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

echo ""
echo "Results: $PASS passed, $FAIL failed"
[ "$FAIL" -eq 0 ]
