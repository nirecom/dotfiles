#!/usr/bin/env bash
# Test: dotfiles can be cloned to any location and still bootstrap correctly.
# Verifies that dotfileslink.sh writes ~/.dotfiles_env with the right path
# and creates symlinks pointing to the actual repo location.
set -euo pipefail

DOTFILES_DIR="$(cd "$(dirname "$0")/.." && pwd)"
ERRORS=0
pass() { echo "  PASS: $1"; }
fail() { echo "  FAIL: $1"; ERRORS=$((ERRORS + 1)); }

echo "=== relocatable dotfiles tests ==="

# 1. .profile_common should NOT contain literal $HOME/dotfiles or ~/dotfiles
echo ""
echo "--- .profile_common has no hardcoded ~/dotfiles ---"
if grep -qE '\$HOME/dotfiles[/"]|~/dotfiles[/"]' "$DOTFILES_DIR/.profile_common"; then
    fail ".profile_common still contains hardcoded \$HOME/dotfiles or ~/dotfiles"
    grep -nE '\$HOME/dotfiles[/"]|~/dotfiles[/"]' "$DOTFILES_DIR/.profile_common"
else
    pass ".profile_common uses \$DOTFILES_DIR consistently"
fi

# 2. install.sh self-resolves DOTFILES_DIR
echo ""
echo "--- install.sh self-resolves DOTFILES_DIR ---"
if grep -qE 'DOTFILES_DIR="\$\(cd "\$\(dirname "\$0"\)" && pwd\)"' "$DOTFILES_DIR/install.sh"; then
    pass "install.sh derives DOTFILES_DIR from its own location"
else
    fail "install.sh does not self-resolve DOTFILES_DIR"
fi

# 3. install/linux/dotfileslink.sh generates ~/.dotfiles_env with the resolved path.
echo ""
echo "--- dotfileslink.sh writes ~/.dotfiles_env from a relocated repo ---"
TMP_HOME="$(mktemp -d)"
TMP_REPO="$TMP_HOME/anywhere/dotfiles"
mkdir -p "$TMP_REPO"
# Copy files needed by dotfileslink.sh (avoid full clone for speed).
mkdir -p "$TMP_REPO/install/linux" "$TMP_REPO/bin" "$TMP_REPO/.config/git" "$TMP_REPO/.emacs.d"
cp "$DOTFILES_DIR/install/linux/dotfileslink.sh" "$TMP_REPO/install/linux/"
cp "$DOTFILES_DIR/bin/detectos.sh" "$TMP_REPO/bin/"
# Create stub files that dotfileslink.sh symlinks (touch is enough for ln -sf).
for f in .bash_profile .zshrc .vimrc .editorconfig .tmux.conf .inputrc filetype.vim; do
    touch "$TMP_REPO/$f"
done
touch "$TMP_REPO/.config/starship.toml"
touch "$TMP_REPO/.emacs.d/init.el"
mkdir -p "$TMP_REPO/.emacs.d/inits"

HOME="$TMP_HOME" bash "$TMP_REPO/install/linux/dotfileslink.sh" >/dev/null 2>&1 || true

if [ -f "$TMP_HOME/.dotfiles_env" ]; then
    pass "~/.dotfiles_env was generated"
    if grep -qF "DOTFILES_DIR=\"$TMP_REPO\"" "$TMP_HOME/.dotfiles_env"; then
        pass "~/.dotfiles_env points to the relocated repo"
    else
        fail "~/.dotfiles_env does not point to $TMP_REPO"
        cat "$TMP_HOME/.dotfiles_env"
    fi
else
    fail "~/.dotfiles_env was not generated"
fi

# Verify symlinks resolve into the relocated repo (not the user's real ~/dotfiles).
LINK_TARGET="$(readlink "$TMP_HOME/.bash_profile" 2>/dev/null || true)"
if [ "$LINK_TARGET" = "$TMP_REPO/.bash_profile" ]; then
    pass "~/.bash_profile symlink points to relocated repo"
else
    fail "~/.bash_profile symlink target was '$LINK_TARGET', expected '$TMP_REPO/.bash_profile'"
fi

rm -rf "$TMP_HOME"

# 4. .bash_profile and .zshrc load ~/.dotfiles_env
echo ""
echo "--- shell rc files load ~/.dotfiles_env ---"
for rc in .bash_profile .zshrc; do
    if grep -q '\.dotfiles_env' "$DOTFILES_DIR/$rc"; then
        pass "$rc sources ~/.dotfiles_env"
    else
        fail "$rc does not source ~/.dotfiles_env"
    fi
done

echo ""
if [ "$ERRORS" -eq 0 ]; then
    echo "All relocatable tests passed."
    exit 0
else
    echo "FAILED: $ERRORS error(s)"
    exit 1
fi
