#!/bin/bash
# Install brew and git
: "${DOTFILES_DIR:=$(cd "$(dirname "$0")/../.." && pwd)}"
source "$DOTFILES_DIR/bin/detectos.sh"

if [ "$OSDIST" != "macos" ]; then
    echo "Skipping brew / git installation only for macos..."
    exit 1
fi

# Ensure brew is installed and usable in this process (Rosetta + installer + shellenv).
# shellcheck source=/dev/null
source "$DOTFILES_DIR/install/linux/brew-bootstrap.sh"
brew_bootstrap

# two dependencies of git
# https://github.com/Homebrew/discussions/discussions/439
#brew reinstall gettext pcre2

if ! brew list git &>/dev/null; then
    brew install git
else
    echo "git is already installed: $(git --version)"
fi
