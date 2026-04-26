#!/bin/bash
# Initializa User: nire
: "${DOTFILES_DIR:=$(cd "$(dirname "$0")/../.." && pwd)}"
source "$DOTFILES_DIR/bin/detectos.sh"

USERNAME=nire
BUCKET=nirecom-home

if [ `whoami` != "$USERNAME" ]; then
    echo "Run with user: nire. Abort."
    exit 1
fi

# Install brew and git on macos (if not macos, will be skipped)
"$DOTFILES_DIR"/install/linux/brew-git.sh
# Install aws cli
"$DOTFILES_DIR"/install/linux/awscli.sh
"$DOTFILES_DIR"/install/linux/keychain.sh
"$DOTFILES_DIR"/install/linux/dotfiles.sh
"$DOTFILES_DIR"/install/linux/zsh.sh
"$DOTFILES_DIR"/install/linux/starship.sh
"$DOTFILES_DIR"/install/linux/git-completion.sh
"$DOTFILES_DIR"/install/linux/google-japanese-input.sh
"$DOTFILES_DIR"/install/linux/vim.sh
"$DOTFILES_DIR"/install/linux/tmux.sh
# source-highlight.sh customizes the color table
"$DOTFILES_DIR"/install/linux/source-highlight.sh
