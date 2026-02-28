#!/bin/bash
# Initializa User: nire
source ~/dotfiles/bin/detectos.sh

USERNAME=nire
BUCKET=nirecom-home

if [ `whoami` != "$USERNAME" ]; then
    echo "Run with user: nire. Abort."
    exit 1
fi

# Install brew and git on macos (if not macos, will be skipped)
~/dotfiles/install/linux/brew-git.sh
# Install aws cli
~/dotfiles/install/linux/awscli.sh
~/dotfiles/install/linux/keychain.sh
~/dotfiles/install/linux/dotfiles.sh
~/dotfiles/install/linux/anyenv.sh
~/dotfiles/install/linux/zsh.sh
~/dotfiles/install/linux/starship.sh
~/dotfiles/install/linux/git-completion.sh
~/dotfiles/install/linux/vim.sh
~/dotfiles/install/linux/tmux.sh
# source-highlight.sh must come to the end, since it reloads shell
~/dotfiles/install/linux/source-highlight.sh
