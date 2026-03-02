#!/bin/sh
# QNAP-minimal symlinks (no zsh, no tmux, no emacs)

# Auto-switch to bash on login
ln -sf ~/dotfiles/.profile_qnap ~/.profile

ln -sf ~/dotfiles/.bash_profile ~/
ln -sf ~/dotfiles/.vimrc ~/
ln -sf ~/dotfiles/.inputrc ~/
ln -sf ~/dotfiles/.editorconfig ~/

# Git config
mkdir -p ~/.config
if [ ! -e ~/.config/git ]; then
    ln -sf ~/dotfiles/.config/git ~/.config/
fi

# Git completion scripts (for prompt and tab-completion)
mkdir -p ~/completion
if [ ! -f ~/completion/git-prompt.sh ]; then
    curl -fsSL -o ~/completion/git-prompt.sh https://raw.githubusercontent.com/git/git/master/contrib/completion/git-prompt.sh
fi
if [ ! -f ~/completion/git-completion.bash ]; then
    curl -fsSL -o ~/completion/git-completion.bash https://raw.githubusercontent.com/git/git/master/contrib/completion/git-completion.bash
fi
