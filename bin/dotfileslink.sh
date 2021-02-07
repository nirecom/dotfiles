#!/bin/sh
#ln -sf ~/dotfiles/.bashrc ~/ # Ubuntu has .bashrc by default
ln -sf ~/dotfiles/.bash_profile ~/
#ln -sf ~/dotfiles/.bash_logout ~/
ln -sf ~/dotfiles/.zshrc ~/
ln -sf ~/dotfiles/.vimrc ~/
#ln -sf ~/dotfiles/.vim ~/
ln -sf ~/dotfiles/.editorconfig ~/
ln -sf ~/dotfiles/.tmux.conf ~/
ln -sf ~/dotfiles/.inputrc ~/

# Git config
mkdir -p ~/.config
[ ! -e ~/.config/git ] && ln -sf ~/dotfiles/.config/git ~/.config/

# Emacs
mkdir -p ~/.emacs.d
ln -sf ~/dotfiles/.emacs.d/init.el ~/.emacs.d/
ln -sf ~/dotfiles/.emacs.d/inits ~/.emacs.d/
#ln -sf ~/dotfiles/.emacs.d/package-install.el ~/.emacs.d/
#ln -sf ~/dotfiles/.emacs.d/packages ~/.emacs.d/
mkdir -p ~/tmp
mkdir -p ~/.emacs_backup

#if [ -d ~/.atom/ ]; then
#    ln -sf ~/dotfiles/.atom/config.cson ~/.atom/
#    ln -sf ~/dotfiles/.atom/keymap.cson ~/.atom/
#fi

chmod +x ~/.ssh/ssh-add-all
