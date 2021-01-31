#!/bin/sh
#ln -sf ~/dotfiles/.bashrc ~/ # Ubuntu has .bashrc by default
ln -sf ~/dotfiles/.bash_profile ~/
ln -sf ~/dotfiles/.bash_logout ~/
ln -sf ~/dotfiles/.zshrc ~/
ln -sf ~/dotfiles/.vimrc ~/
#ln -sf ~/dotfiles/.vim ~/
ln -sf ~/dotfiles/.editorconfig ~/
ln -sf ~/dotfiles/.tmux.conf ~/
ln -sf ~/dotfiles/.gitconfig ~/
ln -sf ~/dotfiles/.config ~/
ln -sf ~/dotfiles/.inputrc ~/

# Emacs
if [ ! -d ~/.emacs.d/ ]; then mkdir ~/.emacs.d; fi
ln -sf ~/dotfiles/.emacs.d/init.el ~/.emacs.d/
ln -sf ~/dotfiles/.emacs.d/packages ~/.emacs.d/packages
if [ ! -d ~/tmp ]; then mkdir ~/tmp; fi
if [ ! -d ~/.emacs_backup ]; then mkdir ~/.emacs_backup; fi

if [ -d ~/.atom/ ]; then
    ln -sf ~/dotfiles/.atom/config.cson ~/.atom/
    ln -sf ~/dotfiles/.atom/keymap.cson ~/.atom/
fi

chmod +x ~/.ssh/ssh-add-all
