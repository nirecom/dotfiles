#!/bin/sh
#ln -sf ~/dotfiles/.bashrc ~/
ln -sf ~/dotfiles/.bash_profile ~/
ln -sf ~/dotfiles/.vimrc ~/
ln -sf ~/dotfiles/.editorconfig ~/
ln -sf ~/dotfiles/.tmux.conf ~/
ln -sf ~/dotfiles/.gitconfig ~/
if [ -d ~/.atom/ ]; then
    ln -sf ~/dotfiles/.atom/config.cson ~/.atom/
    ln -sf ~/dotfiles/.atom/keymap.cson ~/.atom/
fi
if [ -d ~/.emacs.d/ ]; then
    ln -sf ~/dotfiles/init.el ~/.emacs.d/
fi
