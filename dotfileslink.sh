#!/bin/sh
ln -sf ~/dotfiles/.bashrc ~/
#ln -sf ~/dotfiles/.bash_profile ~/
ln -sf ~/dotfiles/.vimrc ~/
ln -sf ~/dotfiles/.editorconfig ~/
ln -sf ~/dotfiles/.tmux.conf ~/
ln -sf ~/dotfiles/.gitconfig ~/
mkdir -p ~/.atom/
ln -sf ~/dotfiles/.atom/config.cson ~/.atom/
ln -sf ~/dotfiles/.atom/keymap.cson ~/.atom/
