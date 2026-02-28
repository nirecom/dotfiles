#!/bin/bash
# Install tmux
source ~/dotfiles/bin/detectos.sh

echo "Installing tmux..."
case "$OSDIST" in
    "ubuntu" )
        sudo apt update
        sudo apt install -y tmux
        ;;
    "macos" )
        brew update
        brew install tmux
        ;;
esac
