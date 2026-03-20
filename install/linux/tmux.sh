#!/bin/bash
# Install tmux
source ~/dotfiles/bin/detectos.sh

if type tmux >/dev/null 2>&1; then
    echo "tmux is already installed: $(tmux -V)"
    exit 0
fi

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
