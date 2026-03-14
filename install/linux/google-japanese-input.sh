#!/bin/bash
# Install Google Japanese Input
source ~/dotfiles/bin/detectos.sh

case "$OSDIST" in
    "macos" )
        if brew list --cask google-japanese-ime >/dev/null 2>&1; then
            echo "Google Japanese Input is already installed."
            exit 0
        fi
        echo "Installing Google Japanese Input via brew..."
        brew install --cask google-japanese-ime
        echo "Google Japanese Input installed."
        ;;
    "ubuntu" )
        echo "Skipping Google Japanese Input on Ubuntu (use ibus-mozc instead)."
        ;;
    * )
        echo "Unsupported OS: $OSDIST"
        exit 1
        ;;
esac
