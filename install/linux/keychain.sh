#!/bin/bash
# Install keychain (SSH/GPG agent manager)
source ./bin/detectos.sh

if command -v keychain &> /dev/null; then
    echo "keychain is already installed: $(keychain --version 2>&1 | head -1)"
    exit 0
fi

echo "Installing keychain..."
case "$OSDIST" in
    "ubuntu" )
        sudo apt update
        sudo apt install -y keychain
        ;;
    "macos" )
        brew update
        brew install keychain
        ;;
    * )
        echo "Unsupported OS: $OSDIST"
        exit 1
        ;;
esac

echo "keychain installed: $(keychain --version 2>&1 | head -1)"
