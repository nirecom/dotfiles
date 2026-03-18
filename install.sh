#!/bin/bash
# Unified installer for dotfiles (Linux/macOS)
# Usage:
#   ./install.sh          # Symlinks only
#   ./install.sh --full   # Symlinks + package installation

set -e

source ~/dotfiles/bin/detectos.sh

if [ "$OSDIST" = "qnap" ]; then
    echo "=== dotfiles installer (QNAP) ==="
    ~/dotfiles/install/qnap/dotfileslink.sh
    echo ""
    echo "=== Done ==="
    exit 0
fi

if [ "$OSDIST" = "mingw" ]; then
    echo "Windows detected. Use install/win/dotfileslink.ps1 instead."
    exit 1
fi

echo "=== dotfiles installer ==="
echo "OS: $OSDIST (WSL=$ISWSL, M1=$ISM1)"

# Step 1: Create symlinks
echo ""
echo "--- Creating symlinks ---"
~/dotfiles/install/linux/dotfileslink.sh

# Step 2: Install Claude Code
echo ""
echo "--- Installing Claude Code ---"
~/dotfiles/install/linux/claude-code.sh

# Step 3: Clean up obsolete files
echo ""
echo "--- Cleaning up obsolete files ---"
~/dotfiles/install/linux/home-obsolete.sh

if [ "$1" = "--full" ]; then
    # Step 4: Install packages
    echo ""
    echo "--- Installing packages ---"
    ~/dotfiles/install/linux/home-init.sh

    # Step 5: Install Rize (macOS only)
    echo ""
    echo "--- Installing Rize ---"
    ~/dotfiles/install/linux/rize.sh
fi

echo ""
echo "=== Done ==="
