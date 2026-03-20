#!/bin/bash
# Unified installer for dotfiles (Linux/macOS)
# Usage:
#   ./install.sh            # Symlinks only
#   ./install.sh --base     # Symlinks + base packages
#   ./install.sh --develop  # Symlinks + dev tools
#   ./install.sh --full     # Symlinks + base + dev tools

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
~/dotfiles/install/linux/install-obsolete.sh

if [ "$1" = "--base" ] || [ "$1" = "--full" ]; then
    # Step 4: Install base packages
    echo ""
    echo "--- Installing base packages ---"
    ~/dotfiles/install/linux/install-base.sh

    # Step 5: Install Rize (macOS only)
    echo ""
    echo "--- Installing Rize ---"
    ~/dotfiles/install/linux/rize.sh

    # Step 6: Install Claude Usage Widget
    echo ""
    echo "--- Installing Claude Usage Widget ---"
    ~/dotfiles/install/linux/claude-usage-widget.sh
fi

if [ "$1" = "--develop" ] || [ "$1" = "--full" ]; then
    # Step 7: Install development tools
    echo ""
    echo "--- Installing development tools ---"
    ~/dotfiles/install/linux/install-develop.sh
fi

echo ""
echo "=== Done ==="
