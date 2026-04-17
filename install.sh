#!/bin/bash
# Unified installer for dotfiles (Linux/macOS)
# Usage:
#   ./install.sh            # Symlinks + Claude Code + nvm
#   ./install.sh --base     # Symlinks + base packages
#   ./install.sh --develop  # Symlinks + dev tools (awscli, vscode)
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
if type claude >/dev/null 2>&1; then
    echo ""
    echo "--- Initializing Claude Code session sync ---"
    ~/dotfiles/install/linux/session-sync-init.sh
fi

# Step 3: Install keychain (SSH agent manager)
echo ""
echo "--- Installing keychain ---"
~/dotfiles/install/linux/keychain.sh

# Step 4: Install nvm (Node.js) - required for Claude Code hooks
echo ""
echo "--- Installing nvm (Node.js) ---"
~/dotfiles/install/linux/nvm.sh

# Step 5: Clean up obsolete files
echo ""
echo "--- Cleaning up obsolete files ---"
~/dotfiles/install/linux/install-obsolete.sh

if [ "$1" = "--base" ] || [ "$1" = "--develop" ] || [ "$1" = "--full" ]; then
    # Step 6: Install base packages
    echo ""
    echo "--- Installing base packages ---"
    ~/dotfiles/install/linux/install-base.sh

    # Step 7: Install uv (Python package manager)
    echo ""
    echo "--- Installing uv ---"
    ~/dotfiles/install/linux/uv.sh

    # Step 8: Install Claude Usage Widget
    echo ""
    echo "--- Installing Claude Usage Widget ---"
    ~/dotfiles/install/linux/claude-usage-widget.sh
fi

if [ "$1" = "--develop" ] || [ "$1" = "--full" ]; then
    # Step 8: Install development tools
    echo ""
    echo "--- Installing development tools ---"
    ~/dotfiles/install/linux/install-develop.sh

    # Step 9: Install VS Code and extensions
    echo ""
    echo "--- Installing Visual Studio Code ---"
    ~/dotfiles/install/linux/vscode.sh
fi

# Run dotfiles-private installer if available
PRIVATE_INSTALLER="$(dirname "$0")/../dotfiles-private/install.sh"
if [ -x "$PRIVATE_INSTALLER" ]; then
    echo ""
    echo "--- Running dotfiles-private installer ---"
    "$PRIVATE_INSTALLER" "$@"
fi

echo ""
echo "=== Done ==="
exec $SHELL -l
