#!/bin/bash
# Unified installer for dotfiles (Linux/macOS)
# Usage:
#   ./install.sh            # Symlinks + keychain + nvm
#   ./install.sh --base     # Symlinks + base packages
#   ./install.sh --develop  # Symlinks + dev tools (awscli, vscode)
#   ./install.sh --full     # Symlinks + base + dev tools

set -e

# Colors (only when stdout is a terminal)
if [ -t 1 ]; then
    C_CYAN='\033[0;36m'; C_GREEN='\033[0;32m'; C_YELLOW='\033[0;33m'; C_BOLD='\033[1m'; C_RESET='\033[0m'
else
    C_CYAN=''; C_GREEN=''; C_YELLOW=''; C_BOLD=''; C_RESET=''
fi

# Resolve DOTFILES_DIR from this script's location so the repo can live anywhere.
DOTFILES_DIR="$(cd "$(dirname "$0")" && pwd)"
export DOTFILES_DIR

source "$DOTFILES_DIR/bin/detectos.sh"

if [ "$OSDIST" = "qnap" ]; then
    printf "${C_CYAN}=== dotfiles installer (QNAP) ===${C_RESET}\n"
    "$DOTFILES_DIR/install/qnap/dotfileslink.sh"
    echo ""
    printf "${C_GREEN}=== Done ===${C_RESET}\n"
    exit 0
fi

if [ "$OSDIST" = "mingw" ]; then
    printf "${C_YELLOW}Windows detected. Use install/win/dotfileslink.ps1 instead.${C_RESET}\n"
    exit 1
fi

printf "${C_CYAN}=== dotfiles installer ===${C_RESET}\n"
echo "OS: $OSDIST (WSL=$ISWSL, M1=$ISM1)"

# Step 1: Create symlinks
echo ""
printf "${C_BOLD}--- Creating symlinks ---${C_RESET}\n"
"$DOTFILES_DIR/install/linux/dotfileslink.sh"

# Step 2: Install keychain (SSH agent manager)
echo ""
printf "${C_BOLD}--- Installing keychain ---${C_RESET}\n"
"$DOTFILES_DIR/install/linux/keychain.sh"

# Step 4: Install nvm (Node.js) - required for Claude Code hooks
echo ""
printf "${C_BOLD}--- Installing nvm (Node.js) ---${C_RESET}\n"
"$DOTFILES_DIR/install/linux/nvm.sh"

# Step 5: Clean up obsolete files
echo ""
printf "${C_BOLD}--- Cleaning up obsolete files ---${C_RESET}\n"
"$DOTFILES_DIR/install/linux/install-obsolete.sh"

if [ "$1" = "--base" ] || [ "$1" = "--develop" ] || [ "$1" = "--full" ]; then
    # Step 6: Install base packages
    echo ""
    printf "${C_BOLD}--- Installing base packages ---${C_RESET}\n"
    "$DOTFILES_DIR/install/linux/install-base.sh"

    # Step 7: Install uv (Python package manager)
    echo ""
    printf "${C_BOLD}--- Installing uv ---${C_RESET}\n"
    "$DOTFILES_DIR/install/linux/uv.sh"

    # Step 8: Install Claude Usage Widget
    echo ""
    printf "${C_BOLD}--- Installing Claude Usage Widget ---${C_RESET}\n"
    "$DOTFILES_DIR/install/linux/claude-usage-widget.sh"
fi

if [ "$1" = "--develop" ] || [ "$1" = "--full" ]; then
    # Step 8: Install development tools
    echo ""
    printf "${C_BOLD}--- Installing development tools ---${C_RESET}\n"
    "$DOTFILES_DIR/install/linux/install-develop.sh"

    # Step 9: Install VS Code and extensions
    echo ""
    printf "${C_BOLD}--- Installing Visual Studio Code ---${C_RESET}\n"
    "$DOTFILES_DIR/install/linux/vscode.sh"
fi

# Run dotfiles-private installer if available (sibling repo)
PRIVATE_INSTALLER="$(dirname "$DOTFILES_DIR")/dotfiles-private/install.sh"
if [ -x "$PRIVATE_INSTALLER" ]; then
    echo ""
    printf "${C_BOLD}--- Running dotfiles-private installer ---${C_RESET}\n"
    "$PRIVATE_INSTALLER" "$@"
fi

# Run agents installer if available (sibling repo)
AGENTS_INSTALLER="$(dirname "$DOTFILES_DIR")/agents/install.sh"
if [ -x "$AGENTS_INSTALLER" ]; then
    echo ""
    printf "${C_BOLD}--- Running agents installer ---${C_RESET}\n"
    "$AGENTS_INSTALLER"
fi

echo ""
printf "${C_GREEN}=== Done ===${C_RESET}\n"
exec $SHELL -l
