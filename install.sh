#!/bin/bash
# Unified installer for dotfiles (Linux/macOS)
# Usage:
#   ./install.sh            # Symlinks + keychain + nvm
#   ./install.sh --base     # Symlinks + base packages
#   ./install.sh --develop  # Symlinks + dev tools (awscli, vscode)
#   ./install.sh --full     # Symlinks + base + dev tools

set -e

# Resolve DOTFILES_DIR from this script's location so the repo can live anywhere.
DOTFILES_DIR="$(cd "$(dirname "$0")" && pwd)"
export DOTFILES_DIR

source "$DOTFILES_DIR/bin/colors.sh"

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

# Step 6: Install GitHub CLI (gh) — required for hooks (private repo detection)
echo ""
printf "${C_BOLD}--- Installing gh (GitHub CLI) ---${C_RESET}\n"
"$DOTFILES_DIR/install/linux/gh.sh"

if [ "$1" = "--base" ] || [ "$1" = "--develop" ] || [ "$1" = "--full" ]; then
    # Step 7: Install base packages
    echo ""
    printf "${C_BOLD}--- Installing base packages ---${C_RESET}\n"
    "$DOTFILES_DIR/install/linux/install-base.sh"

    # Step 8: Install uv (Python package manager)
    echo ""
    printf "${C_BOLD}--- Installing uv ---${C_RESET}\n"
    "$DOTFILES_DIR/install/linux/uv.sh"

    # Step 9: Install Claude Usage Widget
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

echo ""
printf "${C_GREEN}=== Done ===${C_RESET}\n"
if [ -z "${IS_DOTFILES_SLAVE:-}" ]; then
    exec $SHELL -l
fi
