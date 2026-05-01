#!/bin/bash
# Install GitHub CLI (gh)
: "${DOTFILES_DIR:=$(cd "$(dirname "$0")/../.." && pwd)}"
source "$DOTFILES_DIR/bin/colors.sh"
source "$DOTFILES_DIR/bin/detectos.sh"

if command -v gh &>/dev/null; then
    printf "${C_GRAY}gh is already installed: $(gh --version | head -1)${C_RESET}\n"
    if gh auth status &>/dev/null 2>&1; then
        exit 0
    fi
else
    echo "Installing gh (GitHub CLI)..."
    case "$OSDIST" in
        ubuntu)
            sudo mkdir -p -m 755 /etc/apt/keyrings
            curl -fsSL https://cli.github.com/packages/githubcli-archive-keyring.gpg | sudo tee /etc/apt/keyrings/githubcli-archive-keyring.gpg >/dev/null
            sudo chmod go+r /etc/apt/keyrings/githubcli-archive-keyring.gpg
            echo "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/githubcli-archive-keyring.gpg] https://cli.github.com/packages stable main" | sudo tee /etc/apt/sources.list.d/github-cli.list >/dev/null
            sudo apt update
            sudo apt install -y gh
            ;;
        macos)
            brew install gh
            ;;
        *)
            echo "Unsupported OS: $OSDIST" >&2
            exit 1
            ;;
    esac
    printf "${C_GREEN}gh installed: $(gh --version | head -1)${C_RESET}\n"
fi

printf "${C_YELLOW}gh: not authenticated — running gh auth login...${C_RESET}\n"
gh auth login
