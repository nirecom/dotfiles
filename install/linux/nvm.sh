#!/bin/bash
# Install nvm (Node Version Manager)
# Windows uses fnm (nvm has no Windows support); WSL2/macOS/Linux use nvm.

set -e

: "${DOTFILES_DIR:=$(cd "$(dirname "$0")/../.." && pwd)}"
source "$DOTFILES_DIR/bin/colors.sh"

export NVM_DIR="${NVM_DIR:-$HOME/.nvm}"

if [ -s "$NVM_DIR/nvm.sh" ]; then
  printf "${C_GRAY}nvm is already installed: $(. "$NVM_DIR/nvm.sh" && nvm --version)${C_RESET}\n"
else
  echo "Installing nvm..."
  # --skip-shell: .profile_common already handles nvm init; prevent auto-modification of .zshrc/.bashrc
  curl -fsSL https://raw.githubusercontent.com/nvm-sh/nvm/v0.40.4/install.sh | bash -s -- --skip-shell
fi

# Load nvm for this session
. "$NVM_DIR/nvm.sh"

# Install LTS Node.js (skip if already present)
if nvm ls --no-colors lts/* &>/dev/null; then
  printf "${C_GRAY}Node.js LTS already installed: $(node --version)${C_RESET}\n"
else
  echo "Installing Node.js LTS..."
  nvm install --lts
fi
nvm alias default lts/*

echo "nvm setup complete: node $(node --version), npm $(npm --version)"
