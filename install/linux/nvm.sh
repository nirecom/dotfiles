#!/bin/bash
# Install nvm (Node Version Manager)
# Windows uses fnm (nvm has no Windows support); WSL2/macOS/Linux use nvm.

set -e

export NVM_DIR="${NVM_DIR:-$HOME/.nvm}"

if [ -s "$NVM_DIR/nvm.sh" ]; then
  echo "nvm is already installed: $(. "$NVM_DIR/nvm.sh" && nvm --version)"
else
  echo "Installing nvm..."
  curl -fsSL https://raw.githubusercontent.com/nvm-sh/nvm/v0.40.4/install.sh | bash
fi

# Load nvm for this session
. "$NVM_DIR/nvm.sh"

# Install LTS Node.js (skip if already present)
if nvm ls --no-colors lts/* &>/dev/null; then
  echo "Node.js LTS already installed: $(node --version)"
else
  echo "Installing Node.js LTS..."
  nvm install --lts
fi
nvm alias default lts/*

echo "nvm setup complete: node $(node --version), npm $(npm --version)"
