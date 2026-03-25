#!/bin/bash
# Install fnm (Fast Node Manager)
# Replaces: anyenv + nodenv

set -e

# Ensure unzip is available (required by fnm installer)
if ! command -v unzip &> /dev/null; then
  echo "Installing unzip..."
  sudo apt-get install -y unzip
fi

if command -v fnm &> /dev/null; then
  echo "fnm is already installed: $(fnm --version)"
else
  echo "Installing fnm..."
  # --skip-shell: dotfiles/.profile_common handles shell setup
  curl -fsSL https://fnm.vercel.app/install | bash -s -- --skip-shell
fi

# Load fnm for this session
export PATH="$HOME/.local/share/fnm:$PATH"
eval "$(fnm env)"

# Install LTS Node.js
echo "Installing Node.js LTS..."
fnm install --lts
fnm default lts-latest

echo "fnm setup complete: node $(node --version), npm $(npm --version)"
