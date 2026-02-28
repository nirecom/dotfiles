#!/bin/bash
# Install fnm (Fast Node Manager)
# Replaces: anyenv + nodenv

set -e

if command -v fnm &> /dev/null; then
  echo "fnm is already installed: $(fnm --version)"
else
  echo "Installing fnm..."
  # Install scripts does rehash, but zsh does not support it. Avoiding unexpected abort with '|| true'
  curl -fsSL https://fnm.vercel.app/install | bash -s -- --skip-shell || true
  # --skip-shell: dotfiles/.profile_common handles shell setup
fi

# Load fnm for this session
export PATH="$HOME/.local/share/fnm:$PATH"
eval "$(fnm env)"

# Install LTS Node.js
echo "Installing Node.js LTS..."
fnm install --lts
fnm default lts-latest

echo "fnm setup complete: node $(node --version), npm $(npm --version)"
