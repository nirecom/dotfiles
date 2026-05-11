#!/bin/bash
# Install nvm (Node Version Manager)
# Windows uses fnm (nvm has no Windows support); WSL2/macOS/Linux use nvm.
#
# Policy: install the version manager only. Node.js itself is each repo's
# responsibility (via .node-version / .nvmrc). No default version is set —
# repos without a version file will fail explicitly rather than silently
# falling back to a stale LTS.

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
  echo "nvm installed. Restart your shell, then 'nvm install <version>' in each repo as needed."
fi
