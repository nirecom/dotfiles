#!/bin/bash
# Install uv (Python package manager)

set -e

if command -v uv &> /dev/null; then
  echo "uv is already installed: $(uv --version)"
else
  echo "Installing uv..."
  curl -LsSf https://astral.sh/uv/install.sh | INSTALLER_NO_MODIFY_PATH=1 sh
  # Refresh PATH so the newly installed uv is found
  export PATH="$HOME/.local/bin:$PATH"
  echo "uv installed: $(uv --version)"
fi
