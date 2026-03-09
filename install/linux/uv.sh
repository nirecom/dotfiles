#!/bin/bash
# Install uv (Python package manager)

set -e

if command -v uv &> /dev/null; then
  echo "uv is already installed: $(uv --version)"
else
  echo "Installing uv..."
  curl -LsSf https://astral.sh/uv/install.sh | sh
  echo "uv installed: $(uv --version)"
fi
