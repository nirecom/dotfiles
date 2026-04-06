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

# Install Python via uv (if not already installed)
if command -v uv &> /dev/null; then
    if uv python list --only-installed 2>&1 | grep -q cpython; then
        echo "Python is already installed via uv."
    else
        echo "Installing Python via uv..."
        uv python install
        if [ $? -eq 0 ]; then
            echo "Python installed via uv."
        else
            echo "WARNING: Python installation failed. Re-run to retry." >&2
        fi
    fi
fi
