#!/bin/bash
# Install Starship prompt
: "${DOTFILES_DIR:=$(cd "$(dirname "$0")/../.." && pwd)}"
source "$DOTFILES_DIR/bin/detectos.sh"

if type starship >/dev/null 2>&1; then
    echo "starship is already installed: $(starship --version)"
    exit 0
fi

echo "Installing starship..."
case "$OSDIST" in
    "ubuntu" )
        curl -sS https://starship.rs/install.sh | sh -s -- --yes
        ;;
    "macos" )
        brew install starship
        ;;
    * )
        echo "Unsupported OS: $OSDIST"
        exit 1
        ;;
esac

echo "starship installed: $(starship --version)"
