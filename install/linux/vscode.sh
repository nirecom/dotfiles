#!/bin/bash
# Install Visual Studio Code and extensions
# Usage: Called by install.sh --develop

: "${DOTFILES_DIR:=$(cd "$(dirname "$0")/../.." && pwd)}"
source "$DOTFILES_DIR/bin/detectos.sh"

# Install VS Code
if ! type code >/dev/null 2>&1; then
    echo "Installing Visual Studio Code..."
    case "$OSDIST" in
        "ubuntu" )
            if [ "$ISWSL" = "1" ]; then
                echo "WSL detected. VS Code should be installed on the Windows host."
                echo "Install from: https://code.visualstudio.com/"
                return 0 2>/dev/null || exit 0
            fi
            # Microsoft GPG key and repo
            wget -qO- https://packages.microsoft.com/keys/microsoft.asc | gpg --dearmor > /tmp/packages.microsoft.gpg
            sudo install -D -o root -g root -m 644 /tmp/packages.microsoft.gpg /etc/apt/keyrings/packages.microsoft.gpg
            echo "deb [arch=amd64,arm64,armhf signed-by=/etc/apt/keyrings/packages.microsoft.gpg] https://packages.microsoft.com/repos/code stable main" | sudo tee /etc/apt/sources.list.d/vscode.list > /dev/null
            rm -f /tmp/packages.microsoft.gpg
            sudo apt-get update
            sudo apt-get install -y code
            ;;
        "macos" )
            if type brew >/dev/null 2>&1; then
                brew install --cask visual-studio-code
            else
                echo "Homebrew not found. Install VS Code manually: https://code.visualstudio.com/"
                return 0 2>/dev/null || exit 0
            fi
            ;;
        * )
            echo "Unsupported OS for VS Code installation: $OSDIST"
            return 0 2>/dev/null || exit 0
            ;;
    esac

    if type code >/dev/null 2>&1; then
        echo "Visual Studio Code installed."
    else
        echo "VS Code installed but 'code' command not found. Restart your terminal."
        return 0 2>/dev/null || exit 0
    fi
else
    echo "Visual Studio Code is already installed."
fi

# Install extensions
EXT_FILE="$DOTFILES_DIR"/config/vscode-extensions.txt
if [ ! -f "$EXT_FILE" ]; then
    echo "Extension list not found: $EXT_FILE"
    return 0 2>/dev/null || exit 0
fi

INSTALLED_EXTS=$(code --list-extensions 2>/dev/null)

while IFS= read -r ext || [ -n "$ext" ]; do
    ext=$(echo "$ext" | xargs)
    [ -z "$ext" ] && continue
    [[ "$ext" == \#* ]] && continue
    if echo "$INSTALLED_EXTS" | grep -qi "^${ext}$"; then
        echo "  Extension already installed: $ext"
    else
        echo "  Installing extension: $ext"
        code --install-extension "$ext" --force 2>/dev/null
    fi
done < "$EXT_FILE"
echo "VS Code extensions synced."
