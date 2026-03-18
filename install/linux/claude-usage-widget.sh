#!/bin/bash
# Install Claude Usage Widget
# Source: https://github.com/SlavomirDurej/claude-usage-widget
source ~/dotfiles/bin/detectos.sh

# Get latest release tag from GitHub API
get_latest_version() {
    curl -fsSL "https://api.github.com/repos/SlavomirDurej/claude-usage-widget/releases/latest" | grep '"tag_name"' | sed 's/.*"v\(.*\)".*/\1/'
}

case "$OSDIST" in
    "macos" )
        APP_PATH="/Applications/Claude Usage Widget.app"
        if [ -d "$APP_PATH" ]; then
            echo "Claude Usage Widget is already installed."
            exit 0
        fi
        echo "Installing Claude Usage Widget..."
        VERSION=$(get_latest_version)
        if [ "$ISM1" = "1" ]; then
            DOWNLOAD_URL="https://github.com/SlavomirDurej/claude-usage-widget/releases/download/v${VERSION}/Claude-Usage-Widget-${VERSION}-macOS-arm64.dmg"
        else
            DOWNLOAD_URL="https://github.com/SlavomirDurej/claude-usage-widget/releases/download/v${VERSION}/Claude-Usage-Widget-${VERSION}-macOS-x64.dmg"
        fi
        TMPFILE=$(mktemp /tmp/claude-usage-widget-XXXXXX.dmg)
        trap 'rm -f "$TMPFILE"' EXIT
        curl -fSL -o "$TMPFILE" "$DOWNLOAD_URL"
        MOUNT_DIR=$(hdiutil attach "$TMPFILE" -nobrowse | grep "/Volumes" | awk '{print $NF}')
        cp -R "$MOUNT_DIR/Claude Usage Widget.app" /Applications/
        hdiutil detach "$MOUNT_DIR" -quiet
        echo "Claude Usage Widget installed."

        # Configure login item for autostart
        if ! osascript -e 'tell application "System Events" to get the name of every login item' 2>/dev/null | grep -q "Claude Usage Widget"; then
            osascript -e 'tell application "System Events" to make login item at end with properties {path:"/Applications/Claude Usage Widget.app", hidden:false}'
            echo "Added to login items (autostart)."
        else
            echo "Already in login items."
        fi
        ;;
    "ubuntu" )
        if [ "$ISWSL" = "1" ]; then
            echo "Skipping Claude Usage Widget: not supported on WSL."
            exit 0
        fi
        INSTALL_DIR="$HOME/.local/bin"
        APPIMAGE_PATH="$INSTALL_DIR/claude-usage-widget.AppImage"
        if [ -f "$APPIMAGE_PATH" ]; then
            echo "Claude Usage Widget is already installed."
            exit 0
        fi
        echo "Installing Claude Usage Widget..."
        VERSION=$(get_latest_version)
        ARCH=$(uname -m)
        if [ "$ARCH" = "aarch64" ]; then
            DOWNLOAD_URL="https://github.com/SlavomirDurej/claude-usage-widget/releases/download/v${VERSION}/Claude-Usage-Widget-${VERSION}-linux-arm64.AppImage"
        else
            DOWNLOAD_URL="https://github.com/SlavomirDurej/claude-usage-widget/releases/download/v${VERSION}/Claude-Usage-Widget-${VERSION}-linux-x86_64.AppImage"
        fi
        mkdir -p "$INSTALL_DIR"
        curl -fSL -o "$APPIMAGE_PATH" "$DOWNLOAD_URL"
        chmod +x "$APPIMAGE_PATH"
        echo "Claude Usage Widget installed to $APPIMAGE_PATH"

        # Create autostart desktop entry
        AUTOSTART_DIR="$HOME/.config/autostart"
        DESKTOP_FILE="$AUTOSTART_DIR/claude-usage-widget.desktop"
        if [ ! -f "$DESKTOP_FILE" ]; then
            mkdir -p "$AUTOSTART_DIR"
            cat > "$DESKTOP_FILE" <<DESKTOP_EOF
[Desktop Entry]
Type=Application
Name=Claude Usage Widget
Exec=$APPIMAGE_PATH
X-GNOME-Autostart-enabled=true
DESKTOP_EOF
            echo "Created autostart entry."
        else
            echo "Autostart entry already exists."
        fi
        ;;
    * )
        echo "Skipping Claude Usage Widget: not available on $OSDIST."
        ;;
esac
