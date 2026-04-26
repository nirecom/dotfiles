#!/bin/bash
# Install Claude Usage Widget
# Source: https://github.com/SlavomirDurej/claude-usage-widget
: "${DOTFILES_DIR:=$(cd "$(dirname "$0")/../.." && pwd)}"
source "$DOTFILES_DIR/bin/detectos.sh"

# Get latest release tag from GitHub API (strips leading 'v' so output is a bare version)
get_latest_version() {
    curl -fsSL "https://api.github.com/repos/SlavomirDurej/claude-usage-widget/releases/latest" | grep '"tag_name"' | sed 's/.*"tag_name": *"\(.*\)".*/\1/' | sed 's/^v//'
}

case "$OSDIST" in
    "macos" )
        APP_PATH="/Applications/Claude Usage Widget.app"
        LATEST_VERSION=$(get_latest_version)
        if [ -d "$APP_PATH" ]; then
            INSTALLED_VERSION=$(defaults read "$APP_PATH/Contents/Info.plist" CFBundleShortVersionString 2>/dev/null || echo "")
            if [ -n "$INSTALLED_VERSION" ] && [ "$INSTALLED_VERSION" = "$LATEST_VERSION" ]; then
                echo "Claude Usage Widget is up to date (v$INSTALLED_VERSION)."
                exit 0
            fi
            if [ -z "$INSTALLED_VERSION" ]; then
                echo "Claude Usage Widget installed but version unknown. Reinstalling v$LATEST_VERSION..."
            else
                echo "Updating Claude Usage Widget: v$INSTALLED_VERSION -> v$LATEST_VERSION"
            fi
            rm -rf "$APP_PATH"
        else
            echo "Installing Claude Usage Widget v$LATEST_VERSION..."
        fi
        VERSION="$LATEST_VERSION"
        if [ "$ISM1" = "1" ]; then
            DOWNLOAD_URL="https://github.com/SlavomirDurej/claude-usage-widget/releases/download/v${VERSION}/Claude-Usage-Widget-${VERSION}-macOS-arm64.dmg"
        else
            DOWNLOAD_URL="https://github.com/SlavomirDurej/claude-usage-widget/releases/download/v${VERSION}/Claude-Usage-Widget-${VERSION}-macOS-x64.dmg"
        fi
        TMPFILE=$(mktemp /tmp/claude-usage-widget-XXXXXX.dmg)
        trap 'rm -f "$TMPFILE"' EXIT
        curl -fSL -o "$TMPFILE" "$DOWNLOAD_URL"
        MOUNT_DIR=$(hdiutil attach "$TMPFILE" -nobrowse | grep "/Volumes" | sed 's/.*\/Volumes/\/Volumes/')
        APP_SRC=$(find "$MOUNT_DIR" -maxdepth 1 -name "*.app" -print -quit)
        if [ -z "$APP_SRC" ]; then
            hdiutil detach "$MOUNT_DIR" -quiet
            echo "Error: no .app found in DMG."
            exit 1
        fi
        cp -R "$APP_SRC" "$APP_PATH"
        hdiutil detach "$MOUNT_DIR" -quiet
        echo "Claude Usage Widget installed (v$VERSION)."

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
        VERSION_FILE="$APPIMAGE_PATH.version"
        LATEST_VERSION=$(get_latest_version)
        if [ -f "$APPIMAGE_PATH" ]; then
            INSTALLED_VERSION=""
            [ -f "$VERSION_FILE" ] && INSTALLED_VERSION=$(cat "$VERSION_FILE")
            if [ -n "$INSTALLED_VERSION" ] && [ "$INSTALLED_VERSION" = "$LATEST_VERSION" ]; then
                echo "Claude Usage Widget is up to date (v$INSTALLED_VERSION)."
                exit 0
            fi
            if [ -z "$INSTALLED_VERSION" ]; then
                echo "Claude Usage Widget installed but version unknown. Reinstalling v$LATEST_VERSION..."
            else
                echo "Updating Claude Usage Widget: v$INSTALLED_VERSION -> v$LATEST_VERSION"
            fi
        else
            echo "Installing Claude Usage Widget v$LATEST_VERSION..."
        fi
        VERSION="$LATEST_VERSION"
        ARCH=$(uname -m)
        if [ "$ARCH" = "aarch64" ]; then
            DOWNLOAD_URL="https://github.com/SlavomirDurej/claude-usage-widget/releases/download/v${VERSION}/Claude-Usage-Widget-${VERSION}-linux-arm64.AppImage"
        else
            DOWNLOAD_URL="https://github.com/SlavomirDurej/claude-usage-widget/releases/download/v${VERSION}/Claude-Usage-Widget-${VERSION}-linux-x86_64.AppImage"
        fi
        mkdir -p "$INSTALL_DIR"
        curl -fSL -o "$APPIMAGE_PATH" "$DOWNLOAD_URL"
        chmod +x "$APPIMAGE_PATH"
        echo "$LATEST_VERSION" > "$APPIMAGE_PATH.version"
        echo "Claude Usage Widget installed to $APPIMAGE_PATH (v$VERSION)"

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
