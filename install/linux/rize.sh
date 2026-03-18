#!/bin/bash
# Install Rize time tracker
source ~/dotfiles/bin/detectos.sh

case "$OSDIST" in
    "macos" )
        if [ -d "/Applications/Rize.app" ]; then
            echo "Rize is already installed."
            exit 0
        fi
        echo "Installing Rize..."
        if [ "$ISM1" = "1" ]; then
            DOWNLOAD_URL="https://app.rize.io/downloads/macos?arch=arm64&beta=false"
        else
            DOWNLOAD_URL="https://app.rize.io/downloads/macos?beta=false"
        fi
        TMPFILE=$(mktemp /tmp/rize-XXXXXX.dmg)
        trap 'rm -f "$TMPFILE"' EXIT
        curl -fSL -o "$TMPFILE" "$DOWNLOAD_URL"
        MOUNT_DIR=$(hdiutil attach "$TMPFILE" -nobrowse | grep "/Volumes" | awk '{print $NF}')
        cp -R "$MOUNT_DIR/Rize.app" /Applications/
        hdiutil detach "$MOUNT_DIR" -quiet
        echo "Rize installed."
        ;;
    * )
        echo "Skipping Rize: not available on $OSDIST."
        ;;
esac
