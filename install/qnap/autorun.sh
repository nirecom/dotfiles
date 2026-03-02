#!/bin/sh
# QNAP autorun: restore Entware + dotfiles after reboot
#
# Usage: Register this script in QNAP Control Panel:
#   Hardware -> Boot-up User Defined Processes -> /share/CACHEDEV1_DATA/autorun.sh
# Or copy to /tmp/config/autorun.sh via QTS web console.

DOTFILES_DIR="$HOME/dotfiles"
DOTFILES_REPO="https://github.com/nirecom/dotfiles.git"

# Restore Entware if /opt/bin is missing
if [ ! -x /opt/bin/opkg ]; then
    if [ -f /share/CACHEDEV1_DATA/.qpkg/Entware/entware_startup.sh ]; then
        /bin/sh /share/CACHEDEV1_DATA/.qpkg/Entware/entware_startup.sh
    fi
fi

# Install essential packages
if [ -x /opt/bin/opkg ]; then
    /opt/bin/opkg update
    /opt/bin/opkg install git git-http bash vim-full curl
fi

# Clone or pull dotfiles
if [ -x /opt/bin/git ]; then
    if [ -d "$DOTFILES_DIR" ]; then
        /opt/bin/git -C "$DOTFILES_DIR" pull
    else
        /opt/bin/git clone "$DOTFILES_REPO" "$DOTFILES_DIR"
    fi
fi

# Run QNAP dotfiles link if available
if [ -f "$DOTFILES_DIR/install/qnap/dotfileslink.sh" ]; then
    /bin/sh "$DOTFILES_DIR/install/qnap/dotfileslink.sh"
fi
