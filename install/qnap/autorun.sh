#!/bin/sh
# QNAP autorun: restore Entware + dotfiles after reboot
#
# Usage: Register this script in QNAP Control Panel:
#   Hardware -> Boot-up User Defined Processes -> /share/CACHEDEV1_DATA/autorun.sh
# Or copy to /tmp/config/autorun.sh via QTS web console.

# QNAP boot location for the dotfiles repo. Override via env if relocating.
: "${DOTFILES_DIR:=$HOME/dotfiles}"
DOTFILES_REPO="https://github.com/nirecom/dotfiles.git"

# Restore Entware if /opt/bin is missing
if [ ! -x /opt/bin/opkg ]; then
    if [ -f /share/CACHEDEV1_DATA/.qpkg/Entware/Entware.sh ]; then
        /sbin/setcfg Entware Enable TRUE -f /etc/config/qpkg.conf
        /bin/sh /share/CACHEDEV1_DATA/.qpkg/Entware/Entware.sh start
    fi
fi

# Install essential packages (only if not already installed)
if [ -x /opt/bin/opkg ] && [ ! -x /opt/bin/git ]; then
    /opt/bin/opkg update
    /opt/bin/opkg install git git-http bash vim-full curl
fi

# Clone or pull dotfiles
if type git >/dev/null 2>&1; then
    if [ -d "$DOTFILES_DIR" ]; then
        git -C "$DOTFILES_DIR" pull
    else
        git clone "$DOTFILES_REPO" "$DOTFILES_DIR"
    fi
fi

# Run QNAP dotfiles link if available
if [ -f "$DOTFILES_DIR/install/qnap/dotfileslink.sh" ]; then
    /bin/sh "$DOTFILES_DIR/install/qnap/dotfileslink.sh"
fi
