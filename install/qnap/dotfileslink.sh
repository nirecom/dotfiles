#!/bin/sh
# QNAP-minimal symlinks (no zsh, no tmux, no emacs)

# Auto-switch to bash on login
ln -sf ~/dotfiles/.profile_qnap ~/.profile

ln -sf ~/dotfiles/.bash_profile ~/
ln -sf ~/dotfiles/.vimrc ~/
ln -sf ~/dotfiles/.inputrc ~/

# Vim plugins (pathogen + bundles)
mkdir -p ~/.vim/autoload ~/.vim/bundle
[ ! -f ~/.vim/autoload/pathogen.vim ] && curl -fsSLo ~/.vim/autoload/pathogen.vim https://tpo.pe/pathogen.vim
cd ~/.vim/bundle
[ ! -d ./vim-colors-solarized ] && git clone https://github.com/altercation/vim-colors-solarized.git
[ ! -d ./vim-sensible ] && git clone https://github.com/tpope/vim-sensible.git
[ ! -d ./editorconfig-vim ] && git clone https://github.com/editorconfig/editorconfig-vim.git
[ ! -d ./vim-json ] && git clone https://github.com/elzr/vim-json.git
ln -sf ~/dotfiles/filetype.vim ~/.vim/
cd ~
ln -sf ~/dotfiles/.editorconfig ~/

# Git config
mkdir -p ~/.config
if [ ! -e ~/.config/git ]; then
    ln -sf ~/dotfiles/.config/git ~/.config/
fi

# Entware packages required for proper terminal handling
if command -v opkg >/dev/null 2>&1; then
    opkg list-installed 2>/dev/null | grep -q '^terminfo ' || opkg install terminfo
fi

# Git completion scripts (for prompt and tab-completion)
mkdir -p ~/completion
if [ ! -f ~/completion/git-prompt.sh ]; then
    curl -fsSL -o ~/completion/git-prompt.sh https://raw.githubusercontent.com/git/git/master/contrib/completion/git-prompt.sh
fi
if [ ! -f ~/completion/git-completion.bash ]; then
    curl -fsSL -o ~/completion/git-completion.bash https://raw.githubusercontent.com/git/git/master/contrib/completion/git-completion.bash
fi

# Install autorun.sh to flash config partition (requires root)
if command -v /sbin/hal_app >/dev/null 2>&1; then
    BOOT_DEV="$(/sbin/hal_app --get_boot_pd port_id=0)6"
    if [ -n "$BOOT_DEV" ]; then
        echo "Installing autorun.sh to config partition..."
        mkdir -p /tmp/config
        if [ "$(id -u)" = "0" ]; then
            if mount "$BOOT_DEV" /tmp/config; then
                cp ~/dotfiles/install/qnap/autorun.sh /tmp/config/autorun.sh
                chmod +x /tmp/config/autorun.sh
                umount /tmp/config
                echo "autorun.sh installed. Enable 'Run user defined startup processes' in QNAP Control Panel -> Hardware -> General (one-time)."
            else
                echo "WARNING: Failed to mount config partition. autorun.sh was NOT installed."
            fi
        elif command -v sudo >/dev/null 2>&1; then
            if sudo mount "$BOOT_DEV" /tmp/config; then
                sudo cp ~/dotfiles/install/qnap/autorun.sh /tmp/config/autorun.sh
                sudo chmod +x /tmp/config/autorun.sh
                sudo umount /tmp/config
                echo "autorun.sh installed. Enable 'Run user defined startup processes' in QNAP Control Panel -> Hardware -> General (one-time)."
            else
                echo "WARNING: Failed to mount config partition. autorun.sh was NOT installed."
            fi
        else
            echo "WARNING: Root privileges required to install autorun.sh. Run as root or install sudo."
        fi
    fi
fi
