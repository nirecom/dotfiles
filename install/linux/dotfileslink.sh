#!/bin/sh
#ln -sf ~/dotfiles/.bashrc ~/ # Ubuntu has .bashrc by default
ln -sf ~/dotfiles/.bash_profile ~/
#ln -sf ~/dotfiles/.bash_logout ~/
ln -sf ~/dotfiles/.zshrc ~/
ln -sf ~/dotfiles/.vimrc ~/
#ln -sf ~/dotfiles/.vim ~/
ln -sf ~/dotfiles/.editorconfig ~/
ln -sf ~/dotfiles/.tmux.conf ~/
ln -sf ~/dotfiles/.inputrc ~/

# Private context directory (gitignored)
mkdir -p ~/dotfiles/.context-private

# Git config
mkdir -p ~/.config
if [ ! -e ~/.config/git ]; then
    ln -sf ~/dotfiles/.config/git ~/.config/
fi

# Generate OS-specific git config.local
GIT_CONFIG_LOCAL=~/dotfiles/.config/git/config.local
if [ ! -e "$GIT_CONFIG_LOCAL" ]; then
    source ~/dotfiles/bin/detectos.sh
    case "$OSDIST" in
        macos)
            cat > "$GIT_CONFIG_LOCAL" << 'EOF'
[credential]
    helper = osxkeychain
EOF
            ;;
    esac
fi


# Starship config
if [ ! -e ~/.config/starship.toml ]; then
    ln -sf ~/dotfiles/.config/starship.toml ~/.config/
fi

# Claude Code global config
mkdir -p ~/.claude
if [ -d ~/.claude/.git ]; then
    echo "WARNING: ~/.claude is a git repo (dotclaude). Remove .git dir to enable symlinks."
else
    ln -sf ~/dotfiles/claude-code/CLAUDE.md ~/.claude/
    ln -sf ~/dotfiles/claude-code/settings.json ~/.claude/
    if [ -L ~/.claude/commands ]; then
        echo "Already linked: ~/.claude/commands"
    elif [ -d ~/.claude/commands ]; then
        echo "WARNING: ~/.claude/commands exists as directory (not symlink). Remove it and re-run."
    else
        ln -sf ~/dotfiles/claude-code/commands ~/.claude/commands
    fi
fi

# Emacs
mkdir -p ~/.emacs.d
ln -sf ~/dotfiles/.emacs.d/init.el ~/.emacs.d/
ln -sf ~/dotfiles/.emacs.d/inits ~/.emacs.d/
#ln -sf ~/dotfiles/.emacs.d/package-install.el ~/.emacs.d/
#ln -sf ~/dotfiles/.emacs.d/packages ~/.emacs.d/
mkdir -p ~/tmp
mkdir -p ~/.emacs_backup

#if [ -d ~/.atom/ ]; then
#    ln -sf ~/dotfiles/.atom/config.cson ~/.atom/
#    ln -sf ~/dotfiles/.atom/keymap.cson ~/.atom/
#fi

if [ -e ~/.ssh/ssh-add-all ]; then
    chmod +x ~/.ssh/ssh-add-all
fi
