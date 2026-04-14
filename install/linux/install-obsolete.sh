#!/bin/bash
# Uninstall obsolete files

if [ -L $HOME/.bash_logout ]; then
    echo "symbolic link .bash_logout exists. Removing..."
    rm $HOME/.bash_logout
fi

if [ -L $HOME/.vim ]; then
    echo "symbolic link .vim exists. Removing..."
    rm $HOME/.vim
fi

if [ -L $HOME/.gitconfig ]; then
    echo "symbolic link .gitconfig exists. Removing...(Will use .config/git/config)"
    rm $HOME/.gitconfig
fi

if [ -f "$HOME/.gitconfig" ] && [ ! -L "$HOME/.gitconfig" ]; then
    echo "~/.gitconfig exists and overrides dotfiles git config (XDG)."
    read -rp "Delete it? [y/N] " response
    if [[ "$response" =~ ^[Yy]$ ]]; then
        rm "$HOME/.gitconfig"
        echo "Deleted ~/.gitconfig"
    fi
fi

GDIR=$HOME/.emacs.d/git
if [ -d $GDIR ]; then
    echo "emacs: found git clone dir for non-packaged. Removing..."
    rm -r $HOME/.emacs.d/git
fi

#if [ -d $HOME/.emacs.d/elpa/react-snippets-20181002.1046 ]; then
#    echo "react-snippet exists. Removing..."
#    rm -rf $HOME/.emacs.d/elpa/react-snippets-20181002.1046
#fi

if [ -d $HOME/.emacs.d/elpa/js-react-redux-yasnippets-20200316.1144 ]; then
    echo "js-react-redux-yasnippets exists. Removing..."
    rm -rf $HOME/.emacs.d/elpa/js-react-redux-yasnippets-20200316.1144
fi

if [ -d $HOME/.tfenv ]; then
    echo "tfenv exists. Removing... (Replaced by tenv)"
    rm -rf $HOME/.tfenv
    sudo rm -f /usr/local/bin/tfenv /usr/local/bin/terraform
fi

if [ -L ~/dotfiles/claude-code ]; then
    echo "Removing obsolete symlink: ~/dotfiles/claude-code (renamed to claude-global)"
    rm ~/dotfiles/claude-code
fi

# fnm (replaced by nvm on WSL2/macOS/Linux; Windows keeps fnm)
if [ -d "$HOME/.local/share/fnm" ]; then
    echo "Removing fnm directory: ~/.local/share/fnm (replaced by nvm)"
    rm -rf "$HOME/.local/share/fnm"
fi
# Also remove Homebrew fnm if present (not our original install method, but clean up anyway)
if type brew >/dev/null 2>&1 && brew list fnm &>/dev/null; then
    echo "Removing Homebrew fnm package (replaced by nvm)"
    brew uninstall fnm
fi
if command -v fnm &>/dev/null; then
    echo "[WARN] fnm binary still found on PATH after cleanup. Check shell configs."
fi

# --- BEGIN temporary: .git/workflow → ~/.claude/projects/workflow migration ---
_wf_new_dir="$HOME/.claude/projects/workflow"

_salvage_git_workflow() {
    local old="$1"
    [ -d "$old" ] || return
    echo "Found old workflow dir: $old"
    local n=0
    for f in "$old"/*.json; do
        [ -f "$f" ] || continue
        if find "$f" -mtime -7 2>/dev/null | grep -q .; then
            mkdir -p "$_wf_new_dir"
            cp "$f" "$_wf_new_dir/"
            echo "  Salvaged: $(basename "$f")"
            n=$((n+1))
        fi
    done
    rm -rf "$old"
    echo "  Removed: $old (salvaged $n file(s))"
}

while IFS= read -r -d '' d; do
    _salvage_git_workflow "$d"
done < <(find "$HOME" -maxdepth 4 -name "workflow" -path "*/.git/workflow" -type d -print0 2>/dev/null)
# --- END temporary: .git/workflow → ~/.claude/projects/workflow migration ---
