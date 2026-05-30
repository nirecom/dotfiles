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

if [ -L "$DOTFILES_DIR/claude-code" ]; then
    echo "Removing obsolete symlink: $DOTFILES_DIR/claude-code (renamed to claude-global)"
    rm "$DOTFILES_DIR/claude-code"
fi

# Remove obsolete ~/.claude/* symlinks that pointed to dotfiles/claude-global
# (claude-global moved to the agents repo in agents-split refactor)
for _name in CLAUDE.md settings.json skills rules agents; do
    _link="$HOME/.claude/$_name"
    if [ -L "$_link" ]; then
        _target=$(readlink "$_link")
        case "$_target" in
            */dotfiles/claude-global/*)
                echo "Removing obsolete symlink: $_link (was: $_target)"
                rm "$_link"
                ;;
        esac
    fi
done

# Remove leftover dotfiles/claude-global/ directory only when HEAD confirms
# claude-global/ is no longer tracked (post-agents-split). Untracked leftover
# files (e.g. from prior `git reset --hard` over the split commit) survive
# normal git operations and need explicit cleanup.
_cg_dir="$DOTFILES_DIR/claude-global"
if [ -d "$_cg_dir" ] && [ -d "$DOTFILES_DIR/.git" ]; then
    if ! git -C "$DOTFILES_DIR" ls-tree -r HEAD claude-global 2>/dev/null | grep -q .; then
        echo "Removing obsolete dotfiles/claude-global/ (post-agents-split leftover)"
        rm -rf "$_cg_dir"
    fi
fi
unset _cg_dir

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

# Remove obsolete core.hooksPath from config.local (now managed by agents installer via ~/.gitconfig)
_git_config_local="$DOTFILES_DIR/.config/git/config.local"
if [ -f "$_git_config_local" ]; then
    _hp=$(git config --file "$_git_config_local" core.hooksPath 2>/dev/null || true)
    if [ -n "$_hp" ]; then
        echo ""
        echo "core.hooksPath is set in config.local: $_hp"
        echo "This is now written to ~/.gitconfig by the agents installer and is no longer needed here."
        printf "Remove core.hooksPath from config.local? [y/N] "
        read -r _ans
        case "$_ans" in
            [yY]*)
                git config --file "$_git_config_local" --unset core.hooksPath
                echo "Removed core.hooksPath from config.local."
                ;;
            *)
                echo "Skipped."
                ;;
        esac
        unset _ans
    fi
    unset _hp
fi
unset _git_config_local
