#!/bin/zsh
COMMON_PROFILE=$HOME/dotfiles/.profile_common
if [ -e $COMMON_PROFILE ]; then
    source $COMMON_PROFILE
fi

setopt auto_param_keys # complete pharenthesis
setopt auto_param_slash # complete '/'
setopt auto_pushd
setopt correct
#setopt correct_all # spellcheck to all command lines
#setopt globdots                # include dotfiles without '.'
setopt list_packed
setopt mark_dirs # add trailing '/' on completing directory
setopt no_beep
setopt nolistbeep   # does not beep on completion
setopt no_tify              # notify when bg job finished
#setopt print_exit_value
setopt pushd_ignore_dups    # remove duplicated directories on pushd

# History
setopt extended_history         # record time as well
setopt hist_ignore_dups         # do not record duplicated commands
setopt hist_ignore_space
setopt hist_reduce_blanks
setopt hist_no_functions    # do not record commands to define functions
setopt hist_no_store
setopt hist_expand          # auto expand histories on completion
setopt share_history        # share with other shells

# Disable predict, Enable zsh-autosuggestions instead
# ref. https://www.pandanoir.info/entry/2018/02/23/193721
#autoload predict-on
#predict-on
if [ -f ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh ]; then
    source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
fi
plugins=(
    zsh-autosuggestions
)
#ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=5'

zstyle ':completion:*' menu select
# case insensitive on completion
#zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
# directory color: cyan
zstyle ':completion:*' list-colors 'di=36;49'

# git-completion
# ref. https://blog.qnyp.com/2013/05/14/zsh-git-completion/
fpath=(~/completion $fpath)
autoload -Uz compinit
#compinit -u
compinit # runs securely

# git-prompt: unique for zsh
setopt PROMPT_SUBST ; PS1='%F{green}%n@%m%f:%F{cyan}%~%f %F{red}$(__git_ps1 "(%s)")%f\$ '
