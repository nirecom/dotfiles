#!/bin/zsh
COMMON_PROFILE=$HOME/dotfiles/.profile_common
if [ -e $COMMON_PROFILE ]; then
    source $COMMON_PROFILE
fi

setopt auto_param_keys # complete pharenthesis
setopt auto_param_slash # complete '/'
setopt auto_cd # can suppress cd command
cdpath=(.. ~/dotfiles(N-) ~  ~/.emacs.d(N-))
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
setopt hist_ignore_dups         # remove dups in series
setopt hist_ignore_all_dups     # remove older dups
setopt hist_find_no_dups        # skip dups while finding
setopt hist_ignore_space
setopt hist_reduce_blanks
setopt hist_no_functions    # do not store commands to define functions
setopt hist_no_store        # do not store history command
setopt hist_expand          # auto expand histories on completion
setopt share_history        # share with other shells

# add only histories that matches conditions
# ref https://mollifier.hatenablog.com/entry/20090728/p1
zshaddhistory() {
    local line=${1%%$'\n'}
    local cmd=${line%% *}

    [[ ${#line} -ge 3       # store longer lines only
        && ${cmd} != (l|l[sal]) # do not store following commands
        && ${cmd} != (c|cd)
        && ${cmd} != (h|history)
        && ${cmd} != (m|man)
    ]]
}

# Disable predict, Enable zsh-autosuggestions instead with zinit
# ref. https://www.pandanoir.info/entry/2018/02/23/193721
#autoload predict-on
#predict-on

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

# Zinit
# ref. https://github.com/zdharma/zinit#installation
#
### Added by Zinit's installer
if [[ ! -f $HOME/.zinit/bin/zinit.zsh ]]; then
    print -P "%F{33}▓▒░ %F{220}Installing %F{33}DHARMA%F{220} Initiative Plugin Manager (%F{33}zdharma/zinit%F{220})…%f"
    command mkdir -p "$HOME/.zinit" && command chmod g-rwX "$HOME/.zinit"
    command git clone https://github.com/zdharma/zinit "$HOME/.zinit/bin" && \
        print -P "%F{33}▓▒░ %F{34}Installation successful.%f%b" || \
        print -P "%F{160}▓▒░ The clone has failed.%f%b"
fi

source "$HOME/.zinit/bin/zinit.zsh"
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit
### End of Zinit's installer chunk

# Plugins
zinit light zsh-users/zsh-autosuggestions
zinit light zdharma/fast-syntax-highlighting

# kubectl completion
if type kubectl >/dev/null 2>&1; then
    source <(kubectl completion zsh)
fi
#complete -F __start_kubectl k

# Added by SDKMAN
#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="/Users/nire/.sdkman"
[[ -s "/Users/nire/.sdkman/bin/sdkman-init.sh" ]] && source "/Users/nire/.sdkman/bin/sdkman-init.sh"
