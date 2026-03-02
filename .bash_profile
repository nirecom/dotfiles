#!/bin/bash
source $HOME/dotfiles/bin/detectos.sh

COMMON_PROFILE=$HOME/dotfiles/.profile_common
if [ -e $COMMON_PROFILE ]; then
    source $COMMON_PROFILE
fi

if type rbenv >/dev/null 2>&1; then
    eval "$(rbenv init -)"
fi

# git settings
# ref. https://qiita.com/varmil/items/9b0aeafa85975474e9b6
[ -f ~/completion/git-completion.bash ] && source ~/completion/git-completion.bash
if [ "$OSDIST" = "qnap" ]; then
    # QNAP Entware bash: \[...\] broken, use raw readline markers \001/\002
    export PS1=$'\001\033[32m\002\\u@\\h\001\033[00m\002:\001\033[1;34m\002\\w\001\033[0;31m\002$(__git_ps1)\001\033[00m\002\\$ '
else
    export PS1='\[\033[32m\]\u@\h\[\033[00m\]:\[\033[1;34m\]\w\[\033[0;31m\]$(__git_ps1)\[\033[00m\]\$ '
fi

# nvm on macos

# Added by iTerm
test -e "${HOME}/.iterm2_shell_integration.bash" && source "${HOME}/.iterm2_shell_integration.bash"

# Added by SDKMAN
#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"
