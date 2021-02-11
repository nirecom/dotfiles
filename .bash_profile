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
source ~/completion/git-completion.bash
export PS1='\[\033[32m\]\u@\h\[\033[00m\]:\[\033[1;34m\]\w\[\033[0;31m\]$(__git_ps1)\[\033[00m\]\$ '

# nvm on macos

# Added by iTerm
test -e "${HOME}/.iterm2_shell_integration.bash" && source "${HOME}/.iterm2_shell_integration.bash"

# Added by SDKMAN
#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"
