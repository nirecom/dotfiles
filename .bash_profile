#!/bin/bash
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
if [ -f ~/dotfiles/git-values.sh ]; then
	source ~/dotfiles/git-values.sh
fi

# Setup ssh-agent
ISTERM=false
if [ "$(uname)" == 'Darwin' ]; then
    if type keychain >/dev/null 2>&1
    then
        keychain --nogui --quiet ~/.ssh/id_rsa >/dev/null 2>&1 # giving up error happens
    fi
    ISTERM=true
#elif [ "$(expr substr $(uname -s) 1 5)" == 'Linux' ]; then
#    ISTERM=false
elif [ "$(expr substr $(uname -s) 1 10)" == 'MINGW32_NT' ]; then
    ISTERM=true
elif [ "$(expr substr $(uname -s) 1 10)" == 'MINGW64_NT' ]; then
    ISTERM=true
elif [ -v WSLENV ]; then # WSL
    ISTERM=true
fi

if "$ISTERM"; then
    if [ -f ~/.ssh-agent ]; then
	. ~/.ssh-agent >/dev/null
    fi
    if [ -z "$SSH_AGENT_PID" ] || ! kill -0 $SSH_AGENT_PID >/dev/null 2>&1; then
	echo "Launching ssh-agent ..."
	ssh-agent > ~/.ssh-agent
	. ~/.ssh-agent >/dev/null
    fi
    ssh-add -l >& /dev/null || ~/.ssh/ssh-add-all
fi
    
if type git > /dev/null 2>&1; then # if git is installed
    if type pgrep >/dev/null 2>&1; then
	PID=`pgrep -fo bash`
    fi
    if [ -n "$PID" ] && [ $$ != "$PID" ]; then # if first bash process
          echo "git pull ~/dotfiles ..."
          git -C ~/dotfiles pull
    fi
fi

# Source code highlighting
export LESSOPEN="| /usr/local/bin/src-hilite-lesspipe.sh %s"
export LESS='-R'

# nvm on macos
if [ "$(uname)" == 'Darwin' ]; then
    export NVM_DIR="$HOME/.nvm"
    [ -s "/usr/local/opt/nvm/nvm.sh" ] && . "/usr/local/opt/nvm/nvm.sh"  # This loads nvm
    [ -s "/usr/local/opt/nvm/etc/bash_completion.d/nvm" ] && . "/usr/local/opt/nvm/etc/bash_completion.d/nvm"  # This loads nvm bash_completion
fi

# Added by iTerm
test -e "${HOME}/.iterm2_shell_integration.bash" && source "${HOME}/.iterm2_shell_integration.bash"

# Added by SDKMAN
#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"

if ! "$ISTERM"; then
    ~/dotfiles/tmux.sh
fi
