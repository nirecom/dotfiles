export TERM=xterm-256color
# terraform does not read region from ~/.aws/config.
# ref. https://ja.ojit.com/so/terraform/3413058
export AWS_DEFAULT_REGION=$(aws configure get region --profile default)
export BASH_SILENCE_DEPRECATION_WARNING=1

alias em='emacs'
alias d='docker'
alias dx='docker exec -it'
alias dc='docker-compose'
alias dcu='docker-compose up -d'
alias dcub='docker-compose up -d --build'
alias dcd='docker-compose down'
alias g='git'
# gitpush alias: ref. https://qiita.com/ut0n/items/2074623c0b8c1c9ff8e6
gitpush() {
  git add -A
  git commit -m "$*"
  git push origin HEAD
}
alias gd='git diff'
alias gs='git status'
alias gl='git pull'
alias gp=gitpush
alias k='kubectl'
alias ll='ls -al'
alias psmem='ps aux k -pmem | head -n 10'
alias pscpu='ps aux k -pcpu | head -n 10'
alias tmux='~/dotfiles/tmux.sh'
alias viconfig='vim ~/.ssh/config; aws s3 cp ~/.ssh/config s3://nirecom-home/.ssh/'

# make each path unique
# ref. https://qiita.com/key-amb/items/ce39b0c85b30888e1e3b
uniqpath() {
    _path=""
    for _p in $(echo $PATH | tr ':' ' '); do
        case ":${_path}:" in
            *:"${_p}":* )
            ;;
            * )
                if [ "$_path" ]; then
                    _path="$_path:$_p"
                else
                    _path=$_p
                fi
                ;;
        esac
    done
    PATH=$_path
    unset _p
    unset _path
}
export PATH="$HOME/.nodebrew/current/bin:$HOME/.rbenv/bin:$PATH:/usr/local/go/bin:$HOME/development/flutter/bin"

uniqpath
if type rbenv >/dev/null 2>&1; then
    eval "$(rbenv init -)"
fi

# git settings
source ~/dotfiles/git-prompt.sh
source ~/dotfiles/git-completion.bash
GIT_PS1_SHOWDIRTYSTATE=true
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

# Added by iTerm
test -e "${HOME}/.iterm2_shell_integration.bash" && source "${HOME}/.iterm2_shell_integration.bash"

# Added by SDKMAN
#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"

if ! "$ISTERM"; then
    ~/dotfiles/tmux.sh
fi
