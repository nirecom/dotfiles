export TERM=xterm-256color

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
alias gp=gitpush
alias tmux='~/dotfiles/tmux.sh'
alias k='kubectl'
alias ll='ls -al'
# git settings
source ~/dotfiles/git-prompt.sh
source ~/dotfiles/git-completion.bash
GIT_PS1_SHOWDIRTYSTATE=true
export PS1='\[\033[32m\]\u@\h\[\033[00m\]:\[\033[1;34m\]\w\[\033[0;31m\]$(__git_ps1)\[\033[00m\]\$ '
if [ -f ~/dotfiles/git-values.sh ]; then
	source ~/dotfiles/git-values.sh
fi

# Setup ssh-agent
ISCLIENTOS=false
if [ "$(uname)" == 'Darwin' ]; then
    if type keychain >/dev/null 2>&1
    then
        keychain --nogui --quiet ~/.ssh/id_rsa >/dev/null 2>&1 # giving up error happens
    fi
    ISCLIENTOS=true
#elif [ "$(expr substr $(uname -s) 1 5)" == 'Linux' ]; then
#    ISCLIENTOS=false
elif [ "$(expr substr $(uname -s) 1 10)" == 'MINGW32_NT' ]; then
    ISCLIENTOS=true
elif [ "$(expr substr $(uname -s) 1 10)" == 'MINGW64_NT' ]; then
    ISCLIENTOS=true
else
    echo "You are on server. Will not launch ssh-agent."
fi

if "$ISCLIENTOS"; then
    echo "You are on client: $(uname -s)."
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
    
if [ $HOSTNAME = "lab" ]; then
	~/dotfiles/tmux.sh
fi

# Added by iTerm
test -e "${HOME}/.iterm2_shell_integration.bash" && source "${HOME}/.iterm2_shell_integration.bash"

