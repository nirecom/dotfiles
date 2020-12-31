export TERM=xterm-256color

alias ll='ls -al'
alias em='emacs'
alias d='docker'
alias dc='docker-compose'
alias tmux='~/dotfiles/tmux.sh'
# git settings
source ~/dotfiles/git-prompt.sh
source ~/dotfiles/git-completion.bash
GIT_PS1_SHOWDIRTYSTATE=true
export PS1='\[\033[32m\]\u@\h\[\033[00m\]:\[\033[1;34m\]\w\[\033[0;31m\]$(__git_ps1)\[\033[00m\]\$ '
if [ -f ~/dotfiles/git-values.sh ]; then
	source ~/dotfiles/git-values.sh
fi

# Setup ssh-agent
if type keychain >/dev/null 2>&1
then
    keychain --nogui --quiet ~/.ssh/id_rsa >/dev/null 2>&1 # giving up error happens
fi
if [ -f ~/.ssh-agent ]; then
	. ~/.ssh-agent >/dev/null
fi
if [ -z "$SSH_AGENT_PID" ] || ! kill -0 $SSH_AGENT_PID; then
	ssh-agent > ~/.ssh-agent
	. ~/.ssh-agent >/dev/null
fi
ssh-add -l >& /dev/null || ssh-add

if [ $HOSTNAME = "lab" ]; then
	~/dotfiles/tmux.sh
fi

# Added by iTerm
test -e "${HOME}/.iterm2_shell_integration.bash" && source "${HOME}/.iterm2_shell_integration.bash"

