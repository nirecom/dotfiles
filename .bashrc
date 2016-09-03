alias ll='ls -al'
alias tmux='~/dotfiles/tmux.sh'
# git settings
source ~/dotfiles/git-prompt.sh
source ~/dotfiles/git-completion.bash
GIT_PS1_SHOWDIRTYSTATE=true
export PS1='\[\033[32m\]\u@\h\[\033[00m\]:\[\033[1;34m\]\w\[\033[0;31m\]$(__git_ps1)\[\033[00m\]\$ '

# Setup ssh-agent
keychain --nogui --quiet ~/.ssh/id_rsa >/dev/null 2>&1 # giving up error happens
if [ -f ~/.ssh-agent ]; then
	. ~/.ssh-agent >/dev/null
fi
if [ -z "$SSH_AGENT_PID" ] || ! kill -0 $SSH_AGENT_PID; then
	ssh-agent > ~/.ssh-agent
	. ~/.ssh-agent >/dev/null
fi
ssh-add -l >& /dev/null || ssh-add

if [ $HOSTNAME = "lab.nire.com" ]; then
	~/dotfiles/tmux.sh
fi

#[[ $TERM != "screen" ]] && exec tmux -u
