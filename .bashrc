alias ll='ls -al'
alias tmux='tmux -u'
# git settings
source /usr/local/git/contrib/completion/git-prompt.sh
source /usr/local/git/contrib/completion/git-completion.bash
GIT_PS1_SHOWDIRTYSTATE=true
export PS1='\[\033[32m\]\u@\h\[\033[00m\]:\[\033[1;34m\]\w\[\033[0;31m\]$(__git_ps1)\[\033[00m\]\$ '

# Setup ssh-agent
if [ -f ~/.ssh-agent ]; then
    . ~/.ssh-agent >/dev/null
fi
if [ -z "$SSH_AGENT_PID" ] || ! kill -0 $SSH_AGENT_PID; then
    ssh-agent > ~/.ssh-agent
    . ~/.ssh-agent >/dev/null
fi
ssh-add -l >& /dev/null || ssh-add

keychain --nogui --quiet ~/.ssh/id_rsa

[[ $TERM != "screen" ]] && exec tmux -u
