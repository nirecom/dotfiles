alias ll='ls -al'
# git settings
source /usr/local/git/contrib/completion/git-prompt.sh
source /usr/local/git/contrib/completion/git-completion.bash
GIT_PS1_SHOWDIRTYSTATE=true
export PS1='\[\033[32m\]\u@\h\[\033[00m\]:\[\033[1;34m\]\w\[\033[0;31m\]$(__git_ps1)\[\033[00m\]\$ '
<<<<<<< HEAD

# avoid multiple ssh-agent processes
SSH_ENV=$HOME/.ssh/environment

function start_agent {
  ssh-agent > $SSH_ENV
  chmod 600 $SSH_ENV
  . $SSH_ENV > /dev/null
  # ssh-add
}

if [ -f $SSH_ENV ]; then
  . $SSH_ENV > /dev/null
  if ps ${SSH_AGENT_PID:-999999} | grep ssh-agent$ > /dev/null &&
     test -S $SSH_AUTH_SOCK; then
    # agent already running
    :
  else
    start_agent;
  fi
else
  start_agent
fi

[[ $TERM != "screen" ]] && exec tmux -f ~/.tmux.conf
=======
[[ $TERM != "screen" ]] && exec tmux
>>>>>>> parent of af5287f... .gitconfig.* are now obsoleted
