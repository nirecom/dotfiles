if [ -z $TMUX ]; then
  if $(tmux has-session 2> /dev/null); then
    tmux -u -2 attach
  else
    tmux -u -2
  fi
fi
