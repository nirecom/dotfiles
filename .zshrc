#!/bin/zsh
COMMON_PROFILE=$HOME/dotfiles/.profile_common
if [ -e $COMMON_PROFILE ]; then
	source $COMMON_PROFILE
fi

# git-completion
# ref. https://blog.qnyp.com/2013/05/14/zsh-git-completion/
fpath=(~/completion $fpath)
autoload -Uz compinit
compinit -u

# git-prompt: unique for zsh
setopt PROMPT_SUBST ; PS1='%F{green}%n@%m%f:%F{cyan}%~%f %F{red}$(__git_ps1 "(%s)")%f\$ '
