#!/bin/zsh
COMMON_PROFILE=$HOME/dotfiles/.profile_common
if [ -e $COMMON_PROFILE ]; then
	source $COMMON_PROFILE
fi

# git-completion
# ref. https://blog.qnyp.com/2013/05/14/zsh-git-completion/
fpath=(~/.zsh/completion $fpath)
autoload -U compinit
compinit -u
