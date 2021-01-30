#!/bin/zsh
COMMON_PROFILE=$HOME/dotfiles/.bash_profile
if [ -e $COMMON_PROFILE ]; then
	echo "Loading..."
	source $COMMON_PROFILE
	echo "Loaded $COMMON_PROFILE ..."
fi

