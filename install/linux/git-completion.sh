#!/bin/bash
# ref. https://blog.qnyp.com/2013/05/14/zsh-git-completion/
if [ ! -s ~/completion/git-completion.bash ]; then
    echo "Installing git-completion ..."
    mkdir -p ~/completion
    cd ~/completion
    curl -O https://raw.githubusercontent.com/git/git/master/contrib/completion/git-completion.bash
    curl -O https://raw.githubusercontent.com/git/git/master/contrib/completion/git-prompt.sh
    curl -O https://raw.githubusercontent.com/git/git/master/contrib/completion/git-completion.zsh
    mv git-completion.zsh _git
else
    echo "git-completion is already installed."
fi
