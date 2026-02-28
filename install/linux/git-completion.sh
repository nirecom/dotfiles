#!/bin/bash
# ref. https://blog.qnyp.com/2013/05/14/zsh-git-completion/
echo "Installing git-completion ..."
if [ ! -s ~/completion/git-comletion.bash ]; then
    mkdir -p ~/completion
    cd ~/completion
    curl -O https://raw.githubusercontent.com/git/git/master/contrib/completion/git-completion.bash
    curl -O https://raw.githubusercontent.com/git/git/master/contrib/completion/git-prompt.sh
    curl -O https://raw.githubusercontent.com/git/git/master/contrib/completion/git-completion.zsh
    mv git-completion.zsh _git
fi
