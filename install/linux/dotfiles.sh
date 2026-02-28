#!/bin/bash
# Install ssh-add-all and dotfiles

if [ "$OSDIST" = "ubuntu" ] || [ "$OSDIST" = "amazon" ]; then
    if [ ! -e ~/.ssh/ssh-add-all ]; then
        aws s3 cp --recursive s3://$BUCKET/.ssh ~/.ssh
        chmod 400 ~/.ssh/id*
        chmod +x ~/.ssh/ssh-add-all
    fi
fi
if [ ! -d ~/dotfiles ]; then
    cd
    git clone git@github.com:nirecom/dotfiles.git
    dotfiles/bin/dotfileslink.sh
fi
