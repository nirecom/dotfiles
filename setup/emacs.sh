#!/bin/bash
# Install emacs
# ref. https://qiita.com/MasahiroBW/items/f263e7a3dcfe69ec0561
sudo apt install emacs
if [ ! ~/tmp ]; then
    mkdir ~/tmp
fi
if [ ! ~/.emacs_backup ]; then
    mkdir ~/.emacs_backup
fi
