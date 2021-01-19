#!/bin/bash
# Install emacs
# ref. https://qiita.com/MasahiroBW/items/f263e7a3dcfe69ec0561
sudo apt install emacs
if [ ! -d ~/tmp ]; then
    mkdir ~/tmp
fi
if [ ! -d ~/.emacs_backup ]; then
    mkdir ~/.emacs_backup
fi
emacs --script ~/.emacs.d/packages/package-init.el
