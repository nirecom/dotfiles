#!/bin/bash
# Install emacs
# ref. https://qiita.com/MasahiroBW/items/f263e7a3dcfe69ec0561
if ! test emacs >/dev/null 2>&1; then
    sudo apt install emacs
fi
if [ ! -d ~/tmp ]; then
    mkdir ~/tmp
fi
if [ ! -d ~/.emacs_backup ]; then
    mkdir ~/.emacs_backup
fi
emacs --script ~/.emacs.d/packages/package-init.el
