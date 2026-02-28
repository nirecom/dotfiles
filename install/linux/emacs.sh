#!/bin/bash
# Install emacs
source ./bin/detectos.sh
if [ "$OSDIST" = "amazon" ]; then
        echo "Amazon Linux supports older emacs only. Abort."
        exit 1
fi

echo "Installing emacs ..."
if ! type emacs >/dev/null 2>&1; then
    case "$OSDIST" in
        "macos" )
            brew install emacs
            # Support Copy & Paste at macos side
            brew install reattach-to-user-namespace
            ;;
        "ubuntu" )
            # ref. https://qiita.com/MasahiroBW/items/f263e7a3dcfe69ec0561
            sudo apt install -y emacs
            ;;
        * )
            echo "not supported OS. Install manually."
            exit 1
    esac
fi

echo "Creating tmp / backup folders ..."
[ ! -d ~/tmp ] && mkdir ~/tmp
[ ! -d ~/.emacs_backup ] && mkdir ~/.emacs_backup

INSTALLER="$HOME/dotfiles/.emacs.d/package-install.el"
if [ -f $INSTALLER ]; then
    echo "Installing packages ..."
    emacs --script $INSTALLER
fi

#echo "Git cloning tools ..."
#GDIR=$HOME/.emacs.d/git
#mkdir -p $GDIR
#cd $GDIR
#if [ ! -f git-complete.el ]; then
#    curl -L https://raw.githubusercontent.com/zk-phi/git-complete/master/git-complete.el -o git-complete.el
#fi

