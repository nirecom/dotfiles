#!/bin/bash
# Install go
source ./bin/detectos.sh

if type go >/dev/null 2>&1; then
    echo "go is installed."
    exit 1
fi

case "$OSDIST" in
    "ubuntu" )
        FILENAME=go1.15.7.linux-amd64.tar.gz
        cd /tmp/$FILENAME
        curl -LO https://golang.org/dl/$FILENAME
        sudo tar -C /usr/local -xzf /tmp/$FILENAME
        export PATH=$PATH:/usr/local/go/bin
        ;;
    "macos" )
        # brew-based goenv supports up to 1.11.4
        #brew install goenv

#        if [ ! -d $HOME/.goenv ]; then
#            echo "Installing goenv..."
#            git clone https://github.com/syndbg/goenv.git ~/.goenv
#        fi
#        # env settings are in .dotfiles/.profiles_common
#        echo "Setting goenv variables..."
#        export GOENV_ROOT="$HOME/.goenv"
#        eval "$(goenv init -)"

        anyenv install goenv

        echo "Install go..."
        if "$ISM1"; then
            INSTALLGO=1.16beta1
        else
            INSTALLGO=1.15.7
        fi
        goenv install $INSTALLGO
        goenv global $INSTALLGO
        exec $SHELL -l
        ;;
    * )
        echo "Not supported OS."
        exit 1
esac
go version
