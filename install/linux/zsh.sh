#!/bin/bash
source ./bin/detectos.sh

echo "Installing zsh ..."
if [ "$OSDIST" = "mingw" ]; then
    echo "mingw does not support zsh. Abort."
    exit 1
fi

if [ ! -e /bin/zsh ]; then
    case "$OSDIST" in
        "ubuntu" )
            sudo apt install -y zsh ;;
        "amazon" )
            sudo yum install -y zsh util-linux-user ;;
        "*" )
            echo "Not supported Linux. Abort."
            exit 1
    esac
fi
if [ ! "`echo $SHELL | grep zsh`" ]; then
    echo "Changing shell to zsh. Enter password ..."
    chsh -s $(which zsh)
fi

# Install zinit, package manager
if [ ! -d $HOME/.zinit ]; then
    sh -c "$(curl -fsSL https://raw.githubusercontent.com/zdharma/zinit/master/doc/install.sh)"
fi
