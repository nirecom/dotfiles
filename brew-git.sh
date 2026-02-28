#!/bin/bash
# Install brew and git
source ./bin/detectos.sh

if [ "$OSDIST" != "macos" ]; then
    echo "Skipping brew / git installation only for macos..."
    exit 1
fi

echo "Installing Rosetta..."
softwareupdate --install-rosetta --agree-to-license

# Install Brew on macos
if ! type brew >/dev/null 2>&1; then
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
# brew shellenv will be done at .profile_common under dotfiles/ repository
#    if [ ! grep "$BREWPATH" $HOME/.profile >/dev/null 2&>1 ]; then
#        echo 'eval $(/opt/homebrew/bin/brew shellenv)' >>$HOME/.profile
#    fi
#    if [ ! grep "$BREWPATH" $HOME/.profile >/dev/null 2&>1 ]; then
#        echo 'eval $(/opt/homebrew/bin/brew shellenv)' >>$HOME/.zprofile
#    fi
fi
if "$ISM1"; then
    eval $(/opt/homebrew/bin/brew shellenv)
else
    eval $(/usr/local/bin/brew shellenv)
fi

# two dependencies of git
# https://github.com/Homebrew/discussions/discussions/439
#brew reinstall gettext pcre2

brew install git
