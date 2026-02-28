#!/bin/bash
# Source Highlighting for less command
source ./bin/detectos.sh

echo "Installing less source-highlight ..."
case "$OSDIST" in
    "macos" )
        brew install source-highlight
#        export SHAREDIR="/usr/local/share/source-highlight"
        export SHAREDIR="/opt/homebrew/share/source-highlight"
        export LESSOPEN="| /opt/homebrew/bin/src-hilite-lesspipe.sh %s"
        ;;
    "ubuntu" )
        sudo apt install -y source-highlight
        export SHAREDIR="/usr/share/source-highlight"
        export LESSOPEN="| $SHAREDIR/src-hilite-lesspipe.sh %s"
        ;;
    "amazon" )
        sudo yum install -y source-highlight
        export SHAREDIR="/usr/share/source-highlight"
        export LESSOPEN="| $SHAREDIR/src-hilite-lesspipe.sh %s"
        ;;
    * )
        echo "Not supported OS. Abort."
        exit 1
esac
export LESS='-R'

echo "Customizing color table ..."
LOCALDIR=$HOME/dotfiles/source-highlight
if [ -f $LOCALDIR/esc.style ]; then
    sudo mv $SHAREDIR/esc.style $SHAREDIR/esc.style.orig
    sudo cp $LOCALDIR/esc.style $SHAREDIR
    sudo chmod 644 $SHAREDIR/esc.style
    case "$OSDIST" in
        "macos" ) sudo chown nire $SHAREDIR/esc.style ;;
        "ubuntu" ) sudo chown root:root $SHAREDIR/esc.style ;;
        "amazon" ) sudo chown root:root $SHAREDIR/esc.style ;;
    esac
fi
exec $SHELL -l
