#!/bin/bash
# Source Highlighting for less command
: "${DOTFILES_DIR:=$(cd "$(dirname "$0")/../.." && pwd)}"
source "$DOTFILES_DIR/bin/detectos.sh"

if type source-highlight >/dev/null 2>&1; then
    echo "source-highlight is already installed."
else
    echo "Installing source-highlight ..."
    case "$OSDIST" in
        "macos" )
            brew install source-highlight
            ;;
        "ubuntu" )
            sudo apt install -y source-highlight
            ;;
        "amazon" )
            sudo yum install -y source-highlight
            ;;
        * )
            echo "Not supported OS. Abort."
            exit 1
    esac
fi

case "$OSDIST" in
    "macos" )
        export SHAREDIR="/opt/homebrew/share/source-highlight"
        export LESSOPEN="| /opt/homebrew/bin/src-hilite-lesspipe.sh %s"
        ;;
    "ubuntu" )
        export SHAREDIR="/usr/share/source-highlight"
        export LESSOPEN="| $SHAREDIR/src-hilite-lesspipe.sh %s"
        ;;
    "amazon" )
        export SHAREDIR="/usr/share/source-highlight"
        export LESSOPEN="| $SHAREDIR/src-hilite-lesspipe.sh %s"
        ;;
    * )
        echo "Not supported OS. Abort."
        exit 1
esac
export LESS='-R'

echo "Customizing color table ..."
LOCALDIR="$DOTFILES_DIR"/source-highlight
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
