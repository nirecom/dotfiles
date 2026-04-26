#!/bin/bash
: "${DOTFILES_DIR:=$(cd "$(dirname "$0")/../.." && pwd)}"
source "$DOTFILES_DIR/bin/detectos.sh"

echo OSDIST=$OSDIST
echo ISWSL=$ISWSL
echo ISM1=$ISM1
if [ "$OSDIST" = "ubuntu" ]; then
    echo "passed ubuntu test!"
fi
if "$ISWSL"; then
    echo "passed wsl test!"
fi
if "$ISM1"; then
    echo "passed M1 test!"
fi
