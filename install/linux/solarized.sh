#!/bin/bash
cd ~
if [ ! -d solarized ]; then
    git clone git://github.com/altercation/solarized.git
fi
SOLARDEST=~/solarized

mkdir -p ~/.vim/colors/
cp $SOLARDEST/vim-colors-solarized/colors/solarized.vim ~/.vim/colors/
