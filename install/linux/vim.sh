#!/bin/bash
echo "Installing vim ..."
# pathogen
mkdir -p ~/.vim/autoload ~/.vim/bundle
if [ ! -f ~/.vim/autoload/pathogen.vim ]; then
    curl -LSso ~/.vim/autoload/pathogen.vim https://tpo.pe/pathogen.vim
else
    echo "pathogen.vim is already installed."
fi
cd ~/.vim/bundle
# plugins
[ ! -d ./vim-sensible ] && git clone git@github.com:tpope/vim-sensible.git
[ ! -d ./editorconfig-vim ] && git clone git@github.com:editorconfig/editorconfig-vim.git
[ ! -d ./vim-colors-solarized ] && git clone git@github.com:altercation/vim-colors-solarized.git

# vim-json
[ ! -d ./vim-json ] && git clone git@github.com:elzr/vim-json.git
ln -sf ~/dotfiles/filetype.vim ~/.vim/
