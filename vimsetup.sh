# pathogen
mkdir -p ~/.vim/autoload ~/.vim/bundle && \
curl -LSso ~/.vim/autoload/pathogen.vim https://tpo.pe/pathogen.vim
cd ~/.vim/bundle
# plugins
git clone git://github.com/tpope/vim-sensible.git
git clone git@github.com:editorconfig/editorconfig-vim.git
git clone git://github.com/altercation/vim-colors-solarized.git

# vim-json
git clone https://github.com/elzr/vim-json.git
ln -sf ~/dotfiles/filetype.vim ~/.vim/
