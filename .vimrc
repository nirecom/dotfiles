scriptencoding utf-8
set encoding=utf-8
set visualbell t_vb=
set number
set list listchars=tab:\▸\-
set ignorecase
set smartcase

execute pathogen#infect()
syntax on
filetype plugin indent on

colorscheme solarized
let g:solarized_termcolors=256
let g:solarized_contrast = "high"
let g:solarized_visibility="low"
let g:vim_json_syntax_conceal = 0
set background=dark
