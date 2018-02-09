call vundle#begin()

Plugin 'jiangmiao/auto-pairs'
Plugin 'jnurmine/zenburn'
Plugin 'Shougo/vimproc.vim'

call vundle#end()            " required
filetype plugin indent on    " required

colors zenburn

syntax enable

filetype plugin indent on

set number

" show existing tab with 4 spaces width
set tabstop=4
" when indenting with '>', use 4 spaces width
set shiftwidth=4
" On pressing tab, insert 4 spaces
set expandtab
