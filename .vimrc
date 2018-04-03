set nocompatible

filetype on
filetype indent on
filetype plugin on

set fileencoding=utf-8
set encoding=utf-8

set visualbell
set backspace=indent,eol,start

" Search
set incsearch
set hlsearch
set ignorecase
set smartcase

# Coding
syntax enable
set autoident
set showmatch
set number

set noswapfile

" Mapping
nnoremap <Esc><Esc> :<C-u>nohlsearch<CR>
