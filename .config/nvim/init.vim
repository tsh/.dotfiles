call plug#begin()
Plug 'davidhalter/jedi-vim'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'zchee/deoplete-jedi'

Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

Plug 'jpalardy/vim-slime', { 'for': 'python' }
Plug 'hanschen/vim-ipython-cell', { 'for': 'python' }

Plug 'scrooloose/nerdcommenter'  
Plug 'sbdchd/neoformat'
Plug 'scrooloose/nerdtree'
Plug 'neomake/neomake'
Plug 'machakann/vim-highlightedyank'
Plug 'tmhedberg/SimpylFold'
Plug 'airblade/vim-gitgutter'
Plug 'iamcco/markdown-preview.nvim', { 'do': { -> mkdp#util#install() } }
Plug 'yuttie/comfortable-motion.vim'
Plug 'preservim/nerdtree'
"" COLORSCHEMES
Plug 'morhetz/gruvbox'
call plug#end()

set nocompatible

filetype on
filetype indent on
filetype plugin on

set fileencoding=utf-8
set encoding=utf-8

set visualbell
set backspace=indent,eol,start

"" SEARCH
set incsearch
set hlsearch
set ignorecase
set smartcase
xnoremap * :<C-u>call <SID>VSetSearch('/')<CR>/<C-R>=@/<CR><CR>
xnoremap # :<C-u>call <SID>VSetSearch('?')<CR>?<C-R>=@/<CR><CR>
function! s:VSetSearch(cmdtype)
  let temp = @s
  norm! gv"sy
  let @/ = '\V' . substitute(escape(@s, a:cmdtype.'\'), '\n', '\\n', 'g')
  let @s = temp
endfunction

"" CODING
syntax enable
set autoindent
set showmatch
set number

"" AUTOSAVE
:au FocusLost * silent! wa

"" UNDO
set undofile
set undodir=$HOME/.vim_undo_files
set undolevels=3000

set noswapfile

"" COLORSCHEME
colorscheme gruvbox
set background=dark 

"" MAPPING
nnoremap <Esc><Esc> :<C-u>nohlsearch<CR>
nmap <silent> <BS> :nohlsearch<CR>
map <C-n> :NERDTreeToggle<CR>

"" AUTOCOMPLETE
let g:deoplete#enable_at_startup = 1
autocmd InsertLeave,CompleteDone * if pumvisible() == 0 | pclose | endif
"" autocomplete with tab
inoremap <expr><tab> pumvisible() ? "\<c-n>" : "\<tab>"

"" AIRLINE
let g:airline_theme='wombat' 
"" autoformat
" Enable alignment
let g:neoformat_basic_format_align = 1
" Enable tab to spaces conversion
let g:neoformat_basic_format_retab = 1
" Enable trimmming of trailing whitespace
let g:neoformat_basic_format_trim = 1

"" JEDI
" disable autocompletion, cause we use deoplete for completion
let g:jedi#completions_enabled = 0
" open the go-to function in split, not another buffer
let g:jedi#use_splits_not_buffers = "right"

"" CODE CHECK 
let g:neomake_python_enabled_makers = ['pylint']

""FOLD
set foldlevel=99
"" SIMPLE FOLDING
let g:SimpylFold_docstring_preview = 1


"" NERDtree
nnoremap <leader>n :NERDTreeFocus<CR>
nnoremap <C-n> :NERDTree<CR>
nnoremap <C-t> :NERDTreeToggle<CR>
nnoremap <C-f> :NERDTreeFind<CR>

"" HIGHLIGHT YANK
" hi HighlightedyankRegion cterm=reverse gui=reverse<Paste>


