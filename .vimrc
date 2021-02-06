set nocompatible
set complete=.,w,b,u,t,i,]
set ignorecase
set ruler
set number
set tabstop=2
set laststatus=2
set backspace=indent,eol,start
set clipboard=unnamedplus
set shortmess+=c
set completeopt=menuone,longest
set hlsearch
set incsearch
set showmatch
set splitbelow
set splitright
set ttyfast
set wildmenu
set wildmode=full

syntax enable
" disable bells
set noeb vb t_vb=

let mapleader = ","

if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" plug Install automatically
autocmd VimEnter * if len(filter(values(g:plugs), '!isdirectory(v:val.dir)'))
  \| PlugInstall --sync | source $MYVIMRC
\| endif

set omnifunc=ale#completion#OmniFunc
let g:ale_sign_column_always = 1
let g:ale_sign_error = '✗'
let g:ale_sign_warning = '⚠️'
let g:ale_completion_enabled = 1
let g:ale_completion_autoimport = 1
let g:ale_fix_on_save = 1
let g:ale_fixers = {
												\ 'javascript':['eslint'],
												\ 'typescript':['tslint'],
												\ 'rust':['rustfmt'],
												\}
let g:ale_linters = {
												\ 'typescript':['tslint'],
												\ 'javascript':['eslint'],
												\ 'rust':['analyzer'],
												\}

call plug#begin('~/.vim/plugged')

Plug 'vim-airline/vim-airline'
Plug 'vim-scripts/AutoComplPop'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'dense-analysis/ale'

call plug#end()

inoremap { {}<Left>
inoremap ( ()<Left>
inoremap ' ''<Left>
inoremap " ""<Left>
inoremap [ []<Left>
inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
inoremap <expr> <C-n> pumvisible() ? '<C-n>' :
  \ '<C-n><C-r>=pumvisible() ? "\<lt>Down>" : ""<CR>'
inoremap <expr> <M-,> pumvisible() ? '<C-n>' :
  \ '<C-x><C-o><C-n><C-p><C-r>=pumvisible() ? "\<lt>Down>" : ""<CR>'
" open omni completion menu closing previous if open and opening new menu without changing the text
inoremap <expr> <C-Space> (pumvisible() ? (col('.') > 1 ? '<Esc>i<Right>' : '<Esc>i') : '') .
            \ '<C-x><C-o><C-r>=pumvisible() ? "\<lt>C-n>\<lt>C-p>\<lt>Down>" : ""<CR>'
" open user completion menu closing previous if open and opening new menu without changing the text
inoremap <expr> <S-Space> (pumvisible() ? (col('.') > 1 ? '<Esc>i<Right>' : '<Esc>i') : '') .
            \ '<C-x><C-u><C-r>=pumvisible() ? "\<lt>C-n>\<lt>C-p>\<lt>Down>" : ""<CR>'
inoremap <expr> <Tab> pumvisible() ? '<C-n>' : SkipClosingPair()


nnoremap <leader>cb :vert :term cargo build<CR><C-W><C-w>
nnoremap <leader>cr :vert :term cargo run<CR><C-w><C-w>

" functions
function! SkipClosingPair()
  let line = getline('.')
  let current_char = line[col('.')-1]
	"there is more"
  "Ignore EOL
  if col('.') == col('$')
    return "\<Tab>"
  end
  return stridx("}])\'\"", current_char)==-1 ? "\<Tab>" : "\<Right>"
endfunction

" mappings.
vmap <C-c> "+y<Esc>
nnoremap <C-p> :GFiles<CR>
nnoremap <C-b> :Buffers<CR>


nmap <silent> <C-l>g :ALEGoToDefinition<CR>
nmap <silent> <C-l>. :ALECodeAction<CR>
nmap <silent> <C-l>s :ALESymbolSearch<CR>
nmap <silent> <C-l>r :ALERename<CR>
nmap <silent> <C-l>h :ALEHover<CR>
nmap <silent> <C-l>/ :ALEFindReferences<CR>
nmap <silent> <C-l>a <Plug>(ale_previous_wrap)
nmap <silent> <C-l>d <Plug>(ale_next_wrap)
