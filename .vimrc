set nocompatible
set ruler
set number
set tabstop=2
set laststatus=2
set backspace=indent,eol,start

syntax enable
" disable bells
set noeb vb t_vb=

" plug setup if not on machine
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" plug Install automatically
autocmd VimEnter * if len(filter(values(g:plugs), '!isdirectory(v:val.dir)'))
  \| PlugInstall --sync | source $MYVIMRC
\| endif


call plug#begin('~/.vim/plugged')

Plug 'vim-airline/vim-airline'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

call plug#end()



autocmd FileType cs,java,ts,js,rs inoremap { {}<Left><Enter><Enter><Up><Tab>
autocmd FileType cs,java,ts,js,rs inoremap ( ()<Left>
autocmd FileType cs,java,ts,js,rs inoremap ' ''<Left>
autocmd FileType cs,java,ts,js,rs inoremap " ""<Left>

autocmd FileType cs inoremap <expr> <Tab> pumvisible() ? '<C-n>' : getline('.')[col('.')-2] =~# '[[:alnum:].-_#$]' ? '<C-x><C-o>' : SkipClosingParentheses()

" function declarations
" Skip closing parenthesis, need to add to all languages
function! SkipClosingParentheses()
  let line = getline('.')
  let current_char = line[col('.')-1]

  "Ignore EOL
  if col('.') == col('$')
    return "\<Tab>"
  end

  return stridx("]})\'\"", current_char)==-1 ? "\<Tab>" : "\<Right>"
endfunction
