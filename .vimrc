set nocompatible
set ruler
set number
set tabstop=2

syntax enable
" disable bells
set noeb vb t_vb=

color slate

" begin maps
map <C-b> :NERDTreeToggle<CR>


" begin plugins
call plug#begin('~/.vim/plugged')

Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'OmniSharp/omnisharp-vim', { 'for': 'cs' }
Plug 'vim-airline/vim-airline'
Plug 'inside/vim-search-pulse'

call plug#end()

" nerdtree configuration
let NERDTreeShowHidden=1


" dotnet configuration
autocmd FileType cs inoremap { {}<Left><Enter><Enter><Up><Tab>
autocmd FileType cs inoremap ( ()<Left>
autocmd FileType cs inoremap ' ''<Left>
autocmd FileType cs inoremap " ""<Left>
inoremap <expr> <Tab> pumvisible() ? '<C-n>' : getline('.')[col('.')-2] =~# '[[:alnum:].-_#$]' ? '<C-x><C-o>' : '<Tab>'
nnoremap <C-o><C-u> :OmniSharpFindUsages<CR>
nnoremap <C-o><C-d> :OmniSharpGotoDefinition<CR>
nnoremap <C-o><C-d><C-p> :OmniSharpPreviewDefinition<CR>
nnoremap <C-o><C-r> :!dotnet run
nnoremap <C-o><C-t> :!dotnet test