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
Plug 'natebosch/vim-lsc', { 'for': 'java' }

call plug#end()


" lsc configuration
if has("unix")
    let g:lsc_server_commands = { 'java': '~/jls/java-language-server/dist/lang_server_linux.sh' }
elseif has("win32")
    let g:lsc_server_commands = { 'java': '$HOME\java-language-server\dist\lang_server_windows}.sh' }
endif

" nerdtree configuration
let NERDTreeShowHidden=1

" coding configuration
autocmd FileType cs,java,ts inoremap { {}<Left><Enter><Enter><Up><Tab>
autocmd FileType cs,java,ts inoremap ( ()<Left>
autocmd FileType cs,java,ts inoremap ' ''<Left>
autocmd FileType cs,java,ts inoremap " ""<Left>

" dotnet configuration
autocmd FileType cs inoremap <expr> <Tab> pumvisible() ? '<C-n>' : getline('.')[col('.')-2] =~# '[[:alnum:].-_#$]' ? '<C-x><C-o>' : '<Tab>'
nnoremap <C-o><C-u> :OmniSharpFindUsages<CR>
nnoremap <C-o><C-d> :OmniSharpGotoDefinition<CR>
nnoremap <C-o><C-p> :OmniSharpPreviewDefinition<CR>
nnoremap <C-o><C-c> :!dotnet clean
nnoremap <C-o><C-r> :!dotnet run
nnoremap <C-o><C-t> :!dotnet test

" java configuration
nnoremap <C-m><C-c> :!mvnw clean verify compile
nnoremap <C-m><C-i> :!mvnw install
" mvn -Dtest=class#method test



" angular configuration
