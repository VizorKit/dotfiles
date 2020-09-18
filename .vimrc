set nocompatible
set ruler
set number
set tabstop=2
set laststatus=2
set backspace=indent,eol,start

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
Plug 'tpope/vim-gitgutter'

call plug#end()

" lsc configuration
if has("unix")
    let g:lsc_server_commands = { 'java': '~/jls/java-language-server/dist/lang_server_linux.sh' }

elseif has("win32")
    let g:lsc_server_commands = { 'java': '$HOME\java-language-server\dist\lang_server_windows.sh' }
		let g:gitgutter_git_executable = 'C:\Program Files\Git\bin\git.exe'
endif

" nerdtree configuration
let NERDTreeShowHidden=1

" coding configuration
autocmd FileType cs,java,ts,js inoremap { {}<Left><Enter><Enter><Up><Tab>
autocmd FileType cs,java,ts,js inoremap ( ()<Left>
autocmd FileType cs,java,ts,js inoremap ' ''<Left>
autocmd FileType cs,java,ts,js inoremap " ""<Left>

" dotnet configuration
autocmd FileType cs inoremap <expr> <Tab> pumvisible() ? '<C-n>' : getline('.')[col('.')-2] =~# '[[:alnum:].-_#$]' ? '<C-x><C-o>' : SkipClosingParentheses()
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
