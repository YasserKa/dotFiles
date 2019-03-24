" Keymaps {{{
"<leader>a  :Ag
"<leader>h  :nohlsearch<CR>
"<leader>t  tagbarTagbarToggle<CR>
"<leader>n  nerdtree
"<leader>s  easymotion
"<leader>ev :vsp $MYVIMRC<CR>
"<leader>sv :source $MYVIMRC<CR>
"toggle current fold
nnoremap  <space> za
" }}}
" First things come first {{{
" must be first instructions
set nocompatible " vim instead of Vi

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'VundleVim/Vundle.vim' " required

Plugin 'morhetz/gruvbox'
Plugin 'altercation/vim-colors-solarized'
Plugin 'itchyny/lightline.vim'
Plugin 'maximbaz/lightline-ale'
Plugin 'junegunn/rainbow_parentheses.vim'
Plugin 'majutsushi/tagbar'
Plugin 'tpope/vim-surround'
Plugin 'scrooloose/nerdtree'
Plugin 'jiangmiao/auto-pairs'
Plugin 'scrooloose/nerdcommenter'
Plugin 'terryma/vim-multiple-cursors'
Plugin 'kamwitsta/flatwhite-vim'
Plugin 'heavenshell/vim-jsdoc'
Plugin 'mattn/emmet-vim'

Plugin 'xolox/vim-easytags'
" required by easy-tags
Plugin 'xolox/vim-misc'
Plugin 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plugin 'junegunn/fzf.vim'
Plugin 'Valloric/YouCompleteMe'
Plugin 'w0rp/ale'
Plugin 'SirVer/ultisnips'
Plugin 'honza/vim-snippets'

Plugin 'tpope/vim-fugitive'
Plugin 'airblade/vim-gitgutter'

call vundle#end() " required
" }}}
" general settings{{{
syntax on
set clipboard=unnamedplus " make it able to paste using CTRL+V
set virtualedit=all  " moving in whitespace
let mapleader="," " changing the leader from \ to ,
set modelines=1 " sets the number of lines at start and end of file
" edit and load vimrc bindings
nnoremap <leader>ev :split $MYVIMRC<CR>
nnoremap <leader>sv :source $MYVIMRC<CR>
nnoremap <leader>r :split ~/.notes<CR>
" swap ; and : for a faster command access
nnoremap ; :
nnoremap : ;
" sort by the length of the line in visual mode
xnoremap <leader>s  : ! awk '{ print length(), $0 \| "sort -n \| cut -d\\  -f2-" }'<CR><CR>
" Make Y act like D and C
map Y y$
" Open definition in new vertical split
nnoremap <leader>ds :vsp <CR>:exec("tag ".expand("<cword>"))<CR>
" Open definition in new tab
nnoremap <leader>dt :tab split<CR>:exec("tag ".expand("<cword>"))<CR>
vnoremap <C-c> "*y
" }}}
" Display settings{{{
set encoding=utf-8 " encoding used for displaying file
colorscheme gruvbox " set color scheme, must be installed first
" colorscheme solarized gruvbox
" needed to work in terminal emulator
let g:solarized_termcolors=256
set background=light " dark background for console
set laststatus=2 " display the status line always
set number " show the number line
set list " show eof, trailing, etc..
set showcmd  " show command in bottom bar
set cursorline " highlight current line"
set wildmenu " makes a window appear while writting commands
"set foldmethod=indent " folds at start
" change cursor between vertical and block
let &t_SI = "\<Esc>[6 q"
let &t_SR = "\<Esc>[4 q"
let &t_EI = "\<Esc>[2 q"
set cpoptions+=$ " using C command adds $ at the end
" characters for displaying non-printable characters
set listchars=eol:¶,tab:>-,trail:.,nbsp:_,extends:+,precedes:+
" tip window to close when a selection is made
autocmd CursorMovedI * if pumvisible() == 0|pclose|endif
autocmd InsertLeave * if pumvisible() == 0|pclose|endif
"}}}
" write settings{{{
set confirm  " confirm :q in case of unsaved changes
set fileencoding=utf-8 " encoding used when saving file
set nobackup " do not keep the backup~ file
" "}}}
 " edit settings{{{
 set backspace=indent,eol,start " backspacing over everything in insert mode
 set expandtab " fill tabs with spaces
 set nojoinspaces " no extra space after '.' when joining lines
 set shiftwidth=4 " set indentation depth to 8 columns
 set softtabstop=4 " backspacing over 8 spaces like over tabs
 set tabstop=4 " set tabulator length to 8 columns
 set textwidth=80 " wrap lines automatically at 80th column
 " Save file when switching buffers
 set autowrite
 set autoread
 "}}}
 " search settings{{{
 set ignorecase " do case insensitive search...
 set incsearch " do incremental search
 set hlsearch " highlight mathces
 set smartcase " ...unless capital letters are used
 " turn off searchhighlight
 nnoremap <leader>h :nohlsearch<CR>
 "}}}
" file type specific settings{{{
filetype on " enable file type detection
filetype plugin on " load the plugins for specific file types
filetype indent on " automatically indent code
"}}}
" automatic commands{{{
if has('autocmd')
    " clean-up commands that run automatically on write; use with caution

    " delete empty or whitespaces-only lines at the end of file
    autocmd BufWritePre * :%s/\(\s*\n\)\+\%$//ge

    " replace groups of empty or whitespaces-only lines with one empty line
    " autocmd BufWritePre * :%s/\(\s*\n\)\{3,}/\r\r/ge

    " delete any trailing whitespaces
    autocmd BufWritePre * :%s/\s\+$//ge
endif
"}}}
" Plugins{{{
" Display {{{
"##############################################################
" lightline-ale
let g:lightline = {}
let g:lightline.colorscheme = 'solarized'

let g:lightline.component_expand = {
      \  'linter_checking': 'lightline#ale#checking',
      \  'linter_warnings': 'lightline#ale#warnings',
      \  'linter_errors': 'lightline#ale#errors',
      \  'linter_ok': 'lightline#ale#ok',
      \ }
let g:lightline.component_type = {
      \     'linter_checking': 'left',
      \     'linter_warnings': 'warning',
      \     'linter_errors': 'error',
      \     'linter_ok': 'left',
      \ }
let g:lightline.active = { 'right': [['lineinfo'],['percent'],['filetype'], [ 'linter_checking', 'linter_errors', 'linter_warnings',  'linter_ok' ]] }

let g:lightline#ale#indicator_checking = "\uf110 "
let g:lightline#ale#indicator_warnings = "\uf071 "
let g:lightline#ale#indicator_errors = "\uf05e "
let g:lightline#ale#indicator_ok = "\uf00c "
" rainbow_parentheses
" fork https://github.com/junegunn/rainbow_parentheses.vim
" start rainbow at start
autocmd VimEnter * RainbowParentheses

let g:rainbow#max_level = 16
let g:rainbow#pairs = [['(', ')'], ['[', ']'], ['{', '}']]
" gitgutter
set updatetime=250
" vim-javascript
" JSdocs
set foldmethod=syntax
set foldcolumn=1
let javaScript_fold=1
set foldlevelstart=99

" folding
" augroup END
"}}}
" Editing{{{
" YouCompleteMe
let g:ycm_max_num_candidates = 6
"##############################################################
" ALE
" Enable completion where available.
let g:ale_completion_enabled = 1
let g:ale_open_list = 1
let g:ale_echo_cursor = 0

let g:ale_fixers = {
\   '*': ['remove_trailing_lines', 'trim_whitespace'],
\   'javascript': ['jshint'],
\   'python': ['flake8'],
\}
nnoremap <leader>g :YcmCompleter GoToDefinitionElseDeclaration<CR>
"##############################################################
" UltiSnips
" defines the directory private snippet definition files are stored in.
let g:UltiSnipsSnippetDir = [$HOME.'/.vim/Ultisnips']
" configures the directories that are searched for snippets
let g:UltiSnipsSnippetDirectories = [$HOME.'/.vim/Ultisnips']
let g:UltiSnipsListSnippets = "<c-l>"
let g:UltiSnipsExpandTrigger = "<c-j>"
let g:UltiSnipsJumpForwardTrigger = "<c-j>"
let g:UltiSnipsJumpBackwardTrigger = "<c-k>"
" if you want :UltiSnipsEdit to split your window.
let g:UltiSnipsEditSplit = "vertical"
"##############################################################
" nerdCommenter
" Add spaces after comment delimiters by default
let g:NERDSpaceDelims = 1
" Use compact syntax for prettified multi-line comments
let g:NERDCompactSexyComs = 1
" Align line-wise comment delimiters flush left instead of following code indentation
let g:NERDDefaultAlign = 'left'
" Allow commenting and inverting empty lines (useful when commenting a region)
let g:NERDCommentEmptyLines = 1
" Enable trimming of trailing whitespace when uncommenting
let g:NERDTrimTrailingWhitespace = 1
" Enable NERDCommenterToggle to check all selected lines is commented or not
let g:NERDToggleCheckAllLines = 1
"##############################################################
" JsDoc
nmap <silent> <C-l> <Plug>(jsdoc)
" PhpDocs
" source ~/.vim/bundle/php-doc.vim
" inoremap <C-P> <ESC>:call PhpDocSingle()<CR>i
" nnoremap <C-P> :call PhpDocSingle()<CR>
" vnoremap <C-P> :call PhpDocRange()<CR>
"}}}
" Navigation {{{
" Ag (Searching source code in a project)
nnoremap <leader>a :Ag<space>
"##############################################################
" FZF (Full path fuzzy file, buffer, mru, tag) finder
nnoremap <leader>p :Files<CR>
"##############################################################
" nerdtree (hierarchy of files) plugin
nnoremap <leader>n :NERDTreeToggle<CR>
" automatically when no files were specified
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif
" ##############################################################
" Taglist(Shows the structure of the code) Plugin
" remap the toggle
nnoremap <leader>t :TagbarToggle<CR>
"##############################################################
" easymotion
"let g:EasyMotion_do_mapping = 0 " Disable default mappings
" Jump to anywhere you want with minimal keystrokes, with just one key binding.
" `s{char}{label}`
nmap <Leader>s <Plug>(easymotion-overwin-f)
" Turn on case insensitive feature
let g:EasyMotion_smartcase = 1
" JK motions: Line motions
map <Leader>j <Plug>(easymotion-j)
map <Leader>k <Plug>(easymotion-k)
" }}}
" General {{{
" fugitive
" nnoremap <leader>gb :Gblame<CR>
" nnoremap <leader>gs :Gstatus<CR>
" nnoremap <leader>gd :Gdiff<CR>
" nnoremap <leader>gl :Glog<CR>
" nnoremap <leader>gc :Gcommit<CR>
" nnoremap <leader>gp :Git push<CR>
" }}}
" To check undo list: gundo
" }}}
" Extras {{{
function! Get_visual_selection()
    let [line_start, column_start] = getpos("'<")[1:2]
    let [line_end, column_end] = getpos("'>")[1:2]
    let lines = getline(line_start, line_end)
    if len(lines) == 0
        return ''
    endif
    let lines[-1] = lines[-1][: column_end - (&selection == 'inclusive' ? 1 : 2)]
    let lines[0] = lines[0][column_start - 1:]
    return join(lines, "\n")
endfunction
" function! Open_unity_docs()
"     silent !clear
"     let path = "file:///opt/Unity/Documentation/en/ScriptReference/30_search.html?q="
"     execute "!firefox ".path.Get_visual_selection()." &"
" endfunction
" vmap <leader>u :call Open_unity_docs()<CR>
"}}}
"set foldmethod=syntax
" used to fold sections by markers and fold them by default
" vim:foldmethod=marker:foldlevel=0
