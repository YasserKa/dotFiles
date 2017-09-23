set nocompatible              " be iMproved, required
execute pathogen#infect()
filetype plugin indent on             " required
syntax on

filetype plugin on
set noshowmatch
let g:syntastic_cs_checkers = ['syntax', 'semantic', 'issues']



"Start searching at the start of a match
:set incsearch
"Omni sharp config
let g:Omnisharp_start_server = 0
let g:Omnisharp_stop_server  = 0
let g:OmniSharp_host="http://localhost:20001"
let g:ycm_csharp_server_port = 20001
let mapleader=","
" Add some spice
colorscheme gruvbox
set background=dark
" Chaneg tabs to spaces
set expandtab
" For YCM
set encoding=utf-8
" Makes a window appear while writting commands
set wildmenu
" Using C -> $ at the end
set cpoptions+=$
" Moving in whitespace
set virtualedit=all

set complete=.,w,b,t
set textwidth=80
set tabstop=4
set shiftwidth=4
set number
" Show trail, eol
set list listchars=trail:.,eol:¶

" for tags
nnoremap <silent> tt :TagbarToggle<CR>

" nerd tree plugin
nnoremap <silent> tr :NERDTreeToggle<CR>
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif

" If you prefer the Omni-Completion tip window to close when a selection is
" made, these lines close it on movement in insert mode or when leaving
" insert mode
autocmd CursorMovedI * if pumvisible() == 0|pclose|endif
autocmd InsertLeave * if pumvisible() == 0|pclose|endif

    let g:Omnisharp_start_server = 0
    let g:Omnisharp_stop_server  = 0
    let g:OmniSharp_host="http://localhost:20001"
    let g:ycm_csharp_server_port = 20001

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
" Syntastic options
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

" Rainbow colors for parentheses
autocmd VimEnter * RainbowParenthese
let g:rainbow#max_level = 16
let g:rainbow#pairs = [['(', ')'], ['[', ']'], ['{', '}'], ['<', '>']]

call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

" The following are examples of different formats supported.
" Keep Plugin commands between vundle#begin/end.
" plugin on GitHub repo
Plugin 'tpope/vim-fugitive'
" plugin from http://vim-scripts.org/vim/scripts.html
" Plugin 'L9'
" Git plugin not hosted on GitHub
Plugin 'git://git.wincent.com/command-t.git'
" git repos on your local machine (i.e. when working on your own plugin)
"Plugin 'file:///home/gmarik/path/to/plugin'
" The sparkup vim script is in a subdirectory of this repo called vim.
" Pass the path to set the runtimepath properly.
Plugin 'rstacruz/sparkup', {'rtp': 'vim/'}
" Install L9 and avoid a Naming conflict if you've already installed a
" different version somewhere else.
" Plugin 'ascenator/L9', {'name': 'newL9'}

Plugin 'scrooloose/syntastic'

Plugin 'Valloric/YouCompleteMe'
"" ultisnip
" Track the engine.
Plugin 'SirVer/ultisnips'

" Snippets are separated from the engine. Add this if you want them:
Plugin 'honza/vim-snippets'

Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'

" All of your Plugins must be added before the following line
call vundle#end()            " required


filetype plugin indent on    " required
" Trigger configuration. Do not use <tab> if you use https://github.com/Valloric/YouCompleteMe.
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"

" If you want :UltiSnipsEdit to split your window.
let g:UltiSnipsEditSplit="vertical"


" To ignore plugin indent changes, instead use:
"filetype plugin on
"
" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to auto-approve removal
"
" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this line
