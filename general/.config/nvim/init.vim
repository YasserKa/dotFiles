call plug#begin(stdpath('data') . '/plugged')

Plug 'https://github.com/morhetz/gruvbox'
Plug 'https://github.com/itchyny/lightline.vim'
Plug 'https://github.com/tpope/vim-fugitive'
Plug 'https://github.com/kien/rainbow_parentheses.vim'
Plug 'https://github.com/jiangmiao/auto-pairs'
Plug 'https://github.com/altercation/vim-colors-solarized'
Plug 'https://github.com/tpope/vim-surround'
Plug 'https://github.com/liuchengxu/vista.vim', {'on' : 'Vista!!'}
Plug 'https://github.com/preservim/nerdcommenter'
Plug 'https://github.com/terryma/vim-multiple-cursors'
Plug 'https://github.com/junegunn/fzf.vim'
Plug 'https://github.com/dense-analysis/ale'
Plug 'http://github.com/maximbaz/lightline-ale'

Plug 'https://github.com/neoclide/coc.nvim', {'branch': 'release'}
Plug 'https://github.com/SirVer/ultisnips'
Plug 'https://github.com/honza/vim-snippets'

Plug 'https://github.com/mattn/emmet-vim'
Plug 'https://github.com/jwalton512/vim-blade', {'for': 'blade.php'}

Plug 'https://github.com/vim-scripts/dbext.vim', {'for': 'sql'}
Plug 'https://github.com/vim-scripts/SQLComplete.vim', {'for': 'sql'}
Plug 'https://github.com/plasticboy/vim-markdown', {'for': 'md'}
Plug 'https://github.com/iamcco/markdown-preview.nvim', { 'do': { -> mkdp#util#install() }, 'for': 'md'}
let g:solarized_termcolors=256
call plug#end()

" General settings
set clipboard+=unnamedplus    " System clipboard
set virtualedit=all           " Moving in whitespace
set ignorecase                " Do case insensitive search...
set smartcase                 " ...unless capital letters are used
set confirm                   " Confirm :q in case of unsaved changes
set autowrite                 " Save file when switching buffers

" Display settings
colorscheme gruvbox           " Set color scheme, must be installed first
set background=light          " Dark background for console
set guifont=Inconsolata:h14
set scrolloff=5               " Show 5 lines above/below the cursor
set cursorline                " Highlight current line"
set number                    " Show the number line
set relativenumber
set list                      " Show eof, trailing, etc..
set listchars=eol:¶,tab:>-,trail:.,nbsp:_,extends:+,precedes:+
set foldmethod=marker         " Folds at start
set foldmarker=<<<,>>>        " Folds at start

" Tabs, spaces, indentation, wrapping
set expandtab       " Use spaces for tabs
set tabstop=4       " Number of spaces to use for tabs
set shiftwidth=4    " Number of spaces to autoindent
set softtabstop=4   " Number of spaces for a tab
set smartindent     " Automatically insert another level of indent when needed
set nojoinspaces    " No extra space after '.' when joining lines
set textwidth=80    " Wrap lines automatically at 80th column

autocmd FileType html,blade setlocal shiftwidth=2 tabstop=2

let mapleader=","
" Keybindings
noremap ; :
noremap : ;
nnoremap q; q:
nnoremap @; @:
map Y y$
nnoremap <silent> <leader>es :split $MYVIMRC<CR>
nnoremap <silent> <leader>ss :source $MYVIMRC<CR>
nnoremap <leader>h :nohlsearch<CR>
" Moving lines
nnoremap <A-j> :m .+1<CR>==
nnoremap <A-k> :m .-2<CR>==
inoremap <A-j> <Esc>:m .+1<CR>==gi
inoremap <A-k> <Esc>:m .-2<CR>==gi
vnoremap <A-j> :m '>+1<CR>gv=gv
vnoremap <A-k> :m '<-2<CR>gv=gv

autocmd CmdwinEnter * map <buffer> <C-j> <CR>

" Plugins
" Display
"##############################################################
" lightline
let g:lightline = {}
let g:lightline.colorscheme = 'solarized'

let g:lightline.active = {
            \'left' : [ ['mode', 'paste'],
            \           ['cocstatus', 'currentfunction', 'gitbranch', 'readonly', 'filename', 'modified']],
            \'right': [ ['lineinfo'], ['percent'], ['filetype'],
            \           ['linter_checking', 'linter_errors', 'linter_warnings', 'linter_ok']
            \         ] }

let g:lightline.component_expand = {
            \  'linter_checking': 'lightline#ale#checking',
            \  'linter_warnings': 'lightline#ale#warnings',
            \  'linter_errors': 'lightline#ale#errors',
            \  'linter_ok': 'lightline#ale#ok',
            \ }

let g:lightline.component_type = {
            \     'linter_checking': 'left',
            \     'linter_warnings': 'warning',
            \     'cocstatus': 'warrning',
            \     'linter_errors': 'error',
            \     'linter_ok': 'left',
            \ }

let g:lightline.component_function = {
            \ 'gitbranch': 'fugitive#head',
            \ }

let g:lightline#ale#indicator_checking = "\uf110 "
let g:lightline#ale#indicator_warnings = "\uf071 "
let g:lightline#ale#indicator_errors = "\uf05e "
let g:lightline#ale#indicator_ok = "\uf00c "
"##############################################################
" rainbow_parentheses
au VimEnter * RainbowParenthesesToggle
au Syntax * RainbowParenthesesLoadRound
au Syntax * RainbowParenthesesLoadSquare
au Syntax * RainbowParenthesesLoadBraces
let g:rbpt_max = 16
"##############################################################
" Autopairs
let g:AutoPairs = { '<': '>', '(':')', '[':']', '{':'}',"'":"'",'"':'"', '```':'```', '"""':'"""', "'''":"'''", "`":"`"}
"##############################################################
" ALE
let g:ale_fix_on_save = 1

let g:ale_linters = {'python': ['flake8']}
let g:ale_fixers = {
            \   '*': ['remove_trailing_lines', 'trim_whitespace'],
            \   'python': ['autopep8'],
            \   'SQL': ['sqlint'],
            \}

" Auto remove ALE window on buffer exit
augroup CloseLoclistWindowGroup
    autocmd!
    autocmd QuitPre * if empty(&buftype) | lclose | endif
augroup END

" Mapping selecting mappings
imap <C-q> <plug>(fzf-maps-i)
nmap <leader><tab> <plug>(fzf-maps-n)
xmap <leader><tab> <plug>(fzf-maps-x)
omap <leader><tab> <plug>(fzf-maps-o)
"##############################################################
" Snippets
imap <C-l> <Plug>(coc-snippets-expand)
vmap <C-j> <Plug>(coc-snippets-select)

let g:coc_snippet_next = '<c-j>'
let g:coc_snippet_prev = '<c-k>'

imap <C-j> <Plug>(coc-snippets-expand-jump)
"##############################################################
" NerdCommenter
let g:NERDSpaceDelims = 1
let g:NERDCompactSexyComs = 1
let g:NERDDefaultAlign = 'left'
let g:NERDCommentEmptyLines = 1
let g:NERDTrimTrailingWhitespace = 1
let g:NERDToggleCheckAllLines = 1
"##############################################################
" FZF (Full path fuzzy file, buffer, mru, tag) finder
" Terminal buffer options for fzf
nnoremap <leader>a :Ag<space>
nnoremap <leader>p :Files<CR>
nnoremap <leader>o :Buffers<CR>

autocmd! FileType fzf set laststatus=0 noshowmode noruler
            \| autocmd BufLeave <buffer> set laststatus=2 showmode ruler
"##############################################################
" Taglist(Shows the structure of the code) Plugin
nnoremap <leader>t :Vista!!<CR>
"##############################################################
" Coc
set hidden
set updatetime=300
set shortmess+=c

autocmd CursorHold * silent call CocActionAsync('highlight')
xmap if <Plug>(coc-funcobj-i)
xmap af <Plug>(coc-funcobj-a)
omap if <Plug>(coc-funcobj-i)
omap af <Plug>(coc-funcobj-a)


" Mappings using CoCList:
nnoremap <silent> <space>a  <cmd>CocList diagnostics<cr>
nnoremap <silent> <space>c  <cmd>CocList commands<cr>
nnoremap <silent> <space>e  <cmd>CocList extensions<cr>

" GoTo code navigation.
nnoremap <silent>gd <Plug>(coc-definition)
nnoremap <silent>gr <Plug>(coc-references)
nnoremap <silent> <C-k> <Plug>(ale_previous_wrap)
nnoremap <silent> <C-j> <Plug>(ale_next_wrap)

nnoremap <silent> <leader>e :CocCommand explorer<CR>
nnoremap <leader>rn <Plug>(coc-rename)

nnoremap <silent> K :call <SID>show_documentation()<CR>
function! s:show_documentation()
    if (index(['vim','help'], &filetype) >= 0)
        execute 'h '.expand('<cword>')
    else
        call CocAction('doHover')
    endif
endfunction
