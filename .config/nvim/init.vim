" vim-plug <<<
call plug#begin(stdpath('data') . '/plugged')

Plug 'https://github.com/morhetz/gruvbox'

Plug 'https://github.com/itchyny/lightline.vim'
Plug 'https://github.com/maximbaz/lightline-ale'
Plug 'https://github.com/tpope/vim-fugitive'
Plug 'https://github.com/kien/rainbow_parentheses.vim'
Plug 'https://github.com/tmsvg/pear-tree'
Plug 'https://github.com/tpope/vim-surround'
Plug 'https://github.com/tpope/vim-unimpaired'
Plug 'https://github.com/tpope/vim-repeat'
Plug 'https://github.com/wellle/targets.vim'
Plug 'https://github.com/liuchengxu/vista.vim', {'on': 'Vista!!'}
Plug 'https://github.com/tpope/vim-commentary'

Plug 'https://github.com/simnalamburt/vim-mundo', {'on': 'MundoToggle'}

Plug 'https://github.com/terryma/vim-multiple-cursors'
Plug 'https://github.com/junegunn/fzf.vim'
Plug 'https://github.com/dense-analysis/ale'
Plug 'https://github.com/neoclide/coc.nvim', {'branch': 'release'}
Plug 'https://github.com/SirVer/ultisnips'
Plug 'https://github.com/honza/vim-snippets'
Plug 'https://github.com/mattn/emmet-vim', {'for': ['html', 'blade.php', 'vue']}
Plug 'https://github.com/plasticboy/vim-markdown'


Plug 'https://github.com/jwalton512/vim-blade', {'for': 'blade.php'}
Plug 'https://github.com/posva/vim-vue', {'for': 'vue'}

Plug 'https://github.com/vim-scripts/dbext.vim', {'for': 'sql'}
Plug 'https://github.com/vim-scripts/SQLComplete.vim', {'for': 'sql'}
Plug 'https://github.com/iamcco/markdown-preview.nvim', { 'do': 'cd app & yarn install', 'for': ['markdown', 'plantuml']}



Plug 'https://github.com/lervag/vimtex', {'for': ['tex', 'markdown']}
Plug 'https://github.com/dhruvasagar/vim-table-mode'
Plug 'https://github.com/aklt/plantuml-syntax', { 'for': ['markdown']}

call plug#end()
" >>>
" general settings <<<
set clipboard+=unnamedplus    " System clipboard
set virtualedit=all           " Moving in whitespace
set ignorecase                " Do case insensitive search...
set smartcase                 " ...unless capital letters are used
set confirm                   " Confirm :q in case of unsaved changes
set autowrite                 " Save file when switching buffers

colorscheme gruvbox
let g:gruvbox_italic=1
set background=light
set termguicolors
set guifont=Inconsolata:h14
set dictionary=/usr/share/dict/cracklib-small
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
set shiftwidth=4    " Number of spaces to autoindent
set softtabstop=4   " Number of spaces for a tab
set nojoinspaces    " No extra space after '.' when joining lines
set textwidth=80    " Wrap lines automatically at 80th column

" Keybindings
let mapleader=","
let maplocalleader=","
noremap ; :
noremap : ;
noremap g: g;
noremap q; q:
noremap @; @:
map Y y$
nnoremap <silent> <leader>es :split $MYVIMRC<CR>
nnoremap <silent> <leader>ss :source $MYVIMRC<CR>
nnoremap <silent> <leader>h :nohlsearch<CR>
inoremap <C-l> <c-g>u<Esc>[s1z=`]a<c-g>u
" notes
nnoremap <silent> <leader>nm :split $HOME/notes/markdown/math.md<CR>
nnoremap <silent> <leader>nb :split $HOME/notes/markdown/books.md<CR>
nnoremap <silent> <leader>nl :silent !zathura $HOME/notes/documents/symbols.pdf &<CR>
nnoremap <silent> <leader>nn :silent !conda run jupyter notebook --ip=127.0.0.1 ~/notes/notebook/personal_notebook.ipynb & disown<CR>

" trigger tex file
nnoremap <silent> yol :call ToggleTex()<CR>

function ToggleTex()
    if &filetype == 'tex'
        set filetype=markdown
    else
        set filetype=tex
    endif
endfunction

" Moving lines
nnoremap <A-j> :m .+1<CR>==
nnoremap <A-k> :m .-2<CR>==
inoremap <A-j> <Esc>:m .+1<CR>==gi
inoremap <A-k> <Esc>:m .-2<CR>==gi
vnoremap <A-j> :m '>+1<CR>gv=gv
vnoremap <A-k> :m '<-2<CR>gv=gv

" map j k to make it work in the same line (lines not wraped)
nnoremap <expr> k      v:count == 0 ? 'gk' : 'k'
nnoremap <expr> j      v:count == 0 ? 'gj' : 'j'

" Sessions
set sessionoptions-=options
set sessionoptions+=localoptions

function! MakeSession()
  let b:sessiondir = $XDG_DATA_HOME . "/nvim/sessions" . getcwd()
  if (filewritable(b:sessiondir) != 2)
    execute 'silent !mkdir -p ' b:sessiondir
  endif
  let b:filename = b:sessiondir . '/session.vim'
  execute "mksession! " . b:filename
endfunction

function! LoadSession()
    let b:sessiondir = $XDG_DATA_HOME . "/nvim/sessions" . getcwd()
    let b:sessionfile = b:sessiondir . "/session.vim"
    if (filereadable(b:sessionfile))
        execute 'silent source ' b:sessionfile
    endif
endfunction

" AutoCommands
augroup SESSIONS
    autocmd!
    if(argc() == 0)
        autocmd VimEnter * nested call LoadSession()
    endif
    autocmd VimLeave * call MakeSession()
augroup END

augroup VIMENTER
    autocmd!
    autocmd CmdwinEnter * map <buffer> <C-j> <CR>
    autocmd BufRead,BufNewFile neomutt-* set filetype=markdown spell
    autocmd FileType markdown,tex set spell
    autocmd FileType html,blade,vue,yaml setlocal shiftwidth=2 tabstop=2
    autocmd FileType python let b:match_words = '\<if\>:\<elif\>:\<else\>'
augroup END

augroup MARKDOWN
    autocmd!
    autocmd FileType markdown syntax match StrikeoutMatch /\~\~.*\~\~/
    highlight def  StrikeoutHighlight   cterm=strikethrough gui=strikethrough
    highlight link StrikeoutMatch StrikeoutHighlight

    autocmd Filetype markdown syntax match UnderlineMatch /__.*__/
    highlight def  UnderlineHighlight   cterm=underline gui=underline
    highlight link UnderlineMatch UnderlineHighlight
augroup END

" >>>
" lightline.vim <<<
let g:lightline = {}

let g:lightline.colorscheme= 'gruvbox'
let g:lightline.active = {
            \'left' : [ ['mode', 'paste'],
            \           ['gitbranch', 'readonly', 'filename', 'modified']],
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

" Update lighltine on background change
augroup LIGHTLINE
    autocmd!
    autocmd OptionSet background
                \ execute 'source' globpath(&rtp, 'autoload/lightline/colorscheme/'.g:colors_name.'.vim')
                \ | call lightline#colorscheme() | call lightline#update()
augroup END

let g:lightline#ale#indicator_checking = "\uf110 "
let g:lightline#ale#indicator_warnings = "\uf071 "
let g:lightline#ale#indicator_errors = "\uf05e "
let g:lightline#ale#indicator_ok = "\uf00c "
" >>>
" rainbow_parentheses.vim <<<
augroup RAINBOW
    autocmd!
    autocmd VimEnter * RainbowParenthesesToggle
    autocmd Syntax * RainbowParenthesesLoadRound
    autocmd Syntax * RainbowParenthesesLoadSquare
    autocmd Syntax * RainbowParenthesesLoadBraces
augroup END
let g:rbpt_max = 16
" >>>
" pear-tree <<<
let g:pear_tree_repeatable_expand = 0
let g:pear_tree_smart_openers = 1
let g:pear_tree_smart_closers = 1
let g:pear_tree_smart_backspace = 1
" Default rules for matching:
let g:pear_tree_pairs = {
            \ '(': {'closer': ')'}, '[': {'closer': ']'}, '{': {'closer': '}'},
            \ "'": {'closer': "'"},  '"': {'closer': '"'},  '`': {'closer': '`'},
            \ "'''": {'closer': "'''"},  '"""': {'closer': '"""'},  '```': {'closer': '```'},
            \ }

augroup pear_pairs
    autocmd!
    autocmd FileType markdown let b:pear_tree_pairs = extend(deepcopy(g:pear_tree_pairs), {
            \ "$": {'closer': "$"}, "$$": {'closer': "$$"}
            \ }, 'keep')
    autocmd FileType tex let b:pear_tree_pairs = extend(deepcopy(g:pear_tree_pairs), {
            \ "$": {'closer': "$"}, "$$": {'closer': "$$"}
            \ }, 'keep')
augroup END
" >>>
" targets <<<
let g:targets_nl = 'nN'
" >>>
" vimtex <<<
set conceallevel=1
let g:tex_conceal = 'abdmg'
let g:vimtex_view_method = 'zathura'
let g:vimtex_compiler_latexmk = {
            \ 'build_dir' : './tex_output',
            \ 'options' : [
            \   '-verbose',
            \   '-file-line-error',
            \   '-shell-escape',
            \   '-synctex=1',
            \   '-interaction=nonstopmode',
            \ ],
            \}

" Needs https://github.com/marhop/pandoc-unicode-math , check org notes for more info
" vnoremap <silent> <leader>lu <ESC>:set nohlsearch<CR>:set textwidth=1000<CR>`>a#<ESC>`<i#<ESC> <bar>
"             \ :s/#\(.*\n*.*\)#/\=trim(system("latex_to_unicode '".trim(submatch(1))."'"))
"             \ <CR> `<
"             \ :let @/ = "" <bar> set hlsearch<CR>:set textwidth=80<CR>
vnoremap <silent> <leader>lu <ESC>:set nohlsearch<CR>:set textwidth=1000<CR>`>a#<ESC>`<i#<ESC> <bar>
            \ :s/#\(\_[^#]*\)#/\=trim(system("latex_to_unicode '".trim(submatch(1))."'"))
            \ <CR> `<
            \ :let @/ = "" <bar> set hlsearch<CR>:set textwidth=80<CR>
" Surround capital characters with $
vnoremap <silent> <leader>l$ <ESC>:set nohlsearch<CR>gv :substitute:\(\u\)\(\s\\|\.\\|,\\|(\):$\1$\2:gc <bar>
            \ :let @/ = "" <bar> set hlsearch<CR>
" >>>
" vim-markdown <<<
let g:vim_markdown_math = 1
let g:vim_markdown_folding_disabled = 1
" >>>
" vim-table-mode <<<
let g:table_mode_corner='|'
" >>>
" ale <<<
let g:ale_virtualtext_cursor = 1
let g:ale_fix_on_save = 1
let g:ale_linters = {
            \   'python': ['flake8'],
            \   'sh': ['langauge_server'],
            \   'js': ['eslint'],
            \   'php': ['langserver'],
            \}
let g:ale_fixers = {
            \   '*': ['remove_trailing_lines', 'trim_whitespace'],
            \   'python': ['autopep8'],
            \   'SQL': ['sqlint'],
            \   'js': ['eslint'],
            \}

let g:ale_fix_on_save_ignore = {'markdown': ['trim_whitespace'], 'tex': ['trim_whitespace']}

" Auto remove ALE window on buffer exit
augroup CloseLoclistWindowGroup
    autocmd!
    autocmd QuitPre * if empty(&buftype) | lclose | endif
augroup END
" >>>
" ulisnippets, coc-sippets <<<
imap <C-j> <Plug>(coc-snippets-expand-jump)
vmap <C-j> <Plug>(coc-snippets-select)

let g:coc_snippet_prev = '<c-k>'
let g:UltiSnipsEditSplit = "vertical"

imap <C-j> <Plug>(coc-snippets-expand-jump)
inoremap <silent><expr> <c-j> pumvisible() ? coc#_select_confirm() :
            \"\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"
" >>>
" vim-mundo <<<
" Enable persistent undo so that undo history persists across vim sessions
set undofile
nnoremap yeu <cmd>MundoToggle<cr>
" >>>
" markdown-preview.nvim <<<
let g:mkdp_command_for_global = 1
let g:mkdp_page_title = '${name}'
let g:mkdp_auto_close = 0
nmap yem <Plug>MarkdownPreviewToggle

" open page in new window
function! OpenNewBrowserWindow(url)
    execute "silent ! qutebrowser --target window " . a:url
endfunction

let g:mkdp_browserfunc = 'OpenNewBrowserWindow'
" >>>
" fzf <<<
nnoremap <leader>a :Ag<space>
nnoremap <leader>p :Files<CR>
nnoremap <leader>o :Buffers<CR>

" Mapping selecting mappings
imap <C-q> <plug>(fzf-maps-i)
nmap <leader><tab> <plug>(fzf-maps-n)
xmap <leader><tab> <plug>(fzf-maps-x)
omap <leader><tab> <plug>(fzf-maps-o)

autocmd! FileType fzf set laststatus=0 noshowmode noruler
            \| autocmd BufLeave <buffer> set laststatus=2 showmode ruler
" >>>
" vista <<<
nnoremap yeo :Vista!!<CR>
" >>>
" coc <<<
set hidden
set updatetime=300
set shortmess+=c

autocmd CursorHold * silent call CocActionAsync('highlight')

xmap if <Plug>(coc-funcobj-i)
xmap af <Plug>(coc-funcobj-a)
omap if <Plug>(coc-funcobj-i)
omap af <Plug>(coc-funcobj-a)
xmap ic <Plug>(coc-classobj-i)
omap ic <Plug>(coc-classobj-i)
xmap ac <Plug>(coc-classobj-a)
omap ac <Plug>(coc-classobj-a)

" Mappings using CoCList:
nnoremap <silent> <space>a  <cmd>CocList diagnostics<cr>
nnoremap <silent> <space>c  <cmd>CocList commands<cr>
nnoremap <silent> <space>e  <cmd>CocList extensions<cr>

" Code navigation.
nmap <silent>gd <Plug>(coc-definition)
nmap <silent>gr <Plug>(coc-references)
nmap <silent>[g <Plug>(ale_previous_wrap)
nmap <silent>]g <Plug>(ale_next_wrap)

nnoremap <silent> yex :CocCommand explorer --sources=file+<CR>
nnoremap <leader>rn <Plug>(coc-rename)

function! s:show_documentation()
    if (index(['vim','help'], &filetype) >= 0)
        execute 'h '.expand('<cword>')
    else
        call CocAction('doHover')
    endif
endfunction
nnoremap <silent> K :call <SID>show_documentation()<CR>
" >>>
