" general settings {{{
set clipboard+=unnamedplus    " System clipboard
set virtualedit=all           " Moving in whitespace
set ignorecase                " Do case insensitive search...
set smartcase                 " ...unless capital letters are used
set confirm                   " Confirm :q in case of unsaved changes
set autowrite                 " Save file when switching buffers

set background=light
set termguicolors
set dictionary=/usr/share/dict/cracklib-small
set scrolloff=5               " Show 5 lines above/below the cursor
set cursorline                " Highlight current line"
set number                    " Show the number line
set relativenumber
set list                      " Show eof, trailing, etc..
set listchars=eol:¶,tab:>-,trail:.,nbsp:_,extends:+,precedes:+
set foldmethod=marker         " Folds at start
set foldmarker={{{,}}}        " Folds format
set laststatus=3              " Use Global statusline

" Tabs, spaces, indentation, wrapping
set expandtab               " Use spaces for tabs
set shiftwidth=4            " Number of spaces to autoindent
set softtabstop=4           " Number of spaces for a tab
set nojoinspaces            " No extra space after '.' when joining lines
set textwidth=80            " Wrap lines automatically at 80th column

let g:python3_host_prog  = '/bin/python3.10'

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
inoremap <C-l> <c-g>u<Esc>[s1z=`]a<c-g>u     " Fix last misspell

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

" Skeletons
autocmd BufNewFile  *.sh    0r ~/.config/nvim/skeletons/skeleton.sh
autocmd BufNewFile  *.bash    0r ~/.config/nvim/skeletons/skeleton.sh
autocmd BufNewFile  *.py    0r ~/.config/nvim/skeletons/skeleton.py

" Highlight todo comments
augroup vimrc_todo
    au!
    au Syntax * syn match MyTodo /\v<(FIXME|NOTE|TODO|OPTIMIZE|REFACTOR):/
          \ containedin=.*Comment,vimCommentTitle
augroup END
hi def link MyTodo Todo

function! YankOrgLink()
    let @+ = "[[".expand("%:p")."][".expand("%:t")."]]"
endfunction
command YankOrgLink :call YankOrgLink()

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

" personalized vimrc
if getcwd() =~# '/practical-sql'
  set secure exrc
endif

" Execute macro only selected lines
xnoremap @ :<C-u>call ExecuteMacroOverVisualRange()<CR>

function! ExecuteMacroOverVisualRange()
  echo "@".getcmdline()
  execute ":'<,'>normal @".nr2char(getchar())
endfunction

augroup DOSINI_COLOR
    autocmd!
    autocmd VimEnter * hi! link dosiniValue GruvBoxPurple
augroup END

" AutoCommands
augroup SESSIONS
    autocmd!
    if(argc() == 0)
        autocmd VimEnter * nested call LoadSession()
    endif
    autocmd VimLeave * call MakeSession()
augroup END

augroup MAIL
    autocmd!
    autocmd BufRead,BufNewFile neomutt-* set spell tw=100
    autocmd BufRead neomutt-* normal 50%

    autocmd FileType mail nmap <leader>f <Plug>AddVimFootnote
    autocmd FileType mail imap <C-,>f <Plug>AddVimFootnote

    autocmd FileType mail nmap <leader>r <Plug>ReturnFromFootnote
    autocmd FileType mail imap <C-,>r <Plug>ReturnFromFootnote
augroup END

augroup VIMENTER
    autocmd!
    autocmd CmdwinEnter * map <buffer> <C-j> <CR>
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

augroup TMP_FILES
    autocmd!
    autocmd BufRead,BufNewFile tmp.* inoremap <C-c><C-c> <esc>:q<cr>
    autocmd BufRead,BufNewFile tmp.* set noswapfile
    autocmd ExitPre tmp.* :w
augroup END


" }}}
" lightline.vim {{{
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
            \ 'gitbranch': 'FugititiveHead()',
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
" }}}
" vim-cool {{{
" Show number of matches in command-line
let g:CoolTotalMatches = 1
" }}}
" targets {{{
let g:targets_nl = ["n", "N"]
" }}}
" {{{ expand
map + <Plug>(expand_region_expand)
map _ <Plug>(expand_region_shrink)
" }}}
" {{{ vim-commentary
xnoremap gcc  :Commentary<CR>
" }}}
" {{{ vim-doge
let g:doge_mapping_comment_jump_forward = '<C-j>'
let g:doge_mapping_comment_jump_backward = '<C-k>'
" }}}
" vimtex {{{
set conceallevel=1
let g:tex_conceal = 'abdmg'
let g:vimtex_view_method = 'zathura'
let g:vimtex_fold_enabled = 1
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

vnoremap <silent> <leader>lu <ESC>:set nohlsearch<CR>:set textwidth=1000<CR>`>a#<ESC>`<i#<ESC> <bar>
            \ :s/#\(\_[^#]*\)#/\=trim(system("latex_to_unicode '".trim(submatch(1))."'"))
            \ <CR> `<
            \ :let @/ = "" <bar> set hlsearch<CR>:set textwidth=80<CR>
" Surround capital characters with $
vnoremap <silent> <leader>l$ <ESC>:set nohlsearch<CR>gv :substitute:\(\u\)\(\s\\|\.\\|,\\|(\):$\1$\2:gc <bar>
            \ :let @/ = "" <bar> set hlsearch<CR>
" }}}
" {{{ Jupyter Ascending
function! OpenJupyterNotebook()
    let notebook_path = "http://localhost:8888/notebooks/".expand('%')[:-3]."ipynb"
    execute "silent ! qutebrowser --target window " . notebook_path . " &"
endfunction
augroup NOTEBOOK
    autocmd!
    autocmd BufRead,BufNewFile *.sync.py nmap  <leader>e <Plug>JupyterExecute
    autocmd BufRead,BufNewFile *.sync.py nmap  <leader>E <Plug>JupyterExecuteAll

    autocmd BufRead,BufNewFile *.sync.py nnoremap <leader>O :call OpenJupyterNotebook()<CR>
augroup END
" }}}
" {{{ vim-ipython-cell / vim-slime
" Slime
" always use tmux
let g:slime_target = 'tmux'

" https://github.com/jpalardy/vim-slime/tree/main/ftplugin/python
let g:slime_bracketed_ipython = 1

" always send text to the top-right pane in the current tmux tab without asking
let g:slime_default_config = {
            \ 'socket_name': get(split($TMUX, ','), 0),
            \ 'target_pane': '{top-right}' }

let g:slime_dont_ask_default = 1

" }}}
" vim-markdown {{{
let g:vim_markdown_math = 1
let g:vim_markdown_folding_disabled = 1
" }}}
" vim-table-mode {{{
let g:table_mode_corner='|'
" }}}
" ale {{{
let g:ale_virtualtext_cursor = 1
let g:ale_linters = {
            \   'python': ['flake8'],
            \   'sh': ['language_server', 'shellcheck'],
            \   'js': ['eslint'],
            \   'php': ['langserver'],
            \   'markdown': ['write-good'],
            \}
let g:ale_fixers = {
            \   '*': ['remove_trailing_lines', 'trim_whitespace'],
            \   'sh': ['shfmt'],
            \   'python': ['black', 'autoimport', 'isort'],
            \   'SQL': ['sqlint'],
            \   'js': ['eslint'],
            \}

" misbehaving linters
let g:ale_linters_ignore = {
            \ 'scala': ['fsc', 'scalac']
            \}

let g:ale_fix_on_save_ignore = {
            \  'markdown': ['trim_whitespace'],
            \ 'tex': ['trim_whitespace']
            \}
" Auto remove ALE window on buffer exit
augroup CloseLoclistWindowGroup
    autocmd!
    autocmd QuitPre * if empty(&buftype) | lclose | endif
augroup END
" }}}
" ulisnippets, coc-sippets {{{
imap <C-j> <Plug>(coc-snippets-expand-jump)
vmap <C-j> <Plug>(coc-snippets-select)

let g:coc_snippet_prev = '<c-k>'
let g:UltiSnipsEditSplit = "vertical"
let g:ultisnips_python_style = 'google'

imap <C-j> <Plug>(coc-snippets-expand-jump)
inoremap <silent><expr> <c-j> pumvisible() ? coc#_select_confirm() :
            \"\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"
" }}}
" vim-mundo {{{
" Enable persistent undo so that undo history persists across vim sessions
set undofile
nnoremap yeu <cmd>MundoToggle<cr>
" }}}
" markdown-preview.nvim {{{
let g:mkdp_command_for_global = 1
let g:mkdp_page_title = '${name}'
let g:mkdp_auto_close = 0
nmap yem <Plug>MarkdownPreviewToggle

" open page in new window
function! OpenNewBrowserWindow(url)
    execute "silent ! qutebrowser --target window " . a:url
endfunction

let g:mkdp_browserfunc = 'OpenNewBrowserWindow'
" }}}
" vimspector {{{

fun! GotoWindow(id)
    call win_gotoid(a:id)
    MaximizerToggle
endfun

" Debugger remaps
nnoremap <C-w>m :MaximizerToggle!<CR>
nnoremap <leader>dd :call vimspector#Launch()<CR>
nnoremap <leader>dc :call GotoWindow(g:vimspector_session_windows.code)<CR>
nnoremap <leader>dt :call GotoWindow(g:vimspector_session_windows.tagpage)<CR>
nnoremap <leader>dv :call GotoWindow(g:vimspector_session_windows.variables)<CR>
nnoremap <leader>dw :call GotoWindow(g:vimspector_session_windows.watches)<CR>
nnoremap <leader>ds :call GotoWindow(g:vimspector_session_windows.stack_trace)<CR>
nnoremap <leader>do :call GotoWindow(g:vimspector_session_windows.output)<CR>
nnoremap <leader>de :call vimspector#Reset()<CR>

nnoremap <leader>dtcb :call vimspector#CleanLineBreakpoint()<CR>

nmap <leader>dl <Plug>VimspectorStepInto
nmap <leader>dj <Plug>VimspectorStepOver
nmap <leader>dk <Plug>VimspectorStepOut
nmap <leader>d_ <Plug>VimspectorRestart
nnoremap <leader>d<space> :call vimspector#Continue()<CR>

nmap <leader>drc <Plug>VimspectorRunToCursor
nmap <leader>dbp <Plug>VimspectorToggleBreakpoint
nmap <leader>dcbp <Plug>VimspectorToggleConditionalBreakpoint

" }}}
" fzf {{{
" Mapping selecting mappings
imap <C-q> <plug>(fzf-maps-i)
nmap <leader><tab> <plug>(fzf-maps-n)
xmap <leader><tab> <plug>(fzf-maps-x)
omap <leader><tab> <plug>(fzf-maps-o)

autocmd! FileType fzf set laststatus=0 noshowmode noruler
            \| autocmd BufLeave <buffer> set laststatus=2 showmode ruler


command! -bang -nargs=* Rg
  \ call fzf#vim#grep(
  \   'rg --column --line-number --no-heading --color=always --smart-case -- '.shellescape(<q-args>), 1,
  \   fzf#vim#with_preview(), <bang>0)

" override theme, since gruvbox-dark doesn't show some text
command! -bang -nargs=? -complete=dir Files
            \ call fzf#vim#files(<q-args>, {'options': ['--layout=reverse', '--info=inline', '--preview', 'bat --color=always --style=numbers --theme gruvbox-light {}']}, <bang>0)
" }}}
" vista {{{
nnoremap yeo :Vista!!<CR>
" }}}
" coc {{{
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
nmap <silent>[g <Plug>(ale_previous_wrap)
nmap <silent>]g <Plug>(ale_next_wrap)

nnoremap <silent> yex :CocCommand explorer --sources=file+<CR>
nnoremap <leader>rn <Plug>(coc-rename)

function! s:show_documentation()
    if (index(['vim','help'], &filetype) >= 0)
        execute 'h '.expand('<cword>')
    elseif &filetype ==# 'tex'
        VimtexDocPackage
    else
        call CocAction('doHover')
    endif
endfunction
nnoremap <silent> K :call <SID>show_documentation()<CR>
" }}}
" {{{ lua
lua << EOF
require('init')
EOF
" }}}
