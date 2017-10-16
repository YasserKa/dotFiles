" First things come first {{{
" must be first instructions
set nocompatible " vim instead of Vi
" loading pathogen
execute pathogen#infect('~/.vim/bundle/AutoComplete/{}')
" }}}
" general settings{{{
syntax on

" insert mode
autocmd CursorMovedI * if pumvisible() == 0|pclose|endif
autocmd InsertLeave * if pumvisible() == 0|pclose|endif
"let g:UltiSnipsSnippetsDir='~/.vim/bundle/AutoComplete/vim-snippets/UltiSnips'
"let g:UltiSnipsSnippetDirectories=["UtilsSnips"]
" Trigger configuration. Do not use <tab> if you use https://github.com/Valloric/YouCompleteMe.
" dasfas"
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsListSnippet="<c-q>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<s-tab>"

" If you want :UltiSnipsEdit to split your window.
let g:UltiSnipsEditSplit="vertical"

" used to fold sections by markers and fold them by default
" vim:foldmethod=marker:foldlevel=0
