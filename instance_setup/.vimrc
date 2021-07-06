call plug#begin('~/.vim/plugged')
Plug 'ojroques/vim-oscyank'
call plug#end()

noremap ; :
noremap : ;

autocmd TextYankPost * if v:event.operator is 'y' && v:event.regname is '' | OSCYankReg " | endif
let g:oscyank_term = 'tmux'

let g:oscyank_silent = v:true  " or 1 for older versions of Vim
