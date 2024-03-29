call plug#begin('~/.vim/plugged')
Plug 'ojroques/vim-oscyank'
call plug#end()

noremap ; :
noremap : ;
set confirm                   " Confirm :q in case of unsaved changes

" Cursor block (normal) & line (insert)
let &t_SI = "\e[6 q"
let &t_EI = "\e[2 q"

autocmd TextYankPost *
    \ if v:event.operator is 'y' && v:event.regname is '' |
    \ execute 'OSCYankRegister' |
    \ endif

let g:oscyank_term = 'default'

let g:oscyank_silent = v:true  " or 1 for older versions of Vim

colorscheme delek

" Fix indentation problem in tmux
" https://vi.stackexchange.com/questions/23110/pasting-text-on-vim-inside-tmux-breaks-indentation
if &term =~ "tmux"
    let &t_BE = "\e[?2004h"
    let &t_BD = "\e[?2004l"
    exec "set t_PS=\e[200~"
    exec "set t_PE=\e[201~"
endif
