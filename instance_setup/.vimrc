syntax on
set confirm  " Confirm :q in case of unsaved changes
set modeline " Don't disable modeline

noremap ; :
noremap : ;

" Cursor block (normal) & line (insert)
let &t_SI = "\e[6 q"
let &t_EI = "\e[2 q"

augroup Osc52Yank
    autocmd!
    autocmd TextYankPost * if len(v:event.regcontents[0]) > 1 || len(v:event.regcontents) > 1 | call system("printf $'\\e]52;c;%s\\a' \"$(cat | base64)\" >> /dev/tty", v:event.regcontents) | endif
augroup END

" Fix indentation problem in tmux
" https://vi.stackexchange.com/questions/23110/pasting-text-on-vim-inside-tmux-breaks-indentation
if &term =~ "tmux"
    let &t_BE = "\e[?2004h"
    let &t_BD = "\e[?2004l"
    exec "set t_PS=\e[200~"
    exec "set t_PE=\e[201~"
endif
