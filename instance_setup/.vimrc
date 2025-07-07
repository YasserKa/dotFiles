call plug#begin()

Plug 'ojroques/vim-oscyank', {'branch': 'main'}

call plug#end()

syntax on
set confirm  " Confirm :q in case of unsaved changes
set modeline " Don't disable modeline

if (!has('nvim') && !has('clipboard_working'))
    " In the event that the clipboard isn't working, it's quite likely that
    " the + and * registers will not be distinct from the unnamed register. In
    " this case, a:event.regname will always be '' (empty string). However, it
    " can be the case that `has('clipboard_working')` is false, yet `+` is
    " still distinct, so we want to check them all.
    let s:VimOSCYankPostRegisters = ['', '+', '*']
    " copy text to clipboard on both (y)ank and (d)elete
    let s:VimOSCYankOperators = ['y', 'd']
    function! s:VimOSCYankPostCallback(event)
        if index(s:VimOSCYankPostRegisters, a:event.regname) != -1
            \ && index(s:VimOSCYankOperators, a:event.operator) != -1
            call OSCYankRegister(a:event.regname)
        endif
    endfunction
    augroup VimOSCYankPost
        autocmd!
        autocmd TextYankPost * call s:VimOSCYankPostCallback(v:event)
    augroup END
endif

noremap ; :
noremap : ;

" Cursor block (normal) & line (insert)
let &t_SI = "\e[6 q"
let &t_EI = "\e[2 q"

nnoremap Y y$

" Fix indentation problem in tmux
" https://vi.stackexchange.com/questions/23110/pasting-text-on-vim-inside-tmux-breaks-indentation
if &term =~ "tmux"
    let &t_BE = "\e[?2004h"
    let &t_BD = "\e[?2004l"
    exec "set t_PS=\e[200~"
    exec "set t_PE=\e[201~"
endif

