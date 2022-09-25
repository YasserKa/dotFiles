# shellcheck shell=bash
# Source if the shell is interactive
# shellcheck disable=SC1091,SC2034,SC1090,SC2155
case "$-" in
    *i*) ;;
    *) return ;;
esac

# Bash options https://www.gnu.org/software/bash/manual/html_node/The-Shopt-Builtin.html
set -o vi                  # Vi mode
set -o noclobber           # Don't overwrite when redireting using shell
shopt -s autocd            # Auto cd without using cd
shopt -s histappend        # Append to the history file, don't overwrite it
shopt -s cdspell           # Fix spelling errors when using cd
shopt -s dirspell          # Fix spelling errors during tab-completion
shopt -s extglob           # extend pattern matching
shopt -s cmdhist           # History records commands with multiple lines as such instead of using ";"
stty -ixon                 # Enable search C-s for history searching (Enable XON/XOFF flow control)

# Use Alt-h to view documentation for commands
run_help() {
    local -r cmd="$READLINE_LINE"
    help "$cmd" 2>/dev/null || man "$cmd" 2>/dev/null || $cmd --help | $PAGER
}
bind -m vi-insert -x '"\eh": run_help'

# Complete command names and file names for super user
complete -cf sudo
[[ -f /usr/share/doc/pkgfile/command-not-found.bash ]] && . /usr/share/doc/pkgfile/command-not-found.bash
# Gives autocompletion for options
# https://github.com/scop/bash-completion
[[ -f /usr/share/bash-completion/bash_completion ]] && . /usr/share/bash-completion/bash_completion

[[ -f "$XDG_CONFIG_HOME/tmux/tmux_completion" ]] && . "$XDG_CONFIG_HOME/tmux/tmux_completion"

# Prompt with info about git repo
if [[ -e $XDG_CONFIG_HOME/git/git-prompt.sh ]]; then
    # https://www.git-scm.com/book/en/v2/Appendix-A%3A-Git-in-Other-Environments-Git-in-Bash
    . "$XDG_CONFIG_HOME/git/git-prompt.sh"
    export GIT_PS1_SHOWDIRTYSTATE=1
    PS1='\[$(tput bold)\]\[\e[36m\]\w\[\033[38;5;87m\] $(__git_ps1 "(%s)")\n\[\033[38;5;34m\]❯ \[\e[m\]'
else
    git_branch() { git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ \1)/'; }
    PS1='\[$(tput bold)\]\[\e[36m\]\w\[\033[38;5;87m\] $(git_branch)\n\[\033[38;5;34m\]❯ \[\e[m\]'
fi

# Setup autojumping
# https://github.com/clvv/fasd
if command -v fasd > /dev/null; then
    fasd_cache="$HOME/.fasd-init-bash"
    # Cache the code for faster sourcing time
    if [[ ! -s "$fasd_cache" ]]; then
        fasd --init posix-alias bash-hook bash-ccomp bash-ccomp-install >| "$fasd_cache"
    fi
    . "$fasd_cache"
    unset -v fasd_cache
    alias vf='fasd -sife nvim' # quick opening files with vim

    _fasd_bash_hook_cmd_complete vf j
fi

# Commands with colors
# https://github.com/garabik/grc
if command -v grc > /dev/null; then
    GRC_ALIASES=true
    [[ -x /etc/profile.d/grc.sh ]] && . /etc/profile.d/grc.sh
fi

if command -v fzf > /dev/null; then
    [[ -f /usr/share/fzf/completion.bash ]] && . /usr/share/fzf/completion.bash
    [[ -f /usr/share/fzf/key-bindings.bash ]] && . /usr/share/fzf/key-bindings.bash

    # For C-r history search, make <tab> edit selected option and <enter> execute it
    FZF_CTRL_R_EDIT_KEY=ctrl-e
    FZF_CTRL_R_EXEC_KEY=enter
    [[ -f $XDG_CONFIG_HOME/fzf/history-exec.bash ]] && . "$XDG_CONFIG_HOME/fzf/history-exec.bash"

    # Taken from FZF's key-bindings.bash to make C-t works
    # history-exec.bash seems to make the binding not work
    bind -m vi-insert -x '"\C-t": fzf-file-widget'
fi

# Readline with autopairs
[[ -f "$XDG_CONFIG_HOME/readline/autopairs" ]] && . "$XDG_CONFIG_HOME/readline/autopairs"

# Source files
for bash in "$HOME"/.bashrc.d/*.bash ; do
    . "$bash"
done
unset -v bash

# Files with colors
# https://github.com/trapd00r/LS_COLORS
[[ -f /usr/share/LS_COLORS/dircolors.sh ]] && . /usr/share/LS_COLORS/dircolors.sh

# Make sure that pinentry uses the correct TTY
export GPG_TTY=$(tty)
