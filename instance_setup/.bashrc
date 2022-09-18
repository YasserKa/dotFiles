# Source if the shell is interactive
# shellcheck disable=SC1091,SC2034,SC2148,SC1090,SC2155
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

tmp_XDG_CONFIG_HOME="$XDG_CONFIG_HOME"
XDG_CONFIG_HOME="$HOME/.yasser_rc"
export MYVIMRC="$XDG_CONFIG_HOME/.vimrc"
# shellcheck disable=SC2016
export VIMINIT='source $MYVIMRC'

# Use Alt-h to view documentation for commands
run_help() {
    cmd="$READLINE_LINE"
    help "$cmd" 2>/dev/null || man "$cmd" 2>/dev/null || "$cmd" --help | $PAGER
}
bind -m vi-insert -x '"\eh": run_help'

# Prompt with info about git repo
if [[ -e $XDG_CONFIG_HOME/git/git-prompt.sh ]]; then
    # https://www.git-scm.com/book/en/v2/Appendix-A%3A-Git-in-Other-Environments-Git-in-Bash
    . "$XDG_CONFIG_HOME/git/git-prompt.sh"
    export GIT_PS1_SHOWDIRTYSTATE=1
    PS1='\[$(tput bold)\]\[\e[36m\]\w\[\033[38;5;87m\] $(__git_ps1 "(%s)")\n\[\033[38;5;34m\]\u@\h: \[\e[m\]'
 PS1='\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    git_branch() { git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ \1)/'; }
    PS1='\[$(tput bold)\]\[\e[36m\]\w\[\033[38;5;87m\] $(git_branch)\n\[\033[01;32m\]\u@\h\[\033[00m\]\[\033[01;34m\] \[\033[38;5;34m\]❯ \[\e[m\]'
fi

# Readline with autopairs
[[ -f "$XDG_CONFIG_HOME/readline/autopairs" ]] && . "$XDG_CONFIG_HOME/readline/autopairs"
[[ -f "$XDG_CONFIG_HOME/readline/inputrc" ]] && bind -f "$XDG_CONFIG_HOME/readline/inputrc"

# History
# Ongoing session
export HISTSIZE=1000
# Hisotry file
export HISTFILESIZE=2000
# Ignore duplicate commands
export HISTCONTROL="erasedups:ignoreboth"
# Ignore common commands
export HISTIGNORE="&:[ ]*:exit:ls:bg:fg:history:clear"
# Bash stores history when the session terminates
# Make it so that it when the command is executed
export PROMPT_COMMAND='history -a'

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    alias ls='ls --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

export EDITOR=vim
alias v='vim'
alias vi='vim'
alias h='history'
alias hg='history | grep'

# More options
alias mkdir='mkdir -pv'
alias df='df -Tha --total'

# Prompt before overriding
alias mv='mv -i'
alias cp='cp -i --preserve=all --reflink=auto'

# Navigation
alias ..='cd ..'
alias ...='cd ../..'

# Alert after long commands
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Which is unreliable, use type -P instead
# https://unix.stackexchange.com/questions/85249/why-not-use-which-what-to-use-then
which() {
    printf >&2 'The which command is unreliable. Use type -P %s\n' "$*"
    return 2
}

XDG_CONFIG_HOME=$tmp_XDG_CONFIG_HOME
[[ "$tmp_XDG_CONFIG_HOME" != "" ]] && unset "$tmp_XDG_CONFIG_HOME"


