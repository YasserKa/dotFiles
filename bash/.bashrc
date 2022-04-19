# Check for interactive
[ -z "$PS1" ] && return

# Bash options https://www.gnu.org/software/bash/manual/html_node/The-Shopt-Builtin.html
set -o vi                  # Vi mode
set -o noclobber           # Don't overwrite when redireting using shell
shopt -s autocd            # auto cd without using cd
shopt -s histappend        # Append to the history file, don't overwrite it
shopt -s cdspell           # fix  spelling errors when using cd
shopt -s extglob           # extend pattern matching (used for extract function)
shopt -s cmdhist           # history records commands with multiple lines as such instead of using ";"
stty -ixon                 # Enable search C-s for history searching

# complete command names and file names for super user
complete -cf sudo
. /usr/share/doc/pkgfile/command-not-found.bash
. /usr/share/bash-completion/bash_completion

git_branch() {
    git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ \1)/'
}

PS1='\[$(tput bold)\]\[\e[36m\]\w\[\033[38;5;87m\] $(git_branch)\n\[\033[38;5;34m\]❯ \[\e[m\]'

# setup autojumping
fasd_cache="$HOME/.fasd-init-bash"
if [ "$(command -v fasd)" -nt "$fasd_cache" -o ! -s "$fasd_cache" ]; then
    fasd --init posix-alias bash-hook bash-ccomp bash-ccomp-install >| "$fasd_cache"
fi
source "$fasd_cache"
unset fasd_cache

if [[ -r /usr/share/doc/mcfly/mcfly.bash ]]; then
    source /usr/share/doc/mcfly/mcfly.bash
fi

# Commands with colors
GRC_ALIASES=true
[[ -s "/etc/profile.d/grc.sh" ]] && source /etc/profile.d/grc.sh

# Files with colors
. /usr/share/LS_COLORS/dircolors.sh

# Source files
[[ -f $HOME/.bash_aliases ]] && . $HOME/.bash_aliases
[[ -f $HOME/.bash_functions ]] && . $HOME/.bash_functions
