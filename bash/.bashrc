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
source /usr/share/doc/pkgfile/command-not-found.bash

git_branch() {
    git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'
}

PS1='[\[\e[36m\]\w\[\e[0m\]]\e[36m\]\[\033[38;5;87m\]$(git_branch)\n\[\e[33m\]=> \[\e[0m\]'

# setup autojumping
fasd_cache="$HOME/.fasd-init-bash"
if [ "$(command -v fasd)" -nt "$fasd_cache" -o ! -s "$fasd_cache" ]; then
    fasd --init posix-alias bash-hook bash-ccomp bash-ccomp-install >| "$fasd_cache"
fi
source "$fasd_cache"
unset fasd_cache

if [[ -r ~/bin/mcfly.bash ]]; then
    source /usr/share/doc/mcfly/mcfly.bash
fi

# colorizing commands
GRC_ALIASES=true
[[ -s "/etc/profile.d/grc.sh" ]] && source /etc/profile.d/grc.sh

# Source files
[[ -f $HOME/.bash_aliases ]] && source $HOME/.bash_aliases
[[ -f $HOME/.bash_functions ]] && source $HOME/.bash_functions

# Better C-r search
# Note: Need to be after sourcing bash alias, otherwise rofi won't work on
# aliases. Reason: unknown
