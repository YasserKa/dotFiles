# Make sure the shell is interactive
case $- in
    *i*) ;;
    *) return ;;
esac

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
[[ -x /usr/share/doc/pkgfile/command-not-found.bash ]] && /usr/share/doc/pkgfile/command-not-found.bash
[[ -x /usr/share/bash-completion/bash_completion ]] && /usr/share/bash-completion/bash_completion

git_branch() {
    git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ \1)/'
}

PS1='\[$(tput bold)\]\[\e[36m\]\w\[\033[38;5;87m\] $(git_branch)\n\[\033[38;5;34m\]❯ \[\e[m\]'

[ -f /usr/share/fzf/completion.bash ] && source /usr/share/fzf/completion.bash
[ -f /usr/share/fzf/key-bindings.bash ] && source /usr/share/fzf/key-bindings.bash

# setup autojumping
fasd_cache="$HOME/.fasd-init-bash"
if [ "$(command -v fasd)" -nt "$fasd_cache" -o ! -s "$fasd_cache" ]; then
    fasd --init posix-alias bash-hook bash-ccomp bash-ccomp-install >| "$fasd_cache"
fi

command -v fasd > /dev/null && . "$fasd_cache"
unset fasd_cache

if [[ -r /usr/share/doc/mcfly/mcfly.bash ]]; then
    source /usr/share/doc/mcfly/mcfly.bash
fi

# Commands with colors
GRC_ALIASES=true
[[ -x /etc/profile.d/grc.sh ]] && . /etc/profile.d/grc.sh

# Source files
for bash in "$HOME"/.bashrc.d/*.bash ; do
    . "$bash"
done
unset -v bash

[[ -e $HOME/.shell_common ]] && . $HOME/.shell_common
