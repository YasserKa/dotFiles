# Check for interactive
[ -z "$PS1" ] && return

# Bash options
set -o vi                  # Vi mode
set -o noclobber           # Don't overwrite when redireting using shell
shopt -s autocd            # Auto cd
shopt -s histappend        # Append to the history file, don't overwrite it
shopt -s checkwinsize      # Update the value of LINES and COLUMNS after each command if altered
stty -ixon                 # Enable search C-s for history searching

PS1='[\[\e[36m\]\w\[\e[0m\]]\e[36m\]\n\[\e[33m\]=> \[\e[0m\]'

# Autojumping
fasd_cache="$HOME/.fasd-init-bash"
if [ "$(command -v fasd)" -nt "$fasd_cache" -o ! -s "$fasd_cache" ]; then
    fasd --init posix-alias bash-hook bash-ccomp bash-ccomp-install >| "$fasd_cache"
fi
source "$fasd_cache"
unset fasd_cache

# Source files
[[ -f $HOME/.bash_aliases ]] && source $HOME/.bash_aliases
[[ -f $HOME/.bash_functions ]] && source $HOME/.bash_functions

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home/yasser/miniconda3/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/home/yasser/miniconda3/etc/profile.d/conda.sh" ]; then
        . "/home/yasser/miniconda3/etc/profile.d/conda.sh"
    else
        export PATH="/home/yasser/miniconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<
