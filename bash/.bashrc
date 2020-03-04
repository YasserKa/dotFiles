# Check for interactive
[ -z "$PS1" ] && return

# Bash options
set -o vi                  # Vi mode
set -o noclobber           # Don't overwrite when redireting using shell
shopt -s autocd            # Auto cd
shopt -s histappend        # Append to the history file, don't overwrite it
shopt -s checkwinsize      # Update the value of LINES and COLUMNS after each command if altered
stty -ixon                 # Enable search C-s for history searching

# Ignore this if rofi is calling bash (it slows rofi)
if [ -z `pgrep rofi` ]; then
    powerline-daemon -q
    POWERLINE_BASH_CONTINUATION=1
    POWERLINE_BASH_SELECT=1
    . /usr/share/powerline/bindings/bash/powerline.sh
fi

# Autojumping
fasd_cache="$HOME/.fasd-init-bash"
if [ "$(command -v fasd)" -nt "$fasd_cache" -o ! -s "$fasd_cache" ]; then
    fasd --init posix-alias bash-hook bash-ccomp bash-ccomp-install >| "$fasd_cache"
fi
source "$fasd_cache"
unset fasd_cache

# source files
[[ -f $HOME/.bash_aliases ]] && source $HOME/.bash_aliases
[[ -f $HOME/.bash_functions ]] && source $HOME/.bash_functions
