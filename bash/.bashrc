# check for interactive
[ -z "$PS1" ] && return

# bash options
set -o vi                  # Vi mode
set -o noclobber           # Don't overwrite when redireting using shell
shopt -s autocd            # auto cd
shopt -s histappend        # append to the history file, don't overwrite it
shopt -s checkwinsize      # update the value of LINES and COLUMNS after each command if altered
stty -ixon                 # enable search C-s for history searching


# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# ignore this if rofi is calling bash(it slows rofi)
if [ -z `pgrep rofi` ]; then
    powerline-daemon -q
    POWERLINE_BASH_CONTINUATION=1
    POWERLINE_BASH_SELECT=1
    . /usr/share/powerline/bindings/bash/powerline.sh
fi

# source files
[[ -f $HOME/.bash_aliases ]] && source $HOME/.bash_aliases
[[ -f $HOME/.bash_functions ]] && source $HOME/.bash_functions
[[ -f /etc/profile.d/autojump.bash ]] && source /etc/profile.d/autojump.bash
[[ -f /usr/share/git/completion/git-completion.bash ]] && source /usr/share/git/completion/git-completion.bash
