# shellcheck shell=bash

# History
export HISTFILE=$HOME/.bash_history
# Bash stores history when the session terminates
# Make it so that it when the command is executed
export PROMPT_COMMAND='history -a'
# Include timestamp
export HISTTIMEFORMAT='%F %T '
# Ignore duplicate commands
export HISTCONTROL="erasedups:ignoreboth"
# Hisotry file
export HISTFILESIZE=$HISTSIZE

# Common Environment variables and starts X11
[[ -f $HOME/.profile ]] && . "$HOME/.profile"

# Needed, else the login shell won't source it
[[ -f $HOME/.bashrc ]] && . "$HOME/.bashrc"
