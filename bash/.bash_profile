# History
# ongoing session
export HISTSIZE=1000000
# hisotry file
export HISTFILESIZE=1000000
# ignore duplicate commands
export HISTCONTROL=ignoredups
export HISTIGNORE='ls:bg:fg:history'
# history format in .bash_history
# append commands to history as it's issues, since bash records to .bash_history
# when session ends (good if terminal crashes)
export PROMPT_COMMAND='history -a'

# change directory color to white
# LS_COLORS=$LS_COLORS:'di=1;37:' ; export LS_COLORS

[[ -f $HOME/.bashrc ]] && . $HOME/.bashrc
# Common Environment variables and starts X
[[ -f $HOME/.profile ]] && . $HOME/.profile
