# History
# Ongoing session
export HISTSIZE=1000000
# Hisotry file
export HISTFILESIZE=1000000
# Ignore duplicate commands
export HISTCONTROL="erasedups:ignoreboth"
# Ignore common commands
export HISTIGNORE="&:[ ]*:exit:ls:bg:fg:history:clear"
# Bash stores history when the session terminates
# Make it so that it when the command is executed
export PROMPT_COMMAND='history -a'
# Include timestamp
HISTTIMEFORMAT='%F %T '

# Common Environment variables and starts X11
[[ -f $HOME/.profile ]] && . $HOME/.profile

# Needed, else the login shell won't source it
[[ -f $HOME/.bashrc ]] && . $HOME/.bashrc
