# source .bashrc for tmux
export PATH=$PATH:$HOME/bin

# to make cmus escape works
export ESCDELAY=25
# used by bitwarden cli
export BW_SESSION=$(pass show bw_session)

# used by polybar config
# network interfaces
export IFACE_WLAN=$(ls /sys/class/net | grep -E '^(wlan|wlp)' | tail -n1 | cut -d ' ' -f1)
export IFACE_ETH=$(ls /sys/class/net | grep -E '^(eth|enp)' | tail -n1 | cut -d ' ' -f1)
# battery & adapter
export BATTERY=$(ls /sys/class/power_supply | grep -E '^BAT' | tail -n1 | cut -d ' ' -f1)
export ADAPTER=$(ls /sys/class/power_supply | grep -E '^ADP' | tail -n1 | cut -d ' ' -f1)

# history
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

# fzf
# use .ignore
export FZF_DEFAULT_COMMAND='ag --skip-vcs-ignores --hidden --ignore .git -g ""'
export FZF_DEFAULT_OPTS="--bind=ctrl-j:accept --color=light --preview='bat --color=always --style=numbers --theme gruvbox-dark {}'"

# configurations in one place
[ "$XDG_CONFIG_HOME" ] || export XDG_CONFIG_HOME="$HOME/.config"
[ "$XDG_DATA_HOME" ] || export XDG_DATA_HOME="$HOME/.local/share"
[ "$XDG_CACHE_HOME" ] || export XDG_CACHE_HOME="$HOME/.cache"

export GTK2_RC_FILES="$XDG_CONFIG_HOME/gtk-2.0/gtkrc-2.0"
export VIMPAGER_RC="$XDG_CONFIG_HOME/vimpagerrc"
export INPUTRC="$XDG_CONFIG_HOME/inputrc"

# default
export EDITOR=nvim
export VISUAL=nvim
export MYVIMRC="$XDG_CONFIG_HOME/nvim/init.vim"
export BROWSER=qutebrowser
export TERMINAL=alacritty
export DIFFPROG=vimdiff
export PAGER=vimpager

# change directory color to white
LS_COLORS=$LS_COLORS:'di=1;37:' ; export LS_COLORS

# Needs to be at the end, so mcfly works in tmux
if [ -f $HOME/.bashrc ]; then . $HOME/.bashrc; fi

# XDG_VTNR used by systems that use systemd
# if there's no DISPLAY, start X11
if [ -z "${DISPLAY}" ] && [ -n "$XDG_VTNR" ] && [ "$XDG_VTNR" -eq 1 ]; then
    exec startx
fi
