# source .bashrc for tmux
if [ -f $HOME/.bashrc ]; then . $HOME/.bashrc; fi

export PATH=$PATH:$HOME/.config/composer/vendor/bin
export PATH=$PATH:$HOME/.npm-global/bin
export PATH=$PATH:$HOME/bin
export PATH=$PATH:$HOME/.cabal/bin # to get pandoc-unicode-math
export PATH=$PATH:$HOME/.local/bin # to get nvr (neovim-remote)
# to make cmus escape works
export ESCDELAY=25
# used by imgur api
export BW_SESSION=$(pass show bw_session)

# network interfaces
export IFACE_WLAN=$(ls /sys/class/net | grep -E '^(wlan|wlp)')
export IFACE_ETH=$(ls /sys/class/net | grep -E '^(eth|enp)' | tail -n1 | cut -d ' ' -f1)
# battery & adapter
export BATTERY=$(ls /sys/class/power_supply | grep -E '^BAT')
export ADAPTER=$(ls /sys/class/power_supply | grep -E '^ADP')
# history
export HISTSIZE=10000
export HISTFILESIZE=2000
export HISTCONTROL=ignoredups:ignorespace

# Fzf
export FZF_DEFAULT_COMMAND='ag -U --hidden --ignore .git -g ""'
export FZF_DEFAULT_OPTS="--bind=ctrl-j:accept --color=light"

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

# color
LS_COLORS=$LS_COLORS:'di=1;37:' ; export LS_COLORS

# hardeware video acceleration
export LIBVA_DRIVER_NAME=vdpau
export VDPAU_DRIVER=nvidia

# if there's no DISPLAY, start X11
if [ -z "$DISPLAY" ] && [ -n "$XDG_VTNR" ] && [ "$XDG_VTNR" -eq 1 ]; then
    exec startx
fi
