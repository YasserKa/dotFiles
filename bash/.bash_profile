export PATH=$PATH:$HOME/.config/composer/vendor/bin
export PATH=$PATH:$HOME/miniconda3/bin
export PATH=$PATH:$HOME/bin
# to make cmus escape works
export ESCDELAY=25
# used by imgur api
export IMGUR_CLIENT_ID="3fcfcb6de9ff8f8"

# network interfaces
export IFACE_WLAN=$(ls /sys/class/net | grep -E '^(wlan|wlp)')
export IFACE_ETH=$(ls /sys/class/net | grep -E '^(eth|enp)')
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

# default
export EDITOR=neovim
export VISUAL=neovim
export MYVIMRC="$HOME/.vimrc"
export BROWSER=qutebrowser
export TERMINAL=termite
export DIFFPROG=vimdiff
export PAGER=vimpager

# color
LS_COLORS=$LS_COLORS:'di=1;37:' ; export LS_COLORS

# hardeware video acceleration
export LIBVA_DRIVER_NAME=vdpau
export VDPAU_DRIVER=nvidia

# configurations in one place
[ "$XDG_CONFIG_HOME" ] || export XDG_CONFIG_HOME="$HOME/.config"
[ "$XDG_DATA_HOME" ] || export XDG_DATA_HOME="$HOME/.local/share"

export GTK2_RC_FILES="$XDG_CONFIG_HOME/gtk-2.0/gtkrc-2.0"
export VIMPAGER_RC="$XDG_CONFIG_HOME/vimpagerrc"
export INPUTRC="$XDG_CONFIG_HOME/inputrc"

if [ -z "$DISPLAY" ] && [ -n "$XDG_VTNR" ] && [ "$XDG_VTNR" -eq 1 ]; then
    export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"
    exec startx
fi
