# shellcheck shell=bash
# shellcheck disable=SC2155,SC2010

# Shared profile among shells
#
# Guideline
# Personal envs (Not used by applications) have a _ prefix (e.g. _EDITOR_GUI)
# Note: Before making a new env var, Use something like "rg --hidden <pattern>" to figure out which files are dependent on it

# History
# Ongoing session
export HISTSIZE=10000
# Ignore common commands
export HISTIGNORE="&:[ ]*:exit:ls:bg:fg:history:pls:clear:*/pypoetry/virtualenvs/*"

# Default
export EDITOR=nvim
export VISUAL=nvim
export BROWSER=qutebrowser
export TERMINAL=kitty
export DIFFPROG=vimdiff
export PAGER=vimpager
export MYVIMRC="$HOME/.config/nvim/init.vim"

export _EDITOR_GUI=nvim-qt

# Conform to XDG standards
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_MUSIC_HOME="$HOME/Music"

# Conform to XDG standards
# https://wiki.archlinux.org/title/XDG_Base_Directory
export GTK2_RC_FILES="$XDG_CONFIG_HOME/gtk-2.0/gtkrc-2.0"
export VIMPAGER_RC="$XDG_CONFIG_HOME/vimpagerrc"
export JUPYTER_CONFIG_DIR="$XDG_CONFIG_HOME/jupyter"
export INPUTRC="$XDG_CONFIG_HOME/readline/inputrc"
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"
export NOTMUCH_CONFIG="$XDG_CONFIG_HOME/notmuch/config"
export NPM_CONFIG_USERCONFIG="$XDG_CONFIG_HOME/npm/npmrc"
export XINITRC="$XDG_CONFIG_HOME/X11/xinitrc"
export XDG_CONFIG_ISYNC="$XDG_CONFIG_HOME/isync/mbsyncrc"
export GNUPGHOME="$XDG_DATA_HOME/gnupg"
export PASSWORD_STORE_DIR="$XDG_DATA_HOME/pass"
# Cookie needed to acces the X server
export XAUTHORITY="$HOME/.Xauthority"

# Path to notes
export _NOTES_ORG_HOME="$HOME/notes/org"

# Go packages
export GOPATH="$XDG_DATA_HOME/go"

# Make cmus <escape> works
export ESCDELAY=25
# Used by bitwarden cli
export BW_SESSION=$(pass show bw_session)

export GTK_THEME="Adwaita:dark" # Remove the warning from GTK apps "gtk-contained-dark.css Missing closing bracket"

# Specifying language environment
export LC_ALL="en_US.utf8"

# Hunspell's Dictionary path
export DICPATH="$XDG_CONFIG_HOME/.dictionary_en"

# Used by polybar config
# Network interfaces
export IFACE_WLAN=$(ls /sys/class/net | grep -E '^(wlan|wlp)' | tail -n1 | cut -d ' ' -f1)
export IFACE_ETH=$(ls /sys/class/net | grep -E '^(eth|enp)' | tail -n1 | cut -d ' ' -f1)
# Battery & adapter
export BATTERY=$(ls /sys/class/power_supply | grep -E '^BAT' | tail -n1 | cut -d ' ' -f1)
export ADAPTER=$(ls /sys/class/power_supply | grep -E '^ADP' | tail -n1 | cut -d ' ' -f1)

# shellcheck disable=1091
source "$XDG_CONFIG_HOME/fzf/.fzf"

# Expand PATH
export PATH=$PATH:$HOME/bin:$HOME/.local/bin:$XDG_CONFIG_HOME/jupyter/bin:$XDG_CONFIG_HOME/neomutt/bin:$XDG_CONFIG_HOME/tmux/bin

# Disable pipx's emojis
export USE_EMOJI=0

# Start manager for GPG & SSH agents
eval "$(keychain --quick --quiet --nogui --eval --noask --gpg2 --agents "gpg,ssh" id_rsa 116F256041ACF55D33334B77F69626AEBEC29AA7)"

# XDG_VTNR is set by pam_systemd upon login
# If there's no DISPLAY, start X11
if [[ -z "${DISPLAY}" ]] && [[ "$XDG_VTNR" -eq 1 ]]; then
	exec startx "$XINITRC"
fi
