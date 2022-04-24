# Shared profile among shells
# Default
export EDITOR=nvim
export VISUAL=nvim
export BROWSER=qutebrowser
export TERMINAL=kitty
export DIFFPROG=vimdiff
export PAGER=vimpager
export MYVIMRC="$HOME/.config/nvim/init.vim"

export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export JUPYTER_CONFIG_DIR="$XDG_CONFIG_HOME/jupyter"
export XDG_STATE_HOME="$HOME/.local/state"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_MUSIC_HOME="$HOME/Music"

# configurations in one place
export GTK2_RC_FILES="$XDG_CONFIG_HOME/gtk-2.0/gtkrc-2.0"
export VIMPAGER_RC="$XDG_CONFIG_HOME/vimpagerrc"
export INPUTRC="$XDG_CONFIG_HOME/readline/inputrc"
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"
export NOTMUCH_CONFIG="$XDG_CONFIG_HOME/notmuch/config"
export NPM_CONFIG_USERCONFIG="$XDG_CONFIG_HOME/npm/npmrc"
export XINITRC="$XDG_CONFIG_HOME/X11/xinitrc"
export XDG_CONFIG_ISYNC="$XDG_CONFIG_HOME/isync/mbsyncrc"
export GNUPGHOME="$XDG_DATA_HOME/gnupg"
export XAUTHORITY="$XDG_RUNTIME_DIR/Xauthority"
export PASSWORD_STORE_DIR="$XDG_DATA_HOME/pass"

# Used for scripting purposes
export POLYBAR_CONFIG="$XDG_CONFIG_HOME/polybar"
export FEH_CONFIG="$XDG_CONFIG_HOME/feh"
export DUNST_CONFIG="$XDG_CONFIG_HOME/dunst"
export ROFI_CONFIG="$XDG_CONFIG_HOME/rofi"
export I3_CONFIG="$XDG_CONFIG_HOME/i3"
export QUTEBROWSER_CONFIG="$XDG_CONFIG_HOME/qutebrowser"
export NVIM_CONFIG="$XDG_CONFIG_HOME/nvim"
export KITTY_CONFIG="$XDG_CONFIG_HOME/kitty"
export EMACS_CONFIG="$XDG_CONFIG_HOME/emacs"
export TMUX_CONFIG="$XDG_CONFIG_HOME/tmux"
export ABOOK_CONFIG="$XDG_CONFIG_HOME/abook"
export ABOOK_DATA="$XDG_DATA_HOME/abook"
export DOTFILES_HOME="$HOME/dotfiles"
export SSH_CONFIG="$HOME/.ssh"

export NOTES_ORG_HOME="$HOME/notes/org"

# to make cmus escape works
export ESCDELAY=25
# used by bitwarden cli
export BW_SESSION=$(pass show bw_session)

# Specifying language environment
export LC_ALL="en_US.utf8"

# used by polybar config
# network interfaces
export IFACE_WLAN=$(ls /sys/class/net | grep -E '^(wlan|wlp)' | tail -n1 | cut -d ' ' -f1)
export IFACE_ETH=$(ls /sys/class/net | grep -E '^(eth|enp)' | tail -n1 | cut -d ' ' -f1)
# battery & adapter
export BATTERY=$(ls /sys/class/power_supply | grep -E '^BAT' | tail -n1 | cut -d ' ' -f1)
export ADAPTER=$(ls /sys/class/power_supply | grep -E '^ADP' | tail -n1 | cut -d ' ' -f1)

# Fzf
# --follow symlinks
export FZF_DEFAULT_COMMAND='ag --follow --hidden --ignore .git -g ""'
export FZF_DEFAULT_OPTS="--bind='ctrl-j:accept,alt-n:preview-page-down,alt-p:preview-page-up' --color=light --preview='bat --color=always --style=numbers --theme gruvbox-dark {}'"

export PATH=$PATH:$HOME/bin:$HOME/.local/bin:$XDG_CONFIG_HOME/jupyter/bin:$XDG_CONFIG_HOME/mutt/bin

# XDG_VTNR is set by pam_systemd upon login
# if there's no DISPLAY, start X11
if [ -z "${DISPLAY}" ] && [ "$XDG_VTNR" -eq 1 ]; then
    exec startx "$XINITRC"
fi
