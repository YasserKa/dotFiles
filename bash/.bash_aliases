# shorter
alias vim='nvim'
alias vi='nvim'
alias v='nvim'
alias pac='sudo pacman'
alias z='zathura'
alias cdb='cd -'
alias ps?='ps aux | grep'
alias vpn_up='sudo wg-quick up wg0'
alias vpn_down='sudo wg-quick down wg0'

alias rcvim="nvim-qt $MYVIMRC"
alias rcbash="nvim-qt $HOME/.bashrc"
alias rcbash_alias="nvim-qt $HOME/.bash_alias"
alias rctmux="nvim-qt $HOME/.tmux.conf"
alias rcemacs="emacs --file $HOME/.emacs.d/init.el"
alias rcqutebrowser="nvim-qt $XDG_CONFIG_HOME/qutebrowser/config.py"
alias rci3="nvim-qt $XDG_CONFIG_HOME/i3/config"
alias rcrofi="nvim-qt $XDG_CONFIG_HOME/rofi/config.rasi"
alias rcdunst="nvim-qt $XDG_CONFIG_HOME/dunst/dunstrc"
alias rcpolybar="nvim-qt $XDG_CONFIG_HOME/polybar/config.ini"

# history
alias h='history'
alias hg='history | grep'

# sync notes
alias sync_org="wait_internet && rclone sync $HOME/notes/org remote:org --include 'fast_access.org' --include 'groceries.org'"
alias sync_books="wait_internet && rclone sync $HOME/books books:books"

# shutdown
# don't shutdown if there's an unsaved note file (checked via existence of symbolic linked file)
alias reboot="[ ! -h $HOME/notes/org/\.\#* ] && wait_internet && rclone sync $HOME/notes/org org_notes:org --include 'fast_access.org' --include 'groceries.org' && shutdown -r now || dunstify 'unsaved file'"
alias shut="[ ! -h $HOME/notes/org/\.\#* ] && wait_internet && rclone sync $HOME/notes/org org_notes:org --include 'fast_access.org' --include 'groceries.org' && shutdown now || dunstify 'unsaved file'"

# alternatives
alias top='htop'
alias cat='bat --pager=less --theme="gruvbox-dark"'

# colorful

alias ls='ls --hyperlink=auto --color=auto "$@"'
alias ll='ls -alF --hyperlink=auto --color=auto "$@"'
alias grep='grep --color=auto'
# Disabled for now, because it doesn't support --hyperlink that's used in kitty
# alias ls='exa --color=auto'
# alias ll='exa -alFh'

# more options
alias mkdir='mkdir -pv'
alias df='df -Tha --total'
alias cal='cal -m'
alias alsamixer='alsamixer -c 1'
alias networkmanager_rofi="networkmanager_dmenu -lines 5 -monitor -2 -location 0"

# prompt before overriding
alias mv='mv -i'
alias cp='cp -i --preserve=all --reflink=auto'

# clipboard
alias pbcopy='xclip -selection clipboard'
alias pbpaste='xclip -selection clipboard -o'

# navigation
alias ..='cd ..'
alias ...='cd ../..'

# calculator
alias calc='rofi -show calc -modi calc -no-show-match -no-sort'

# Alert after long commands
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# autojumping
alias vf='fasd -sife nvim' # quick opening files with vim

_fasd_bash_hook_cmd_complete vf j

# emacs git & notes
alias magit='i3-msg "workspace --no-auto-back-and-forth 4; exec emacs --funcall=magit-list-repositories"'
alias org='i3-msg "workspace --no-auto-back-and-forth 3; exec emacs --file=$HOME/notes/org/general.org"'
alias slack='i3-msg "workspace --no-auto-back-and-forth 7; exec slack"'

alias check_audi_amount="cd $HOME/Projects/check-bank-acount && pipenv run python main.py"

# cron
alias cron='$EDITOR $XDG_CONFIG_HOME/crons.cron; crontab $XDG_CONFIG_HOME/crons.cron'

# last installed packages
alias last='expac --timefmt="%Y-%m-%d %T" "%l\t%w\t%n" | grep explicit | sort | tail -n 20'

# music player
alias cmus='screen -q -r -D cmus || screen -S cmus $(which cmus)'

# misc
alias get_mail='polybar-msg hook mail 2 && mailsync && polybar-msg hook mail 1'
alias myip='curl ifconfig.me; echo'
