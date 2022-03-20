# shorter
alias vim='nvim'
alias vi='nvim'
alias v='nvim'
alias rc='nvim $MYVIMRC'
alias pac='sudo pacman'
alias z='zathura'
alias cdb='cd -'
alias ps?='ps aux | grep'
alias vpn_up='sudo wg-quick up wg0'
alias vpn_down='sudo wg-quick down wg0'

# history
alias h='history'
alias hg='history | grep'

# sync notes
alias sync_org="wait_internet && rclone sync $HOME/notes/org remote:org --include '*.org'"
alias sync_books="wait_internet && rclone sync $HOME/books books:books"

# shutdown
# don't shutdown if there's an unsaved note file (checked via existence of symbolic linked file)
alias reboot="[ ! -h $HOME/notes/org/\.\#* ] && wait_internet && rclone sync $HOME/notes/org remote:org --include '*.org' && shutdown -r now || dunstify 'unsaved file'"
alias shut="[ ! -h $HOME/notes/org/\.\#* ] && wait_internet && rclone sync $HOME/notes/org remote:org --include '*.org' && shutdown now || dunstify 'unsaved file'"

# alternatives
alias top='htop'
alias ls='exa --color=auto'
alias ll='exa -alFh'
alias cat='bat --pager=less --theme="gruvbox-dark"'

# more options
alias mkdir='mkdir -pv'
alias df='df -Tha --total'
alias cal='cal -m'
alias grep='grep --color=auto'

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

# autojumping
alias vf='fasd -sife nvim' # quick opening files with vim

_fasd_bash_hook_cmd_complete vf j

# emacs git & notes
alias magit='i3-msg "workspace --no-auto-back-and-forth 4; exec emacs --funcall=magit-list-repositories"'
alias org='i3-msg "workspace --no-auto-back-and-forth 3; exec emacs --file=$HOME/notes/org/20210911093036-general.org"'
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
