# shorter
alias vim='nvim'
alias vi='nvim'
alias v='nvim'
alias rc='nvim $MYVIMRC'
alias syu='sudo pacman -Syu'
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
alias sync_org='rclone sync /home/yasser/notes/org remote:org --include "*.org"'

# shutdown
alias reboot='rclone sync /home/yasser/notes/org remote:org --include "*.org"; shutdown -r now'
alias shut='rclone sync /home/yasser/notes/org remote:org --include "*.org"; shutdown now'

# alternatives
alias top='htop'
alias ls='exa --color=auto'
alias ll='exa -alFh'
alias cat='bat'

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
alias j='fasd_cd -di'

_fasd_bash_hook_cmd_complete vf j

# emacs git & notes
alias magit='i3-msg "workspace --no-auto-back-and-forth 4; exec emacs --funcall=magit-list-repositories"'
alias org='i3-msg "workspace --no-auto-back-and-forth 3; exec emacs --file=$HOME/notes/org/general.org"'

# cron
alias cron='$EDITOR $XDG_CONFIG_HOME/crons.cron; crontab $XDG_CONFIG_HOME/crons.cron'

# project setup
alias cool='cd /srv/http/cooldown/; alacritty -e ./artisan serve 2> /dev/null & qutebrowser http://localhost:8000/getMatch --target window 2> /dev/null & nvim'

# last installed packages
alias last='expac --timefmt="%Y-%m-%d %T" "%l\t%w\t%n" | grep explicit | sort | tail -n 20'

# music player
alias cmus='screen -q -r -D cmus || screen -S cmus $(which cmus)'

# misc
alias tea_time='dunstify "Tea time in 5 minutes"; sleep $((5*60)); dunstify "Tea time"'
alias get_mail='polybar-msg hook mail 2 && mailsync && polybar-msg hook mail 1'
alias myip='curl ifconfig.me; echo'
