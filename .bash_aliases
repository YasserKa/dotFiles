# Shorter
alias vim='nvim'
alias vi='nvim'
alias v='nvim'
alias rc='nvim $MYVIMRC'
alias syu='sudo pacman -Syu'
alias pac='sudo pacman'
alias z='zathura'

# History
alias h='history'
alias hg='history | grep'

# Shutdown
alias reboot='shutdown -r now'
alias shut='shutdown now'

# Alternatives
alias top='htop'
alias ls='exa --color=auto'
alias ll='exa -alFh'

# More options
alias mkdir='mkdir -pv'
alias df='df -Tha --total'
alias cal='cal -m'
alias grep='grep --color=auto'

# Prompt before overriding
alias mv='mv -i'
alias cp='cp -i --preserve=all --reflink=auto'

# Clipboard
alias pbcopy='xclip -selection clipboard'
alias pbpaste='xclip -selection clipboard -o'

# Navigation
alias back='cd $OLDPWD'
alias ..='cd ..'
alias ...='cd ...'

# Autojumping
alias f='fasd -fe nvim' # quick opening files with vim
alias j='fasd_cd -d'
_fasd_bash_hook_cmd_complete f j

# Emacs git & notes
alias magit='i3-msg "workspace --no-auto-back-and-forth 4; exec emacs --funcall=magit-list-repositories"'
alias org='i3-msg "workspace --no-auto-back-and-forth 3; exec emacs --file=$HOME/org/general.org"'

# Cron
alias cron='$EDITOR $XDG_CONFIG_HOME/crons.cron; crontab $XDG_CONFIG_HOME/crons.cron'

# Project setup
alias cool='cd /srv/http/cooldown/; alacritty -e ./artisan serve 2> /dev/null & qutebrowser http://localhost:8000/getMatch --target window 2> /dev/null & nvim'

# Last installed packages
alias last='expac --timefmt="%Y-%m-%d %T" "%l\t%w\t%n" | grep explicit | sort | tail -n 20'
# Music player
alias cmus='screen -q -r -D cmus || screen -S cmus $(which cmus)'

alias tea_time='dunstify "Tea time in 5 minutes"; sleep $((5*60)); dunstify "Tea time"'
alias get_mail='polybar-msg hook mail 2 && mailsync && polybar-msg hook mail 1'
