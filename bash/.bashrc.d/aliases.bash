# Shorter
alias v='nvim'
alias vi='nvim'
alias vim='nvim'
alias h='history'
alias hg='history | grep'
alias pac='sudo pacman'
alias z='zathura'
alias vpn_up='sudo wg-quick up wg0'
alias vpn_down='sudo wg-quick down wg0'

# Sync books
alias syncbooks="wait_internet && rclone sync $HOME/books books:books"

# Alternatives
alias top="btm --color=gruvbox"
alias cat='bat --pager=less --theme="gruvbox-dark"'

# Logging
# Can't override journalctl without using a function
alias journalctl='function journalctl_override(){ (command journalctl "$@" | lnav) }; journalctl_override'
alias logxorg="cat $HOME/.local/share/xorg/Xorg.0.log"
# Doesn't work for dmesg
# Check https://github.com/tstack/lnav/issues/878

# Different options to search for files
alias ls="lsd"
alias lsa="ls --almost-all"    # ignore . ..
alias l="ls --long --classify" # */=>@ indicators
alias la="ls -AFl"
alias ltree="ls --tree --depth=2"
alias ltreea="ls --tree"
alias lt="ls -l --sort=time --reverse"
alias lta="ls -Al --sort=time --reverse"
alias lss="ls -l --total-size --sort=size --reverse"
alias lssa="ls -Al --total-size --sort=size --reverse"

# More options
alias mkdir='mkdir -pv'
alias df='df -Tha --total'
alias cal='cal -m'
alias alsamixer='alsamixer -c 1'
alias grep='grep --color=auto'

# Prompt before overriding
alias mv='mv -i'
alias cp='cp -i --preserve=all --reflink=auto'

# Clipboard
alias pbcopy='xclip -selection clipboard'
alias pbpaste='xclip -selection clipboard -o'

# Navigation
alias ..='cd ..'
alias ...='cd ../..'

# Calculator
alias calc='rofi -show calc -modi calc -no-show-match -no-sort'

# Alert after long commands
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# List last installed packages
alias last='expac --timefmt="%Y-%m-%d %T" "%l\t%w\t%n" | grep explicit | sort | tail -n 20'
alias browse_packages="pacman -Qq | fzf --preview 'pacman -Qil {}' --layout=reverse --bind 'enter:execute(pacman -Qil {} | less)'"

# Music player
alias cmus="screen -q -r -D cmus || screen -S cmus $(which cmus)"

# misc
alias get_mail='polybar-msg hook mail 2 && syncmail && polybar-msg hook mail 1'
alias test_mail='echo | command neomutt -s "Testing mail" yasser.kaddoura19@gmail.com &> /dev/null'
alias myip='curl ifconfig.me; echo'
