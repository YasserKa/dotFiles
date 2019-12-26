# shorter
alias vi='vim'
alias v='vim'
alias syu='sudo pacman -Syu'
alias pac='sudo pacman'
alias z='zathura'
# history
alias h='history'
alias hg='history | grep'
#shutdown
alias reboot='shutdown -r now'
alias shutdown='shutdown now'
alias shut='shutdown now'

alias top='htop'

# emacs git & notes
alias magit='i3-msg "workspace --no-auto-back-and-forth 4; exec emacs --funcall=magit-list-repositories"'
alias org='i3-msg "workspace --no-auto-back-and-forth 3; exec emacs --file=$HOME/org/general.org"'

# more options
alias ll='ls -alFh'
alias mkdir='mkdir -pv'
alias df='df -Tha --total'
alias cal='cal -m'
# prompt before overriding
alias mv='mv -i'
alias cp='cp -i --preserve=all --reflink=auto'

#clipboard
alias pbcopy='xclip -selection clipboard'
alias pbpaste='xclip -selection clipboard -o'

# color
alias ls='ls --color=auto'
alias grep='grep --color=auto'

# Navigation
alias back='cd $OLDPWD'
alias ..='cd ..'
alias ...='cd ...'

# Get the last installed packages
alias last='expac --timefmt="%Y-%m-%d %T" "%l\t%w\t%n" | grep explicit | sort | tail -n 20'
# Music player
alias cmus='screen -q -r -D cmus || screen -S cmus $(which cmus)'

# alias myz='zathura $HOME/Documents/Coding-the-Matrix-Linear-Algebra-through-Computer-Science-Applications.pdf'
# alias notebook='conda run jupyter notebook & disown'


# Monitors
# alias myscreen='xrandr --output DVI-I-2-1 --auto --left-of eDP1; \
#                   xrandr --output DVI-I-3-2 --auto --right-of eDP1;'
#
# alias noscreen='xrandr --output DVI-I-2-1 --off; \
#                   xrandr --output DVI-I-3-2 --off;'
