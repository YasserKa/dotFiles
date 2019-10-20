# shorter
alias vi='vim'
alias v='vim'
alias syu='sudo pacman -Syu'
alias z='zathura'
# history
alias h='history'
alias hg='history | grep'
#shutdown
alias reboot='shutdown -r now'
alias shut='shutdown now'

alias top='htop'

# more options
alias ll='ls -alFh'
alias mkdir='mkdir -pv'
alias df='df -Tha --total'
alias cal='cal -m'
# prompt before overriding
alias mv='mv -i'
alias cp='cp -i --preserve=all --reflink=auto'


# color
alias ls='ls --color=auto'
alias grep='grep --color=auto'

# Navigation
alias back='cd $OLDPWD'
alias ..='cd ..'
alias ...='cd ...'

# Misc
alias cmus='screen -q -r -D cmus || screen -S cmus $(which cmus)'
# alias myz='zathura $HOME/Documents/Coding-the-Matrix-Linear-Algebra-through-Computer-Science-Applications.pdf'
# alias notebook='conda run jupyter notebook & disown'


# for more monitors
# alias myscreen='xrandr --output DVI-I-2-1 --auto --left-of eDP1; \
#                   xrandr --output DVI-I-3-2 --auto --right-of eDP1;'
#
# alias noscreen='xrandr --output DVI-I-2-1 --off; \
#                   xrandr --output DVI-I-3-2 --off;'
