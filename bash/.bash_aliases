# General
alias ll='ls -alh'
alias c='clear'
alias mkdir='mkdir -pv'
alias h='history'
alias hg='history | grep'
alias top='atop'
alias reboot='shutdown --no-wall -r'
alias shutdown='sudo shutdown now'
alias df='df -Tha --total'
alias vi='vim'
alias v='vim'

# Navigation
alias back='cd $OLDPWD'
alias ..='cd ..'
alias ...='cd ...'

# work
alias aug='cd $HOME/Documents/Augmental'
alias workscreen='xrandr --output DVI-I-2-1 --auto --left-of eDP1; \
                  xrandr --output DVI-I-3-2 --auto --right-of eDP1;'

# Misc
alias def='sdcv -c'
alias net='wicd-curses'
alias cmus='screen -q -r -D cmus || screen -S cmus $(which cmus)'
alias myzathura='zathura $HOME/Documents/Coding-the-Matrix-Linear-Algebra-through-Computer-Science-Applications.pdf'
alias notebook='conda run jupyter notebook $HOME/Documents/notebooks & disown'
alias python='python3.6'

# Handy Extract Program
function extract()
{
    if [ -f $1 ] ; then
        case $1 in
            *.tar.bz2)   tar xvjf $1     ;;
            *.tar.gz)    tar xvzf $1     ;;
            *.bz2)       bunzip2 $1      ;;
            *.rar)       unrar x $1      ;;
            *.gz)        gunzip $1       ;;
            *.tar)       tar xvf $1      ;;
            *.tbz2)      tar xvjf $1     ;;
            *.tgz)       tar xvzf $1     ;;
            *.zip)       unzip $1        ;;
            *.Z)         uncompress $1   ;;
            *.7z)        7z x $1         ;;
            *)           echo "'$1' cannot be extracted via >extract<" ;;
        esac
    else
        echo "'$1' is not a valid file!"
    fi
}

# commands run in background automatically
function zathura() { (command zathura "$@" &) }
function xdg-open() { (command xdg-open "$@" &) }
