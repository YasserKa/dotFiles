# General
alias ll='ls -alh'
alias c='clear'
alias claer='clear'
alias mkdir='mkdir -pv'
alias histg='history | grep'
alias top='atop'
alias reboot='shutdown -r'
alias df='df -Tha --total'
alias fhere='find . -name'
alias less='/usr/share/vim/vim80/macros/less.sh'


# Navigation
alias back='cd $OLDPWD'
alias ..='cd ..'
alias ...='cd ...'
alias soap='cd /srv/http/tempo/app/Helpers/SoapUI'
alias game='cd ~/myProjects/pathToGameDevelopment/C/'


# Dictionary
alias def='sdcv -c'

# Network
alias myip='curl http://ipecho.net/plain; echo'
alias pingg='ping 8.8.8.8'
alias yaourt='yaourt --noconfirm'
alias wget='wget -c'
alias search='firefox --search &'

# Resizing tmux 
alias takeover='tmux detach -a'

# Offline arch wiki
alias doc='firefox /usr/share/doc/arch-wiki/html/en/'

# Show physical disks connected
alias disks='lsblk -ido KNAME,TYPE,SIZE,MODEL'

# start ssh-agent at start
alias startx='ssh-agent startx'

alias zathurac='zathura /home/yasser/Documents/Books/The\ C++\ Programming\ Language\ \[4th\ Edition\]\ -\ Bjarne\ Stroustrup.pdf & disown'

function rawc()
{
	RAW_NAME=`echo $1 | cut -d. -f1`
	g++ $1 -o $RAW_NAME && ./$RAW_NAME && rm $RAW_NAME
}

function extract()      # Handy Extract Program
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
