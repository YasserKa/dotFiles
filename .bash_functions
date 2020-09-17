#! /usr/bin/bash

function man() {
    env \
        LESS_TERMCAP_mb=$'\e[01;36m' \
        LESS_TERMCAP_md=$'\e[01;36m' \
        LESS_TERMCAP_me=$'\e[0m' \
        LESS_TERMCAP_se=$'\e[0m' \
        LESS_TERMCAP_so=$'\e[01;44;37m' \
        LESS_TERMCAP_ue=$'\e[0m' \
        LESS_TERMCAP_us=$'\e[01;37m' \
        man "$@"
    }

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

## frequently used pacman commands
function orphans() {
    if [[ ! -n $(pacman -Qdtt) ]]; then
        echo "no orphans to remove"
    else
        sudo pacman -Rnsc $(pacman -Qdttq)
    fi
}

## path synchronization for ranger
# (reference: /usr/share/doc/ranger/examples/bash_automatic_cd.sh)
function ranger {
    tempfile="$(mktemp -t ranger-cd.XXXXXX)"
    /usr/bin/ranger --choosedir="$tempfile" "${@:-$(pwd)}"
    if [[ -f "$tempfile" ]] && [[ "$(cat -- "$tempfile")" != "$(echo -n `pwd`)" ]]; then
        cd -- "$(cat "$tempfile")"
    fi
    rm -f -- "$tempfile"
}

# commands run in background automatically
function zathura() { (command zathura "$@" &> /dev/null &) }
function mpv() { (command mpv "$@" &> /dev/null &) }
function xdg-open() { (command xdg-open "$@" &) }
