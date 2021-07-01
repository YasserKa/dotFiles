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

# extract compressed files function
extract() {
    local c e i

    (($#)) || return

    for i; do
        c=''
        e=1

        if [[ ! -r $i ]]; then
            echo "$0: file is unreadable: \`$i'" >&2
            continue
        fi

        case $i in
            *.t@(gz|lz|xz|b@(2|z?(2))|a@(z|r?(.@(Z|bz?(2)|gz|lzma|xz)))))
                   c=(bsdtar xvf);;
            *.7z)  c=(7z x);;
            *.Z)   c=(uncompress);;
            *.bz2) c=(bunzip2);;
            *.exe) c=(cabextract);;
            *.gz)  c=(gunzip);;
            *.rar) c=(unrar x);;
            *.xz)  c=(unxz);;
            *.zip) c=(unzip);;
            *.zst) c=(unzstd);;
            *)     echo "$0: unrecognized file extension: \`$i'" >&2
                   continue;;
        esac

        command "${c[@]}" "$i"
        ((e = e || $?))
    done
    return "$e"
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

function def() {
    sdcv -n --utf8-output --color "$@" 2>&1 | \
        fold --width=$(tput cols) | \
        vimpager
}

# move file to lower case
function to_lower_case() {
    lower_case=`echo "$@"  | tr '[A-Z]' '[a-z]'`
    mv "$@" "$lower_case"
}

# commands run in background automatically
function zathura() { (command zathura "$@" &> /dev/null &) }
function mpv() { (command mpv "$@" &> /dev/null &) }
function xdg-open() { (command xdg-open "$@" &) }
function notebook() { (command conda run jupyter notebook --ip=127.0.0.1 "$@") }
