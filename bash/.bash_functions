#!/bin/sh

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
            *.t@(gz|lz|xz|b@(2|z?(2))|a@(z|r?(.@(Z|bz?(2)|gz|lzma|xz))))) c=(bsdtar xvf);;
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

function upgrade_system() {
    orphans
    baph -un
}

## path synchronization for ranger
function ranger {
    tempfile="$(mktemp -t ranger-cd.XXXXXX)"
    /usr/bin/ranger --choosedir="$tempfile" "${@:-$(pwd)}"
    if [[ -f "$tempfile" ]] && [[ "$(cat -- "$tempfile")" != "$(echo -n `pwd`)" ]]; then
        cd -- "$(cat "$tempfile")"
    fi
    rm -f -- "$tempfile"
}

function def() {
    sdcv -n --utf8-output --color "$@" 2>&1 | fold --width=$(tput cols) | vimpager
}

function j() {
    paths=$(fasd -dlR "$@")
    path="$HOME"
    # If only one path exists, go to it
    if [[ $(echo -e "$paths" | wc -l) == 1 ]]; then
        path=$paths
    else # Use fzf otherwise
        path=$(echo -e "$paths" | fzf --preview-window hidden --keep-right --height=20 --layout=reverse)
    fi
    cd $path
}

# move file to lower case
function to_lower_case() {
    lower_case=`echo "$@"  | tr '[A-Z]' '[a-z]'`
    mv "$@" "$lower_case"
}

# tmux + fzf
fzftmux() {
    TMUX_SESSION=$(tmux list-sessions | cut -d: -f1 | fzf)
    [[ -z $TMUX_SESSION ]] && return
    tmux attach-session -t $TMUX_SESSION
}
