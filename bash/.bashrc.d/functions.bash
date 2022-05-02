#!/usr/bin/env bash

# extract compressed files function
function extract() {
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
    paru --sync --refresh --sysupgrade --noconfirm
    echo "TODO: include emacs, vim, and package list update"
}

## path synchronization for ranger
function ranger() {
    tempfile="$(mktemp -t ranger-cd.XXXXXX)"
    /usr/bin/ranger --choosedir="$tempfile" "${@:-$(pwd)}"
    if [[ -f "$tempfile" ]] && [[ "$(cat -- "$tempfile")" != "$(echo -n `pwd`)" ]]; then
        cd -- "$(cat "$tempfile")"
    fi
    rm -f -- "$tempfile"
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

# Open TUIR with top page within 24 hours
function tuir() {
    ( xdotool search --sync --name "^Front Page - tuir" key g t 2 & )
    command tuir
}

# tmux + fzf
function fzftmux() {
    TMUX_SESSION=$(tmux list-sessions | cut -d: -f1 | fzf)
    [[ -z $TMUX_SESSION ]] && return
    tmux attach-session -t $TMUX_SESSION
}

function open_cli() {
    local command="$1"

    [[ ! $(command -v $command) ]] && notify-send "$command doesn't exit"

    [[ $TERMINAL != "kitty" && $TERMINAL != "alacritty" ]] && notify-send "$TERMINAL is not supported" && return

    # Check if it's in a terminal
   if [[ $TERMINAL == "kitty" ]]; then
       # "s" Commands are read from the standard input device such as keyboard
        [[ "$-" == *s* ]] && $command || $TERMINAL -- $command
    else
        [[ "$-" == *s* ]] && $command || $TERMINAL -e $command
   fi

}

cli_list=("neomutt" "tuir" "newsboat")

for cli in "${cli_list[@]}"; do
  alias $cli="open_cli $cli"
done
unset cli

alias neomutt="open_cli neomutt"

open_gui() {
    local name="$1"
    local command="$2"

    xdotool search --name $name windowactivate
    if [[ $? != 0 ]]; then
        bash -c "chronic ${command} & disown"

        # Wait until window is visible
        xdotool search --sync --name "^$name$" windowactivate
    fi
}

function org() {
    local name="emacs_org_name"

    open_gui $name "emacs --title=$name --file=$NOTES_ORG_HOME/capture.org"
}

function magit() {
    local name="magit_name"
    local git_root=$(git rev-parse --show-toplevel)

    chronic git rev-parse --show-toplevel || return 1
    open_gui $name \
        "emacs --title=$name --eval '(magit-status \"${git_root}\")'"
}

function syncorg() {
   emacsclient --no-wait --eval "(org-save-all-org-buffers)"
   $HOME/bin/wait_internet && rclone sync ${NOTES_ORG_HOME} org_notes:org --include 'fast_access.org' --include 'groceries.org'
}


function reboot() {
    syncorg
    command shutdown --reboot now
}

function shutdown() {
    syncorg
    command shutdown now
}
