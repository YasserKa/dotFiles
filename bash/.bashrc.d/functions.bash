#!/usr/bin/env bash

# Extract compressed files function
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

# Remove dependencies that are no longer needed (orphans)
orphans() {
    if [[ $(pacman -Qdtt) ]]; then
        echo "no orphans to remove"
    else
        sudo pacman -Rnsc "$(pacman -Qdttq)"
    fi
}

upgrade_system() {
    orphans
    paru --sync --refresh --sysupgrade --noconfirm
    printf "%s\n" "Updating Vim packages"
    nvim -c 'PlugUpgrade|PlugInstall|qall'
    printf "%s\n" "Updating Emacs packages"
    # Update packages and exit afterwards
    emacs --no-window-system --eval "(progn
        (add-hook 'emacs-startup-hook #'(lambda () (interactive) (save-buffers-kill-emacs)))
        (auto-packages-update-now))"

    echo "Upgrading PyPi packages"
    pip install --upgrade --user tmuxp poetry i3ipc neovim-remote selenium \
        PyMuPDF pyperclip flake8 black adblock tldextract sci-hub

    echo 'Use the following commands to checkup on the system:
    systemctl --failed --user
    logxorg
    sudo journalctl -p 3 -b'
}

# Automatically change current directory to the last visited one after ranger quits
# /usr/share/doc/ranger/examples/shell_automatic_cd.sh
ranger() {
    temp_file="$(mktemp -t "ranger_cd.XXXXXXXXXX")"
    command ranger --choosedir="$temp_file" -- "${@:-$PWD}"
    if chosen_dir="$(cat -- "$temp_file")" && [ -n "$chosen_dir" ] && [ "$chosen_dir" != "$PWD" ]; then
        cd -- "$chosen_dir" || exit
    fi
    rm -f -- "$temp_file"
}

# Use fasd and FZF to jump through directories
j() {
    paths=$(fasd -dlR "$@")
    path="$HOME"
    # If only one path exists, go to it
    if [[ $(echo -e "$paths" | wc -l) == 1 ]]; then
        path=$paths
    else # Use fzf otherwise
        path=$(echo -e "$paths" | fzf --preview-window hidden --keep-right --height=20 --layout=reverse)
    fi
    cd "$path" || exit 1
}

# Pick tmux sessions using FZF
fzftmux() {
    TMUX_SESSION=$(tmux list-sessions | cut -d: -f1 | fzf)
    [[ -z $TMUX_SESSION ]] && return
    tmux attach-session -t "$TMUX_SESSION"
}

# Edit last n lines in history using $EDITOR
fclast() { command fc "-${1}" 0; }

# Open TUIR apps from menu picker (spawning a termianl) or the command line
open_cli() {
    local command="$1"

    [[ ! $(command -v "$command") ]] &&
        notify-send "$command doesn't exit" && return 127

    [[ $TERMINAL != "kitty" && $TERMINAL != "alacritty" ]] && notify-send "$TERMINAL is not supported" && return 1

    # Command is run from a shell using -c option
    if [[ "$-" != *c* ]]; then
        command "$command"
    else
        $TERMINAL --detach -e bash -c "$command && exec bash"
    fi
}

cli_list=("newsboat" "neomutt")

for cli in "${cli_list[@]}"; do
    # shellcheck disable=SC2139
    alias "$cli=open_cli $cli"
done
unset cli

# Open TUIR with top page within 24 hours by pressing "g t 2"
tuir() {
    (xdotool search --sync --name "^Front Page - tuir" key g t 2 &)
    open_cli tuir
}

################################################################################
# Open file/s in a directory depending on the interactivity of the shell
# Arguments:
#     $1: path of directory
#     $@: file name
################################################################################
open_file() {
    [[ $TERMINAL != "kitty" && $TERMINAL != "alacritty" ]] &&
        notify-send "$TERMINAL not supported for open_file function" &&
        return 1

    local -r DIRECTORY_PATH="$1"
    shift
    [[ ! -d "${DIRECTORY_PATH}" ]] && notify-send "${DIRECTORY_PATH} doesn't exist" && exit 1

    # Check if it's in a terminal
    if [[ "$-" != *c* ]]; then
        # The -o +only arguments are a hack to mitigate nvim's warning upon
        # exiting for editing multiple files
        # -o open files in windows and +only keep one of them
        cd "${DIRECTORY_PATH}" && "${EDITOR}" +only -o "$@"
    else
        cd "${DIRECTORY_PATH}" && "${_EDITOR_GUI}" "$@" -- -o +only
    fi
}

alias rcreadline='open_file $XDG_CONFIG_HOME/readline inputrc'
alias rcgpg='open_file $GNUPGHOME gpg-agent.conf'
alias rcssh='open_file $HOME ssh sshd_config'
alias rcx11='open_file ${XINITRC%/*} xinitrc'
alias rcbash='open_file $HOME .bashrc .bash_profile .profile .bashrc.d/*bash'
alias rckitty='open_file $XDG_CONFIG_HOME/kitty kitty.conf'
alias rcvim='open_file $XDG_CONFIG_HOME/nvim init.vim'
alias rci3='open_file $XDG_CONFIG_HOME/i3 config'
alias rcneomutt='open_file $XDG_CONFIG_HOME/neomutt neomuttrc'
alias rctuir='open_file $XDG_CONFIG_HOME/tuir tuir.cfg'
alias rcnewsboat='open_file $XDG_CONFIG_HOME/newsboat'
alias rcfeh='open_file $XDG_CONFIG_HOME/feh keys'
alias rcrofi='open_file $XDG_CONFIG_HOME/rofi config.rasi'
alias rcdunst='open_file $XDG_CONFIG_HOME/dunst dunstrc'
alias rcpolybar='open_file $XDG_CONFIG_HOME/polybar config.ini'
alias rctmux='open_file $XDG_CONFIG_HOME/tmux tmux.conf'
alias rczathura='open_file $XDG_CONFIG_HOME/zathura zathurarc'
alias rcqutebrowser='open_file $XDG_CONFIG_HOME/qutebrowser config.py'

# Open Emacs's config file in Emacs
alias rcemacs='emacs --file $XDG_CONFIG_HOME/emacs/init.el'

rcdotfiles() {
    if [[ "$-" != *c* ]]; then
        cd "${HOME}/dotfiles/" || return
    else
        "${TERMINAL}" -e --directory "${HOME}/dotfiles/"
    fi
}
alias cron='open_file $XDG_CONFIG_HOME/cron crons.cron; crontab $XDG_CONFIG_HOME/cron/crons.cron'

open_gui() {
    local name="$1"
    local command="$2"

    if ! xdotool search --name "$name" windowactivate; then
        bash -c "chronic ${command} & disown"

        # --sync doesn't seem to work, so keep activating until it works
        while [[ "$(xdotool getactivewindow getwindowname)" != "$name" ]]; do
            xdotool search --sync --name "$name" windowactivate
        done
    fi
}

org() {
    local name="emacs_org_name"

    open_gui $name "emacs --title=$name --file=$_NOTES_ORG_HOME/capture.org"
}

magit() {
    local name="magit_name"
    local git_root
    git_root=$(git rev-parse --show-toplevel)

    chronic git rev-parse --show-toplevel || return 1
    open_gui $name "emacs --title=$name --eval '(magit-status \"${git_root}\")'"
}

alias gitdotfiles='cd $HOME/dotfiles && magit'

syncorg() {
    emacsclient --no-wait --eval "(org-save-all-org-buffers)"
    "$HOME"/bin/wait_internet && rclone sync "${_NOTES_ORG_HOME}" org_notes:org --include 'fast_access.org' --include 'groceries.org'
}

reboot() {
    syncorg
    command shutdown --reboot now
}

shutdown() {
    syncorg
    command shutdown now
}

# Which is unreliable, use type -P instead
# https://unix.stackexchange.com/questions/85249/why-not-use-which-what-to-use-then
which() {
    printf >&2 'The which command is unreliable. Use type -P %s\n' "$*"
    return 2
}

# Reload qutebrowser and return to active window
reload_browser() {
    WINDOW_NAME="$1"
    FOCUSED_ID="$(xdotool getwindowfocus)"
    xdotool search --onlyvisible --name "${WINDOW_NAME}" windowfocus key --delay 100 --window %@ 'r'
    sleep 0.5
    xdotool windowfocus "${FOCUSED_ID}"
}

# Commands run in background automatically
zathura() { (command zathura "$@" &>/dev/null &) }
mpv() { (command mpv "$@" &>/dev/null &) }
xdg-open() { (command xdg-open "$@" &) }
