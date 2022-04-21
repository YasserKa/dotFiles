# Shorter
alias v='nvim'
alias vi='nvim'
alias vim='nvim'
alias h='history'
alias hg='history | grep'
alias pac='sudo pacman'
alias z='zathura'
alias cdb='cd -'
alias psg='ps aux | grep'
alias vpn_up='sudo wg-quick up wg0'
alias vpn_down='sudo wg-quick down wg0'

# Open configuration files easily
# First arg directory, others files to open
# Open in current terminal or a new one depending on shell interactivity
function rc () {
    [[ ! $TERMINAL == "kitty" ]] && notify-send "$TERMINAL not supported for rc function" \
        && return

    path="$1"
    files_to_open="${@:2}"

    # Check if it's in a terminal by the exit code
    if tty -s; then
        cd $path && $EDITOR $files_to_open
    else
        # The +only -o arguments are a hack to mitigate nvim's warning upon
        # exiting for editing multiple files
        # -o open files in windows and +only keep one of them
        $TERMINAL --directory $path $EDITOR +only -o $files_to_open
    fi
}

alias rcvim="rc $NVIM_CONFIG init.vim"
alias rcfeh="rc $FEH_CONFIG *"
alias rckitty="rc $KITTY_CONFIG kitty.conf"
alias rcbash="rc $HOME .bashrc .bash_aliases .bash_functions .bash_profile"
alias rctmux="rc $TMUX_CONFIG .tmux.conf"
alias rcqutebrowser="rc $QUTEBROWSER_CONFIG config.py userscripts/*"
alias rci3="rc $I3_CONFIG config"
alias rcrofi="rc $ROFI_CONFIG config.rasi"
alias rcdunst="rc $DUNST_CONFIG dunstrc"
alias rcpolybar="rc $POLYBAR_CONFIG config.ini"
alias rcgpg="rc $GNUPGHOME gpg-agent.conf"
alias rcssh="rc $SSH_CONFIG ssh sshd_config"
alias rctmux="rc $TMUX_CONFIG tmux.conf"
# Open Emacs's config file in Emacs
alias rcemacs="emacs --file $EMACS_CONFIG/init.el"

# sync notes
alias sync_org="wait_internet && rclone sync $NOTES_ORG_HOME remote:org --include 'fast_access.org' --include 'groceries.org'"
alias sync_books="wait_internet && rclone sync $HOME/books books:books"

# shutdown
# don't shutdown if there's an unsaved note file (checked via existence of symbolic linked file)
alias reboot="[[ ! -h $NOTES_ORG_HOME/\.\#* ]] && wait_internet && rclone sync $NOTES_ORG_HOME org_notes:org --include 'fast_access.org' --include 'groceries.org' && shutdown -r now || dunstify 'unsaved file'"
alias shut="[[ ! -h $NOTES_ORG_HOME/\.\#* ]] && wait_internet && rclone sync $NOTES_ORG_HOME org_notes:org --include 'fast_access.org' --include 'groceries.org' && shutdown now || dunstify 'unsaved file'"

# Alternatives
alias top="btm --color=gruvbox"
alias cat='bat --pager=less --theme="gruvbox-dark"'

# Can't override journalctl using a function
alias journalctl='function journalctl_override(){ (command journalctl "$@" | lnav) }; journalctl_override'
alias logxorg="cat $HOME/.local/share/xorg/Xorg.0.log | lnav"
# Doesn't work for dmesg
# Check https://github.com/tstack/lnav/issues/878

# commands run in background automatically
function zathura() { (command zathura "$@" &> /dev/null &) }
function mpv() { (command mpv "$@" &> /dev/null &) }
function xdg-open() { (command xdg-open "$@" &) }

# Colorful
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

# clipboard
alias pbcopy='xclip -selection clipboard'
alias pbpaste='xclip -selection clipboard -o'

# navigation
alias ..='cd ..'
alias ...='cd ../..'

# calculator
alias calc='rofi -show calc -modi calc -no-show-match -no-sort'

# Alert after long commands
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# autojumping
alias vf='fasd -sife nvim' # quick opening files with vim

_fasd_bash_hook_cmd_complete vf j

function app_runner() {
    window_title="$1"
    command_to_run="$2"

    xdotool search --name $window_title windowactivate
    if [[ $? != 0 ]]; then
        bash -c "$command_to_run"
        # Wait until window is visible
        while [[ -z $(wmctrl -xl | grep " $window_title") ]]; do sleep 0.5; done
        xdotool search --name $window_title windowactivate
    fi
}

function org() {
    title="emacs_org"
    app_runner $title "emacs --title=$title --file=$NOTES_ORG_HOME/general.org &"
}

function magit() {
    title="magit"
    app_runner $title "emacs --title=$title --funcall=magit-list-repositories &"
}

alias check_audi_amount="cd $HOME/Projects/check-bank-acount && pipenv run python main.py"
# cron
# TODO: make this work like rc files
alias cron="$EDITOR $XDG_CONFIG_HOME/crons.cron; crontab $XDG_CONFIG_HOME/crons.cron"

# last installed packages
alias last='expac --timefmt="%Y-%m-%d %T" "%l\t%w\t%n" | grep explicit | sort | tail -n 20'

# music player
alias cmus="screen -q -r -D cmus || screen -S cmus $(which cmus)"

alias abook="abook --config $ABOOK_CONFIG/abookrc --datafile $ABOOK_DATA/addressbook"

# misc
alias get_mail='polybar-msg hook mail 2 && mailsync && polybar-msg hook mail 1'
alias myip='curl ifconfig.me; echo'
