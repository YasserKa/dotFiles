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
function open_file () {
    [[ $TERMINAL != "kitty" && $TERMINAL != "alacritty" ]] \
        && notify-send "$TERMINAL not supported for open_file function" \
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
        if [[ $TERMINAL == "kitty" ]]; then
            $TERMINAL --working-directory $path bash -ic "cd $path && $EDITOR +only -o $files_to_open"
        else
            $TERMINAL --working-directory $path -e bash -ic "cd $path && $EDITOR +only -o $files_to_open"
        fi
    fi
}

alias rcvim="open_file $XDG_CONFIG_HOME/nvim init.vim"
alias rcreadline="open_file $XDG_CONFIG_HOME/readline inputrc"
alias rcneomutt="open_file $XDG_CONFIG_HOME/neomutt neomuttrc"
alias rctuir="open_file $XDG_CONFIG_HOME/tuir tuir.cfg"
alias rcfeh="open_file $XDG_CONFIG_HOME/feh keys"
alias rckitty="open_file $XDG_CONFIG_HOME/kitty kitty.conf \"*\""
alias rcbash="open_file $HOME .bashrc .bash_profile .bashrc.d/*bash"
alias rctmux="open_file $TMUX_CONFIG .tmux.conf"
alias rcqutebrowser="open_file $XDG_CONFIG_HOME/qutebrowser config.py userscripts/*"
alias rci3="open_file $XDG_CONFIG_HOME/i3 config"
alias rcx11="open_file ${XINITRC%/*} xinitrc"
alias rcrofi="open_file $XDG_CONFIG_HOME/rofi config.rasi"
alias rcdunst="open_file $XDG_CONFIG_HOME/dunst dunstrc"
alias rcpolybar="open_file $XDG_CONFIG_HOME/polybar config.ini"
alias rcgpg="open_file $GNUPGHOME gpg-agent.conf"
alias rcssh="open_file $HOME ssh sshd_config"
alias rctmux="open_file $XDG_CONFIG_HOME/tmux tmux.conf"
alias rczathura="open_file $XDG_CONFIG_HOME/zathura zathurarc"
alias cron="open_file $XDG_CONFIG_HOME crons.cron; crontab $XDG_CONFIG_HOME/crons.cron"

# Open Emacs's config file in Emacs
alias rcemacs="emacs --file $XDG_CONFIG_HOME/emacs/init.el"

# sync notes
alias syncbooks="wait_internet && rclone sync $HOME/books books:books"

# Alternatives
alias top="btm --color=gruvbox"
alias cat='bat --pager=less --theme="gruvbox-dark"'

# Can't override journalctl using a function
alias journalctl='function journalctl_override(){ (command journalctl "$@" | lnav) }; journalctl_override'
alias logxorg="cat $HOME/.local/share/xorg/Xorg.0.log"
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

alias dotfiles="cd $HOME/dotfiles && magit"

alias check_audi_amount="cd $HOME/Projects/check-bank-acount && pipenv run python main.py"

# last installed packages
alias last='expac --timefmt="%Y-%m-%d %T" "%l\t%w\t%n" | grep explicit | sort | tail -n 20'
alias browse_packages="pacman -Qq | fzf --preview 'pacman -Qil {}' --layout=reverse --bind 'enter:execute(pacman -Qil {} | less)'"

# music player
alias cmus="screen -q -r -D cmus || screen -S cmus $(which cmus)"

alias abook="abook --config $XDG_CONFIG_HOME/abook/abookrc --datafile $XDG_DATA_HOME/addressbook"

# misc
alias get_mail='polybar-msg hook mail 2 && mailsync && polybar-msg hook mail 1'
alias myip='curl ifconfig.me; echo'
