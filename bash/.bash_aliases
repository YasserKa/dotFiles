# Shorter
alias vim='nvim'
alias vi='nvim'
alias v='nvim'
alias h='history'
alias hg='history | grep'
alias pac='sudo pacman'
alias z='zathura'
alias cdb='cd -'
alias ps?='ps aux | grep'
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

alias rcvim="rc $XDG_CONFIG_HOME/nvim init.vim"
alias rckitty="rc $XDG_CONFIG_HOME/kitty kitty.conf"
alias rcbash="rc $HOME .bashrc .bash_aliases .bash_functions .bash_profile"
alias rctmux="rc $HOME .tmux.conf"
alias rcqutebrowser="rc $XDG_CONFIG_HOME/qutebrowser config.py userscripts/*"
alias rci3="rc $XDG_CONFIG_HOME/i3 config"
alias rcrofi="rc $XDG_CONFIG_HOME/rofi config.rasi"
alias rcdunst="rc $XDG_CONFIG_HOME/dunst dunstrc"
alias rcpolybar="rc $XDG_CONFIG_HOME/polybar config.ini"
# Unique case
alias rcemacs="emacs --file $HOME/.emacs.d/init.el"

# sync notes
alias sync_org="wait_internet && rclone sync $HOME/notes/org remote:org --include 'fast_access.org' --include 'groceries.org'"
alias sync_books="wait_internet && rclone sync $HOME/books books:books"

# shutdown
# don't shutdown if there's an unsaved note file (checked via existence of symbolic linked file)
alias reboot="[ ! -h $HOME/notes/org/\.\#* ] && wait_internet && rclone sync $HOME/notes/org org_notes:org --include 'fast_access.org' --include 'groceries.org' && shutdown -r now || dunstify 'unsaved file'"
alias shut="[ ! -h $HOME/notes/org/\.\#* ] && wait_internet && rclone sync $HOME/notes/org org_notes:org --include 'fast_access.org' --include 'groceries.org' && shutdown now || dunstify 'unsaved file'"

# Alternatives
alias top='btm --color=gruvbox'
alias cat='bat --pager=less --theme="gruvbox-dark"'

# Colorful
alias ls="lsd"
alias lsa="ls --almost-all" # ignore . ..
alias l="ls --long"
alias la="ls -al"
alias lla="ls -Al --classify" # */=>@ indicators
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

# emacs git & notes
alias magit='i3-msg "workspace --no-auto-back-and-forth 4; exec emacs --funcall=magit-list-repositories"'
alias org='i3-msg "workspace --no-auto-back-and-forth 3; exec emacs --file=$HOME/notes/org/general.org"'
alias slack='i3-msg "workspace --no-auto-back-and-forth 7; exec slack"'

alias check_audi_amount="cd $HOME/Projects/check-bank-acount && pipenv run python main.py"

# cron
alias cron='$EDITOR $XDG_CONFIG_HOME/crons.cron; crontab $XDG_CONFIG_HOME/crons.cron'

# last installed packages
alias last='expac --timefmt="%Y-%m-%d %T" "%l\t%w\t%n" | grep explicit | sort | tail -n 20'

# music player
alias cmus='screen -q -r -D cmus || screen -S cmus $(which cmus)'

# misc
alias get_mail='polybar-msg hook mail 2 && mailsync && polybar-msg hook mail 1'
alias myip='curl ifconfig.me; echo'
