# shellcheck shell=bash
# Shorter
# alias v='nvim'
#######################################
# If directories doesn't exist, create them after accepting a prompt
# Arguments:
#   $1: file path
#######################################
v() {
	FILE_PATH="$1"

	# Extract directory path from the file path
	DIR_PATH="$(dirname "$FILE_PATH")"

	# Check if the directory exists, if not, create it after accepting a prompt
	if [[ ! -d "$DIR_PATH" ]]; then

		QUESTION=" $DIR_PATH path doesn't exist, create it? (y/n) "
		if [[ "$BASH" ]]; then
			read -rp "$QUESTION"T CHOICE
		elif [[ "$ZSH_NAME" ]]; then
			read -rq "CHOICE?$QUESTION"
		fi

		[[ "$CHOICE" != "y" ]] && return 0

		mkdir -p "$DIR_PATH"
	fi

	nvim "$FILE_PATH"
}

alias vi='v'
alias vim='v'
alias h='history'
alias hg='history | grep'

alias pac='sudo pacman'
alias parin='paru --sync --skipreview'
alias pacin='sudo pacman --sync'
alias z='zathura'
alias b='~-'
alias vimdiff='nvim -d'

# Expand sudo aliases
alias sudo='sudo '
alias pls='sudo $(fc -ln -1)'

# Create directories and cd/edit immedietly
mkdircd() { command mkdir -pv "$1" && cd "$1" || exit; }
mkdirv() { mkdircd "${1%/*}" && nvim "${1##*/}" || exit; }

backrm() {
	CURR_PWD="${PWD}"
	cd .. && rm -rf "${CURR_PWD}" || exit
}

# Sync books
alias syncbooks='wait_internet && rclone sync $HOME/books books:books'

# Alternatives
alias top="btm --color=gruvbox"
alias cat='bat --pager=less --theme="gruvbox-dark"'

# Logging
# grc aliases journalctl to add colourify
alias journalctl >/dev/null 2>&1 && unalias journalctl
# Can't override journalctl without using a function
journalctl() { command journalctl "$@" | lnav; }
alias logxorg='cat $HOME/.local/share/xorg/Xorg.0.log'
# Doesn't work for dmesg
# Check https://github.com/tstack/lnav/issues/878

# Different options to search for files
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

# Reset fzf's options for libby to remove the bat preview
alias libby='FZF_DEFAULT_OPTS="--tac" libby'

# Prompt before overriding
alias mv='mv -i'
alias cp='cp -i --preserve=all --reflink=auto'

# Clipboard
# xclip doesn't close stdout nor stderr, leading for the
alias pbcopy='xclip -selection clipboard >/dev/null 2>&1'
alias pbpaste='xclip -selection clipboard -o >/dev/null 2>&1'

# Navigation
alias ..='cd ..'
alias ...='cd ../..'

# Calculator
alias calc='rofi -show calc -modi calc -no-show-match -no-sort'

# Alert after long commands
alias lert='notify-send --expire-time=99999 "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

alias tmp="\${EDITOR} /tmp/tmp"
alias tmppy="\${EDITOR} /tmp/tmp.py"
alias tmptex="\${EDITOR} /tmp/tmp.tex"
alias tmpmd="\${EDITOR} /tmp/tmp.md"
alias tmpsh="\${EDITOR} /tmp/tmp.sh"

# List last installed packages
alias last='expac --timefmt="%Y-%m-%d %T" "%l\t%w\t%n" | grep explicit | sort | tail -n 20'
alias browse_packages="pacman -Qq | fzf --preview 'pacman -Qil {}' --layout=reverse --bind 'Ctrl-j:execute(pacman -Qil {} | \$PAGER)+abort'"

# Music player
alias cmus='screen -q -r -D cmus || screen -S cmus $(which cmus)'

# SSH setup
# shellcheck disable=SC2048,SC2086
ssh() { command ssh $* -t 'export yasser_config_env=1; TERM=xterm-256color; bash -login'; }
ssh_config_setup() { make --file ~/dotfiles/instance_setup/Makefile --keep-going move_config_to_server "host=$*"; }

# misc
alias get_mail='polybar-msg hook mail 2 && syncmail && polybar-msg hook mail 1'
alias testmail='echo | command neomutt -s "Testing mail" yasser.kaddoura19@gmail.com &> /dev/null'
alias myip='curl ifconfig.me; echo'
# Activate virtual env
alias pdmshell='eval "$(pdm venv activate)"'
