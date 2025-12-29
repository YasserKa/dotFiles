# shellcheck shell=bash
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

# Conform to XDG standards
alias wget=wget --hsts-file="$XDG_DATA_HOME/wget-hsts"

# Expand sudo aliases
alias sudo='sudo '
alias pls='sudo $(fc -ln -1)'

# Create directories and cd/edit immedietly
mkdircd() { command mkdir -pv "$1" && cd "$1" || exit; }
mkdirv() { mkdircd "${1%/*}" && nvim "${1##*/}" || exit; }

backrm() {
	local -r CURR_PWD="${PWD}"
	echo -n "Are you sure you want to remove '$CURR_PWD'? [y/N] "
	read -r confirm
	if [[ "$confirm" =~ ^[Yy]$ ]]; then
		cd .. && rm -rf "${CURR_PWD}" || exit
	fi
}

# Sync books
alias syncbooks='wait_internet && rclone sync $HOME/books books:books'

# Alternatives
alias top="btm --theme=gruvbox"
alias cat='bat'

# Logging
# grc aliases journalctl to add colourify
alias journalctl >/dev/null 2>&1 && unalias journalctl
# Can't override journalctl without using a function
journalctl() { command journalctl --follow "$@" | lnav; }
alias logxorg='cat $HOME/.local/share/xorg/Xorg.0.log'
# Doesn't work for dmesg
# Check https://github.com/tstack/lnav/issues/878

# Different options to search for files
# eza colors symbolic links better than lsd
# da=36 cyan timestamps
alias ls="EZA_COLORS='da=36' eza --time-style=relative --color-scale=age"
alias lsa="ls --almost-all"           # ignore . ..
alias l="ls --long --classify=always" # show file indicators
alias la="l --almost-all"
# Tree view
alias ltreea="ls --tree"
alias ltree="ltreea --level=2"
# Sort by time or size
alias lt="ls --long --sort=time"
alias lta="lt --almost-all"
# lsd is faster than eza
alias lss="lsd --long --total-size --sort=size --reverse"
alias lssa="lss --almost-all"

# More options
alias mkdir='mkdir -pv'
alias df='df -Tha --total'
alias cal='cal -m'
alias alsamixer='alsamixer -c 1'
alias grep='grep --color=auto'

# Reset fzf's options for libby to remove the bat preview
alias libby='FZF_DEFAULT_OPTS="--tac" libby'

# Prompt before overriding
alias cp='cp -i --preserve=all --reflink=auto'

# Navigation
alias ..='cd ..'
alias ...='cd ../..'

# Calculator
alias calc='rofi -show calc -modi calc -no-show-match -no-sort'

alias tmp="\${EDITOR} /tmp/tmp"
alias tmppy="\${EDITOR} /tmp/tmp.py"
alias tmptex="\${EDITOR} /tmp/tmp.tex"
alias tmpmd="\${EDITOR} /tmp/tmp.md"
alias tmpsh="\${EDITOR} /tmp/tmp.sh"
alias tmpbash="\${EDITOR} /tmp/tmp.bash"

# List last installed packages
alias last='expac --timefmt="%Y-%m-%d %T" "%l\t%w\t%n" | grep explicit | sort | tail -n 20'

# Music player
alias cmus='tmux attach-session -t cmus || tmux new-session -A -D -s cmus "$(type cmus | cut -d " " -f 3-)"'

# Generate Bibtex using citations in Tex file
alias gen_bib="command cp $XDG_CONFIG_HOME/Zotero/better-bibtex/my_library.bib bibliography.bib && biber --output-format=bibtex --output_file=bibliography.bib tex_output/*bcf"

alias show_keys="sudo showkey --keycodes"

# SSH setup
ssh() { command ssh "$@" -t 'export yasser_config_env=1; TERM=xterm-256color; bash -login'; }
mosh() { command mosh --ssh="ssh" "$@" -- bash -c 'export yasser_config_env=1; TERM=xterm-256color; exec bash -login'; }
ssh_config_setup() { make --file "$DOTFILES_DIR"/instance_setup/Makefile --keep-going move_config_to_server "host=$*"; }

# misc
alias testmail='echo | command neomutt -s "Testing mail" yasser.kaddoura19@gmail.com &> /dev/null'
# Activate virtual env
alias pdmshell='eval "$(pdm venv activate)"'
