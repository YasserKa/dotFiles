# shellcheck shell=bash

#######################################
# Extand cd by making it accept a non-directory paramater as well
# Arguments:
#   $1 Path to file
#######################################
function cd() {
	while true ; do
  	case "$1" in
  		-)
  			builtin cd - && return;;
  		--)
  			shift; break ;;
  		*)
  			break;;
  	esac
 	done
	if [ $# -eq 0 ]; then
		builtin cd || exit
	elif [ -d "$1" ]; then
		# argument is a directory
		builtin cd "$1" || exit
	else
		# argument is not a directory
		cd "$(dirname "$1")" || exit
	fi
}

# https://github.com/xvoland/Extract/blob/master/extract.sh
function extract {
	if [ $# -eq 0 ]; then
		echo "Usage: extract <path/file_name>.<zip|rar|bz2|gz|tar|tbz2|tgz|Z|7z|xz|ex|tar.bz2|tar.gz|tar.xz|.zlib|.cso>"
		echo "       extract <path/file_name_1.ext> [path/file_name_2.ext] [path/file_name_3.ext]"
	fi
	for n in "$@"; do
		if [ ! -f "$n" ]; then
			echo "'$n' - file doesn't exist"
			return 1
		fi

		case "${n#*.}" in
			cbt | tar.bz2 | tar.gz | tar.xz | tbz2 | tgz | txz | tar)
				tar -pxvf "$n"
				;;
			lzma) unlzma ./"$n" ;;
			bz2) bunzip2 ./"$n" ;;
			cbr | rar) unrar x -ad ./"$n" ;;
			gz) gunzip ./"$n" ;;
			cbz | epub | zip) unzip ./"$n" ;;
			z) uncompress ./"$n" ;;
			7z | apk | arj | cab | cb7 | chm | deb | iso | lzh | msi | pkg | rpm | udf | wim | xar | vhd)
				7z x ./"$n"
				;;
			xz) unxz ./"$n" ;;
			exe) cabextract ./"$n" ;;
			cpio) cpio -id <./"$n" ;;
			cba | ace) unace x ./"$n" ;;
			zpaq) zpaq x ./"$n" ;;
			arc) arc e ./"$n" ;;
			cso) ciso 0 ./"$n" ./"$n.iso" &&
				extract "$n.iso" && \rm -f "$n" ;;
						zlib) zlib-flate -uncompress <./"$n" >./"$n.tmp" &&
							mv ./"$n.tmp" ./"${n%.*zlib}" && rm -f "$n" ;;
												dmg)
													hdiutil mount ./"$n" -mountpoint "./$n.mounted"
													;;
												*)
													echo "extract: '$n' - unknown archive method"
													return 1
													;;
											esac
										done
									}

# Remove dependencies that are no longer needed
orphans() {
	if [[ "$(pacman -Qdtt)" ]]; then
		sudo pacman -Qttdq | sudo pacman -Rns -
	else
		echo "no orphans to remove"
	fi
}

upgrade_system() {
	sudo -v
	orphans
	yes | paru --sync --refresh --sysupgrade --noconfirm

	printf "%s\n" "Updating Vim packages, LSPs, formatters, etc."
	nvim --headless -c 'autocmd User LazySync quitall' -c "AstroMasonUpdateAll" "+Lazy! sync"

	printf "%s\n" "Updating Emacs packages"
	emacsclient --eval "(progn
  (add-hook 'emacs-startup-hook #'(lambda () (interactive) (save-buffers-kill-emacs)))
  (auto-package-update-now))"

	# Upgrade python packages
	pipx upgrade-all

	pdm self update

	# Update Zsh plugins
	zap update all
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
	local paths
	paths=$(fasd -dlR "$@" | grep -v "$DOTFILES_DIR")
	local my_path="$HOME"
	# If only one path exists, go to it
	if [[ $(echo -e "$paths" | wc -l) == 1 ]]; then
		my_path=$paths
	else # Use fzf otherwise
		my_path=$(echo -e "$paths" | fzf --preview-window hidden --keep-right --height=20 --layout=reverse)
	fi
	EXIT_CODE="$?"
	((EXIT_CODE != 0)) && return
	cd "$my_path" || exit 1
}

# Use fasd and FZF to a open file and go to the directory it's in
vf() {
	local PATHS
	PATHS=$(fasd -flR "$@")
	local FILE_PATH
	FILE_PATH=""
	# If only one path exists, go to it
	NUMBER_FASD_FILES="$(echo -e "${PATHS}" | wc -l)"

	if [[ "$NUMBER_FASD_FILES" == 0 || "$PATHS" == "" ]]; then
		echo "Fasd isn't tracking '$*'"
		return 0
	elif ((NUMBER_FASD_FILES == 1)); then
		FILE_PATH="${PATHS}"
	else # Use fzf otherwise
		FILE_PATH=$(echo -e "${PATHS}" | fzf --preview-window hidden --keep-right --height=20 --layout=reverse)
		[[ -z $FILE_PATH ]] && return 0
	fi
	local EXIT_CODE
	EXIT_CODE="$?"
	((EXIT_CODE != 0)) && return
	cd "${FILE_PATH%/*}" || return 1
	"${EDITOR}" "${FILE_PATH}" || return 1
}

# fkill - kill processes - list only the ones you can kill. Modified the earlier script.
fps() {
	local pid
	if [ "$UID" != "0" ]; then
		pid=$(ps -f -u $UID | sed 1d | fzf -m | awk '{print $2}')
	else
		pid=$(ps -ef | sed 1d | fzf -m | awk '{print $2}')
	fi
}

# fkill - kill processes - list only the ones you can kill
fkill() {
	local pid
	if [ "$UID" != "0" ]; then
		pid=$(ps -f -u $UID | sed 1d | fzf -m | awk '{print $2}')
	else
		pid=$(ps -ef | sed 1d | fzf -m | awk '{print $2}')
	fi

	if [ "x$pid" != "x" ]; then
		echo "$pid" | xargs kill "-${1:-9}"
	fi
}

vpn_toggle() {
	local -r CON="wg0"

	if [[ $(nmcli connection show --active "$CON" | wc -c) -ne 0 ]]; then
		# shellcheck disable=2015
		nmcli connection down "$CON" && notify-send "vpn down" || notify-send "problem with vpn"
	else
		# shellcheck disable=2015
		nmcli connection up "$CON" && notify-send "vpn up" || notify-send "problem with vpn"
	fi
}

# Pick tmux sessions using FZF
fzftmux() {
	TMUX_SESSION=$(tmux list-sessions | cut -d: -f1 | fzf)
	[[ -z $TMUX_SESSION ]] && return
	tmux attach-session -t "$TMUX_SESSION"
}

# Edit last n lines in history using $EDITOR
fclast() { command fc "-${1}" 0; }

# Copy last command
cl() { history -p '!!' | tr -d \\n | pbcopy &>/dev/null; }

if [[ $- == *i* ]]; then
	if [[ "$BASH" ]]; then
		copyline() { printf %s "$READLINE_LINE" | pbcopy &>/dev/null; }
		bind -m vi-insert -x '"\C-y":copyline'
		bind -m vi-command -x '"\C-y":copyline'
	elif [[ "$ZSH_NAME" ]]; then
		# shellcheck disable=2317
		cmd_to_clip() { printf "%s" "$BUFFER" | xsel -i --clipboard; }
		zle -N cmd_to_clip
		bindkey -M vicmd '^y' cmd_to_clip
		bindkey -M viins '^y' cmd_to_clip
	fi
fi

# Use Alt-h to view documentation for commands
# Replicate bash
if [[ "$BASH" ]]; then
	run_help() {
		local -r cmd="$READLINE_LINE"
		# shellcheck disable=2046,2116
		help $(echo "$cmd") 2>/dev/null || man $(echo "$cmd") 2>/dev/null || $(echo "$cmd") --help | $PAGER
	}
	if [[ $- == *i* ]]; then
		bind -m vi-insert -x '"\eh": run_help'
	fi
elif [[ "$ZSH_NAME" ]]; then
	run_help() {
		local -r cmd="$BUFFER"
		# shellcheck disable=2046,2116
		man $(echo "$cmd") 2>/dev/null || $(echo "$cmd") --help | $PAGER
	}
	zle -N run_help
	bindkey '^[h' run_help
fi

# Attach job & send notification after it's finished
alert_last() {
	fg
	notify-send --expire-time=99999 "$(history | tail -n 2 | head -n 1 | cut -d ' ' -f 3-)"
}

# Open TUIR apps from menu picker (spawning a termianl) or the command line
open_cli() {
	local command="$1"

	[[ ! $(command -v "$command") ]] &&
		notify-send "$command doesn't exit" && return 127

	[[ $TERMINAL != "kitty" ]] && notify-send "$TERMINAL is not supported" && return 1

	# Command is run from a shell using -c option
	if [[ "$-" != *c* ]]; then
		command "$command"
	else
		$TERMINAL --detach -e bash -c "$command && exec $SHELL"
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
	(xdotool search --sync --name "^Front Page - tuir" key --clearmodifiers g t 2 &)
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

	local -r DIRECTORY_PATH="$DOTFILES_DIR/$1"
	shift
	[[ ! -d "${DIRECTORY_PATH}" ]] && notify-send "${DIRECTORY_PATH} doesn't exist" && exit 1

	# Open one file only
	(($# > 1)) && ONLY_OPTION="+only"

	# Check if it's in a terminal
	if [[ "$-" != *c* ]]; then
		# The -o +only arguments are a hack to mitigate nvim's warning upon
		# exiting for editing multiple files
		# -o open files in windows and +only keep one of them
		# shellcheck disable=SC2046,SC2116,SC2086
		cd "${DIRECTORY_PATH}" && "${EDITOR}" $ONLY_OPTION -o $(echo "$@")

	else
		# shellcheck disable=SC2046,SC2116,SC2086
		"${TERMINAL}" --directory "${DIRECTORY_PATH}" --detach -e "${EDITOR}" ${ONLY_OPTION} -o "$@"
	fi
}

alias rcreadline='open_file readline/.config/readline inputrc '
alias rcgpg='open_file gnupg/.local/share/gnupg gpg-agent.conf'
alias rcgit='open_file git/.config/git config'
alias rcssh='open_file ssh/.ssh config'
alias rcfzf='open_file fzf/.config/fzf config'
alias rcnavi='open_file navi/.local/share/navi/cheats/my__cheats my_cheats.cheat'
alias rcx11='open_file X11/.config/X11 xinitrc'
alias rcbash='open_file bash .bashrc .bash_profile .profile .bashrc.d/*'
alias rczsh='open_file zsh/.config/zsh .zshrc'
alias rckitty='open_file kitty/.config/kitty kitty.conf'
alias rcvim='open_file nvim/.config/nvim ./lua/plugins/user.lua'
alias rci3='open_file i3/.config/i3 config'
alias rcsway='open_file sway/.config/sway config'
alias rcwaybar='open_file waybar/.config/waybar config'
alias rcneomutt='open_file neomutt/.config/neomutt neomuttrc'
alias rcmutt='open_file neomutt/.config/neomutt neomuttrc'
alias rctuir='open_file tuir/.config/tuir tuir.cfg'
alias rcnewsboat='open_file newsboat/.config/newsboat config'
alias rcfeh='open_file feh/.config/feh keys'
alias rcrofi='open_file rofi/.config/rofi config.rasi'
alias rcdunst='open_file dunst/.config/dunst/ dunstrc'
alias rcpolybar='open_file polybar/.config/polybar config.ini'
alias rctmux='open_file tmux/.config/tmux tmux.conf'
alias rczathura='open_file zathura/.config/zathura zathurarc'
alias rcqutebrowser='open_file qutebrowser/.config/qutebrowser config.py'

# Open Emacs's config file in Emacs
rcemacs() { emacsclient --no-wait --socket-name="$EMACS_DEFAULT_SOCKET" --create-frame "$XDG_CONFIG_HOME/emacs/init.el"; }

rcdotfiles() {
	if [[ "$-" != *c* ]]; then
		cd "$DOTFILES_DIR/" || return
	else
		"${TERMINAL}" -e --directory "$DOTFILES_DIR/Makefile"
	fi
}
alias cron='vim $XDG_CONFIG_HOME/cron/crons.cron; crontab $XDG_CONFIG_HOME/cron/crons.cron'

goto_window() { timeout 3 xdotool search --sync --name "^$1$" windowactivate; }

is_window_exists() { xdotool search --name "^$1$" >/dev/null; }

#######################################
# Override emacsclient by running the server if it's not running in the background
#######################################
emacsclient() {
	ARGS=("$@")
	parsed=$(getopt --options=s: --longoptions=socket-name: --name "$0" -- "$@" 2>/dev/null)
	eval set -- "$parsed"

	SOCKET_NAME=
	while :; do
		case "$1" in
			-s | --socket-name)
				SOCKET_NAME="$2"
				break
				;;
			--)
				shift
				break
				;;
		esac
	done
	[[ ! "$SOCKET_NAME" ]] && SOCKET_NAME="$EMACS_DEFAULT_SOCKET" && ARGS+=("--socket-name=$EMACS_DEFAULT_SOCKET")

	# If server isn't running, run it
	if ! command emacsclient --socket-name="$SOCKET_NAME" -a false -e 't' >/dev/null 2>&1; then
		emacs --bg-daemon="$SOCKET_NAME"
		wait
	fi
	command emacsclient "${ARGS[@]}"
}
alias emacs="emacsclient --no-wait --create-frame --alternate-editor='' "

org() {
	local NAME="emacs_org"
	is_window_exists "$NAME" || emacsclient --no-wait --socket-name="$EMACS_ORG_SOCKET" --create-frame --frame-parameters='((title . "'"$NAME"'"))' -n -e '(progn (find-file "'"$NOTES_ORG_HOME/capture.org"'") (org-agenda nil "a") (delete-other-windows))'
	goto_window $NAME
}

# Open org notes without emacsclient to test config
_org() {
	command emacs --file="$NOTES_ORG_HOME/capture.org"
}

# Open daemonless Emacs
_emacs() {
	command emacs
}

magit() {
	local NAME="emacs_magit"
	local git_root
	git_root=$(git rev-parse --show-toplevel)
	chronic git rev-parse --show-toplevel || return 1

	is_window_exists "$NAME" ||
		emacsclient --no-wait --socket-name="$EMACS_DEFAULT_SOCKET" --create-frame --frame-parameters '((title . "'"$NAME"'"))' --eval '(magit-status "'"$git_root"'")' >/dev/null
			goto_window $NAME

		}

		alias gitdotfiles='cd $DOTFILES_DIR && magit'

		syncorg() {
			emacsclient --no-wait --socket-name="$EMACS_ORG_SOCKET" --eval "(org-save-all-org-buffers)" 2>/dev/null
			"$HOME"/bin/wait_internet && rclone sync "${NOTES_ORG_HOME}" org_notes:org \
				--filter '- .git/' --filter '- images/' --filter '- ltximg/' --filter '+ groceries.org' --filter '+ fast_access.org' --filter '- *' \
				||	notify-send --urgency=critical "Sync org not working" 
			}

# Pick a color and store it in clipbaord
pick_color() {
	command -v gpick >/dev/null && gpick -so | pbcopy
}

reboot() {
	if "$HOME/bin/wait_internet"; then
		syncorg
	else
		[[ $(dunstify "No internet connection" "Reboot without <b>syncorg</b>" --action="action,label") == "action" ]] || return 1
	fi
	command shutdown --reboot now
}

shutdown() {
	if "$HOME/bin/wait_internet"; then
		syncorg
	else
		[[ $(dunstify "No internet connection" "Shutdown without <b>syncorg</b>" --action="action,label") == "action" ]] || return 1
	fi
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

# Yay install and uninstall {{{
# Helper function to integrate paru and fzf
pzf() {
  (($#==0)) && echo "This is a helper function, use pai or par instead" && return
  # Position of the value in each candidate
  pos=$1
  AUR_URL='https://aur.archlinux.org/packages'
  OFFICIAL_URL='https://archlinux.org/packages'
  shift
  sed "s/ /\t/g" \
  	| fzf --ansi --nth="$pos" --multi --history="${FZF_HISTDIR:-$XDG_STATE_HOME/fzf}/history-pzf" \
    --preview-window=60%,border-left \
		--bind="ctrl-o:execute(xdg-open \$(paru -Si {$pos} | grep URL | head -1 | awk '{print \$NF}') 2>/dev/null)" \
		--bind="alt-o:execute(2>/dev/null { pacman -Si {$pos} &&  xdg-open '$OFFICIAL_URL/{$pos}' || xdg-open '$AUR_URL?K={$pos}&SB=p&SO=d&PP=100'; })" \
		--header 'C-o: Upstream URL, A-o: Official or AUR URL' \
    "$@" | cut -f"$pos" | xargs
	}

# List installable packages into fzf and install selection
pai() {
  cache_dir="/tmp/pas-$USER"
  mkdir -p "$cache_dir"
  preview_cache="$cache_dir/preview_{2}"
  list_cache="$cache_dir/list"
  { test "$(wc -l <"$list_cache$*")" -lt 50000 && rm "$list_cache$*"; 
  } 2>/dev/null

  pkg=$( (cat "$list_cache$*" 2>/dev/null || { pacman --color=always -Sl "$@"; paru --color=always -Sl aur "$@"; } \
  	| sed 's/ [^ ]*unknown-version[^ ]*//' | tee "$list_cache$*") \
  	| pzf 2 --tiebreak=index --preview="cat $preview_cache 2>/dev/null | grep -v 'querying' | grep . || paru --color always -Si {2} | tee $preview_cache")

  if test -n "$pkg"
  then echo "installing $pkg..."
    cmd="paru -S $pkg"
		# Add a shell history entry
    print -s "$cmd"
    eval "$cmd"
    rehash
    rm -rf "$cache_dir"
  fi
}

# list installed packages into fzf and remove selection
# tip: use -e to list only explicitly installed packages
par() {
  pkg=$(paru --color=always -Q "$@" | pzf 1 --tiebreak=length --preview="paru --color always -Qli {1}")
  if test -n "$pkg"
  then echo "removing $pkg..."
    cmd="paru -r --cascade --recursive $pkg"
    print -s "$cmd"
    eval "$cmd"
  fi
}

alias pas="pacman -Qq | pzf 1 --preview 'pacman -Qil {}' --bind 'enter:execute(pacman -Qil {} | \$PAGER)+abort'"
# }}}

# Commands run in background automatically
zathura() { (command zathura "$@" &>/dev/null &) }
mpv() { (command mpv "$@" &>/dev/null &) }
xdg-open() { (command xdg-open "$@" &>/dev/null); }
pcmanfm() { (command pcmanfm "$@" &) }
