# shellcheck shell=bash
#
# Dependencies: jq

#######################################
# Extand cd by making it accept a non-directory paramater as well
# Arguments:
#   $1 Path to file
#######################################
function cd() {
	while true; do
		case "$1" in
			-)
				builtin cd - && return
				;;
			--)
				shift
				break
				;;
			*)
				break
				;;
		esac
	done
	if [ $# -eq 0 ]; then
		builtin cd || exit
	elif [ -d "$1" ]; then
		# argument is a directory
		builtin cd "$1" || exit
	elif [ -e "$1" ]; then
		# argument is not a directory
		builtin cd "$(dirname "$1")" || exit
	else
		echo "cd: No such file or directory" >&2
	fi
}

#######################################
# Attempts to close a window with in a time interval
# Arguments:
#   $1 Word to identify the window (e.g. Slack)
#######################################
close_window() {
	local -r TIMEOUT=5
	local SECONDS=0
	while sleep 1; do
		id="$(wmctrl -l | grep "$1" | cut -d ' ' -f -1)"
		[[ -n "$id" ]] && wmctrl -ic "$id" && exit 0
		((SECONDS >= TIMEOUT)) && break
	done
}

#######################################
# Extand mv by creating the target directory if it doesn't exist
#######################################
mv() {
	last_arg="${*: -1}"

	if [[ "$#" -gt 2 ]]; then
		parent_dir="$last_arg"
	# The last arg is the directory if it ends with /
	elif [[ ${last_arg: -1} == "/" ]]; then
		parent_dir="$last_arg"
	else
		parent_dir="$(dirname "$last_arg")"
	fi

	# If destination exists, proceed normally
	if [[ ! -d "$parent_dir" ]]; then
		echo -n "Destination '$parent_dir' does not exist. Create directory? [y/N] "
		read -r confirm
		if [[ "$confirm" =~ ^[Yy]$ ]]; then
			mkdir -p "$parent_dir"
		else
			return 1
		fi
	fi
	command mv -i "$@"
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

		case "${n}" in
			*.cbt | *.tar.bz2 | *.tar.gz | *.tar.xz | *.tbz2 | *.tgz | *.txz | *.tar)
				tar --auto-compress -xvf "$n"
				;;
			*.lzma) unlzma "$n" ;;
			*.bz2) bunzip2 "$n" ;;
			*.cbr | *.rar) unrar x -ad "$n" ;;
			*.gz) gunzip "$n" ;;
			*.cbz | *.epub | *.zip) unzip "$n" ;;
			*.z) uncompress "$n" ;;
			*.7z | *.apk | *.arj | *.cab | *.cb7 | *.chm | *.deb | *.iso | *.lzh | *.msi | *.pkg | *.rpm | *.udf | *.wim | *.xar | *.vhd)
				7z x "$n"
				;;
			*.xz) unxz "$n" ;;
			*.exe) cabextract "$n" ;;
			*.cpio) cpio -id <"$n" ;;
			*.cba | *.ace) unace x "$n" ;;
			*.zpaq) zpaq x "$n" ;;
			*.arc) arc e "$n" ;;
			*.cso) ciso 0 "$n" "$n.iso" && extract "$n.iso" && rm -f "$n" ;;
			*.zlib) zlib-flate -uncompress <"$n" >"${n%.*zlib}" && rm -f "$n" ;;
			*.dmg)
				mnt_dir=$(mktemp -d)
				hdiutil mount "$n" -mountpoint "$mnt_dir"
				echo "Mounted at: $mnt_dir"
				;;
			*.tar.zst) tar -I zstd -xvf "$n" ;;
			*.zst) zstd -d "$n" ;;
			*)
				echo "extract: '$n' - unknown archive method"
				continue
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

# Used for tmuxp
# Can't be used directly in tmuxp because of how YAML processes the single quotes
update_emacs() {
	emacs
	org

	# Updating treesitters
	emacsclient --socket-name="$EMACS_ORG_SOCKET" --eval "(mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))"
	# Open Emacs for each server to track status

	emacs_update_code="
	(if (featurep 'straight)
  (straight-pull-all)))"
	emacsclient --socket-name="$EMACS_ORG_SOCKET" --eval "$emacs_update_code" &
	disown
	emacsclient --socket-name="$EMACS_DEFAULT_SOCKET" --eval "$emacs_update_code" &
	disown
	command emacs --init-directory "$XDG_CONFIG_HOME/emacs" --eval="$emacs_update_code" &
	disown
}
alias upgrade_system="tmuxp load system_upgrade"

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
	local paths my_path

	paths="$(fasd -dlR "$@" | grep -v "$DOTFILES_DIR")"
	my_path="$(
		echo -e "$paths" | fzf --select-1 --preview-window hidden --keep-right --height=20 --layout=reverse \
			--bind "ctrl-alt-d:execute-silent(fasd --delete '{}')+reload(fasd -dlR '$*' | grep -v $DOTFILES_DIR)"
	)" || return 2
	[[ ! "$my_path" ]] &&
		{ my_path="$(eval "$FZF_ALT_C_COMMAND" | FZF_DEFAULT_OPTS="--reverse --walker=dir,follow,hidden --scheme=path --query '$*' ${FZF_ALT_C_OPTS:-} +m " fzf)" || return 2; }
	fasd -A "$my_path"
	cd "$my_path" || return 1
}

# Use fasd and FZF to a open file and go to the directory it's in
vf() {
	local file_path

	paths="$(fasd -flR "$@")"
	file_path="$(
		echo -e "$paths" | fzf --select-1 --preview-window hidden --keep-right --height=20 --layout=reverse \
			--bind "ctrl-alt-d:execute-silent(fasd --delete '{}')+reload(fasd -flR '$*')"
	)" || return 2
	[[ ! "$file_path" ]] &&
		{ cd "$HOME" && file_path="$(eval "$FZF_CTRL_T_COMMAND" | FZF_DEFAULT_OPTS="--reverse --walker=file,follow,hidden --scheme=path --query '$*' ${FZF_CTRL_T_OPTS:-}" fzf)" || return 2; }
	cd "${file_path%/*}" || return 2
	fasd -A "${file_path##*/}"
	"${EDITOR}" "${file_path##*/}" || return 1
}

# Kill processes
fkill() {
	local -r pid="$(fps)"

	if [[ "$pid" ]]; then
		echo "$pid" | xargs kill "-${1:-9}"
	fi
}

# Copy error code to clipboard
ferrno() {
	local -r err_list="$(errno --list | awk '{$1 = "\033[37;2m" $1 "\033[0m"; $2 = "\033[1;32m" $2 "\033[0m"; print }')"
	echo "$err_list" | fzf --ansi --bind "enter:execute-silent(clipboard_copy {2})+abort"
}

# Show country, region, ISP, IPv4, and IPv6
myip() {
	CYAN='\033[0;36m'
	YELLOW='\033[1;33m'
	curl -s 'http://ip-api.com/json?fields=message,country,city,isp,query' | jq -r '. | to_entries[] | if .key == "query" then "\u001b[0;36mIPv4:\u001b[1;33m \(.value)\u001b[0m" else "\u001b[0;36m\(.key):\u001b[1;33m \(.value)\u001b[0m" end'
	echo -e "${CYAN}IPv6:${YELLOW} $(curl -s https://v6.ident.me)"
}

vpn_toggle() {
	if tailscale exit-node list | grep selected; then
		tailscale set --exit-node=
	else
		tailscale set --exit-node=remote
	fi

	(($? == 1)) && notify-send "VPN not functional"
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
cl() { history -p '!!' | tr -d \\n | clipboard_copy; }

if [[ $- == *i* ]]; then
	if [[ "$BASH" ]]; then
		copyline() { printf %s "$READLINE_LINE" | clipboard_copy &>/dev/null; }
		bind -m vi-insert -x '"\C-y":copyline'
		bind -m vi-command -x '"\C-y":copyline'
	elif [[ "$ZSH_NAME" ]]; then
		# shellcheck disable=2317
		cmd_to_clip() { printf "%s" "$BUFFER" | clipboard_copy &>/dev/null; }
		zle -N cmd_to_clip
		bindkey -M vicmd '^y' cmd_to_clip
		bindkey -M viins '^y' cmd_to_clip
	fi
fi

# Use Alt-h to view documentation for commands
# Replicate bash
if [[ "$BASH" ]]; then
	run_help() {
		local -r cmd="${READLINE_LINE%% *}"
		# shellcheck disable=2046,2116
		help $(echo "$cmd") 2>/dev/null || man $(echo "$cmd") 2>/dev/null || $(echo "$cmd") --help | $PAGER
	}
	if [[ $- == *i* ]]; then
		bind -m vi-insert -x '"\eh": run_help'
	fi
elif [[ "$ZSH_NAME" ]]; then
	run_help() {
		# This accomadates git push --help
		for count in {2,1}; do
			read -r cmd < <(cut -d ' ' -f -"$count" <(echo "$BUFFER"))
			# shellcheck disable=2046,2116
			{ man $(echo "$cmd") 2>/dev/null || $(echo "$cmd") --help || [[ "$(help $(echo "$cmd") 2>&1)" != *"No manual entry for $cmd"* ]]; } &>/dev/null || continue
			# shellcheck disable=2046,2116
			man $(echo "$cmd") 2>/dev/null || $(echo "$cmd") --help || help $(echo "$cmd") | $PAGER && return 0
		done
	}
	zle -N run_help
	bindkey '^[h' run_help

	encode_url() { printf %s "$1" | jq -sRr @uri; }

	explainshell() {
		local -r cmd="$BUFFER"

		xdg-open "https://explainshell.com/explain?cmd=$(encode_url "$cmd")"
	}
	zle -N explainshell
	bindkey '^[e' explainshell
fi

# Attach job & send notification after it's finished
alert_last() {
	fg
	local last_cmd="Process finished"
	if [[ "$BASH" ]]; then
		last_cmd="$(history | tail -n 2 | head -n 1 | cut -d ' ' -f 5-)"
	elif [[ "$ZSH_NAME" ]]; then
		last_cmd="$(history -1 | cut -d ' ' -f 4-)"
	fi
	notify-send --expire-time=99999 "$last_cmd"
}

alert() {
	local cmd="Process finished"
	if [[ "$BASH" ]]; then
		cmd="$(history | tail -n1)"
	elif [[ "$ZSH_NAME" ]]; then
		cmd="$(history "$HISTCMD" | cut -d ' ' -f 4-)"
	fi
	# Remove alert
	cmd="$(sed -e '''s/^\s*[0-9]\+\s*//;s/[;&|]*\s*alert$//''' <(echo "$cmd"))"
	notify-send --expire-time=99999 "$cmd"
}

# Open TUIR apps from menu picker (spawning a termianl) or the command line
open_cli() {
	local command="$1"
	shift

	[[ ! $(command -v "$command") ]] &&
		notify-send "$command doesn't exit" && return 127

	! command -v "$TERMINAL" &>/dev/null && notify-send "$TERMINAL is not supported" && return 1

	eval_interactive_cmd "$command" "$@"
}

cli_list=("newsboat" "neomutt")

for cli in "${cli_list[@]}"; do
	# shellcheck disable=SC2139
	alias "$cli=open_cli $cli"
done
unset cli

# Open TUIR with top page within 24 hours by pressing "g t 2"
tuir() {
	if [[ -n "${WAYLAND_DISPLAY}" ]]; then
		(timeout 30 sh -c 'while ! swaymsg -t get_tree | jq -e "..|.name?|select(. != null and test(\"Front Page - tuir\"))" >/dev/null; do sleep 0.1; done' && \
  		swaymsg "[title=\"Front Page - tuir\"]" focus && \
  		sleep 0.1 && \
  		ydotool key 34:1 34:0 20:1 20:0 3:1 3:0) &
	elif [[ -n "${DISPLAY}" ]]; then
		(xdotool search --sync --name "^Front Page - tuir" key --clearmodifiers g t 2 &)
	fi
	open_cli tuir
}

################################################################################
# Open file/s in a directory depending on the interactivity of the shell
# Arguments:
#     $1: path of directory
#     $@: file name
################################################################################
open_file() {
	! command -v "$TERMINAL" &>/dev/null &&
		notify-send "$TERMINAL not supported for open_file function" && return 1

	local -r DIRECTORY_PATH="$DOTFILES_DIR/$1"
	shift
	[[ ! -d "${DIRECTORY_PATH}" ]] && notify-send "${DIRECTORY_PATH} doesn't exist" && exit 1

	# Open one file only
	(($# > 1)) && ONLY_OPTION="+only"

	eval_interactive_cmd "cd ${DIRECTORY_PATH} && ${EDITOR} $ONLY_OPTION -o $*"
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
alias rcwaybar='open_file waybar/.config/waybar config.jsonc'
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

is_window_exists() {
	i3-msg -t get_tree |
	jq -e --arg re "$1" '
  	recurse(.nodes[]?, .floating_nodes[]?) |
  	select(
    	((.name // "") | test($re)) or
    	((.app_id // "") | test($re)) or
    	((.window_properties.class // "") | test($re))
  	)
	' >/dev/null
}

wait_window() {
	local TIMEOUT=5 START=$SECONDS
  while ! is_window_exists "$1"; do
    (( SECONDS - START >= TIMEOUT )) && return 1
    sleep 0.1
  done
}

wait_window_exit() {
	local TIMEOUT=999 START=$SECONDS
  while is_window_exists "$1"; do
    (( SECONDS - START >= TIMEOUT )) && return 1
    sleep 0.1
  done
}

window_get_condition() {
	# Get the condition (app_id, class, name) to use for focusing the window
	CRITERIA="$(i3-msg -t get_tree | jq -r --arg re "$1" '
  	recurse(.nodes[]?, .floating_nodes[]?) |
  	if ((.app_id // "") | test($re)) then "app_id"
  	elif ((.window_properties.class // "") | test($re)) then "class"
  	elif ((.window_properties.title // "") | test($re)) then "title"
  	elif ((.name // "") | test($re)) then "name"
  	else empty end')"
	echo "[${CRITERIA}=\"$1\"]"
}

goto_window() {
	wait_window "$1"
	CONDITION="$(window_get_condition "$1")"
	i3-msg "$CONDITION focus"
}

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
	local NAME="^emacs_org$"
	is_window_exists "$NAME" || {
		emacsclient --no-wait --socket-name="$EMACS_ORG_SOCKET" --create-frame --frame-parameters='((title . "'"$NAME"'"))' -e '(progn (find-file "'"$NOTES_ORG_HOME/capture.org"'") (org-agenda nil "a") (delete-other-windows) (load-file (concat user-emacs-directory "/init.el")))'
	}
goto_window "$NAME"
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

	is_window_exists "$NAME" || emacsclient --no-wait --socket-name="$EMACS_DEFAULT_SOCKET" --create-frame --frame-parameters '((title . "'"$NAME"'"))' --eval '(magit-status "'"$git_root"'")' >/dev/null
	goto_window $NAME
}

alias gitdotfiles='cd $DOTFILES_DIR && magit'

elfeed() {
	emacs --eval --create-frame "(progn (elfeed) (elfeed-update))"
}

# Pick a color and store it in clipbaord
pick_color() {
	command -v gpick >/dev/null && clipboard_copy "$(gpick -so)"
}

reboot() {
	if wait_internet; then
		push_files_to_remote || exit 0
	else
		[[ $(dunstify --urgency=critical "No internet connection" "Reboot without <b>syncorg</b>" --action="action,label") == "action" ]] || return 1
	fi
	command shutdown --reboot now
}

create_jupytext_pair() {
	echo "# %%" >"$1.py" && jupytext --to ipynb "$1.py"
}

shutdown() {
	if wait_internet; then
		push_files_to_remote || exit 1
		git_check
	else
		[[ $(dunstify --urgency=critical "No internet connection" "Shutdown without <b>syncorg</b>" --action="action,label") == "action" ]] || return 1
	fi
	command shutdown now
}

# Which is unreliable, use type -P instead
# https://unix.stackexchange.com/questions/85249/why-not-use-which-what-to-use-then
which() {
	printf >&2 'The which command is unreliable. Use command -v %s\n' "${@[-1]}"
	return 2
}

find_pattern_in_dotfiles() {
  [[ $# -gt 0 ]] || return 1
	rg --hidden --no-ignore --glob '!**/zsh/history' -- "$1" "$HOME/.dotfiles" "$HOME/.dotfiles-private"
}
# Yay install and uninstall {{{
# Helper function to integrate paru and fzf
pzf() {
	(($# == 0)) && echo "This is a helper function, use pai or par instead" && return
	# Position of the value in each candidate
	pos=$1
	AUR_URL='https://aur.archlinux.org/packages'
	OFFICIAL_URL='https://archlinux.org/packages'
	shift
	sed "s/ /\t/g" |
		fzf --ansi --nth="$pos" --multi --history="${FZF_HISTDIR:-$XDG_STATE_HOME/fzf}/history-pzf" \
		--preview-window=60%,border-left \
		--bind="ctrl-o:execute-silent(xdg-open \$(paru -Si {$pos} | grep URL | head -1 | awk '{print \$NF}') 2>/dev/null)" \
		--bind="alt-o:execute-silent(&>/dev/null { pacman -Si {$pos} &&  xdg-open '$OFFICIAL_URL/{$pos}' || xdg-open '$AUR_URL?K={$pos}&SB=p&SO=d&PP=100'; })" \
		--header 'C-o: Upstream URL, A-o: ArchLinux.org' \
		"$@" | cut -f"$pos" | xargs
	}

	# List installable packages into fzf and install selection
	pai() {
		cache_dir="/tmp/pas-$USER"
		mkdir -p "$cache_dir"
		preview_cache="$cache_dir/preview_{2}"
		list_cache="$cache_dir/list"
		{
			test "$(wc -l <"$list_cache$*")" -lt 50000 && rm "$list_cache$*"
		} 2>/dev/null

	pkg=$( (cat "$list_cache$*" 2>/dev/null || {
		pacman --color=always -Sl "$@"
			paru --color=always -Sl aur "$@"
		} |
			sed 's/ [^ ]*unknown-version[^ ]*//' | tee "$list_cache$*") |
			pzf 2 --tiebreak=index --preview="cat $preview_cache 2>/dev/null | grep -v 'querying' | grep . || paru --color always -Si {2} | tee $preview_cache")

		if test -n "$pkg"; then
			echo "installing $pkg..."
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
		if test -n "$pkg"; then
			echo "removing $pkg..."
			cmd="paru --remove --nosave --recursive $pkg"
			print -s "$cmd"
			eval "$cmd"
		fi
	}

alias pas="pacman -Qq | pzf 1 --preview 'pacman -Qil {}' --bind 'enter:execute(pacman -Qil {} | \$PAGER)+abort'"
# }}}

# Commands run in background automatically
zathura() { (command zathura "$@" &>/dev/null &) }
mpv() { (command mpv --input-ipc-server=/tmp/mpv-socket --no-terminal "$@" &>/dev/null &) }
xdg-open() {
	for file in "$@"; do
		(command xdg-open "$file" &>/dev/null &)
	done
}
pcmanfm() { (command pcmanfm "$@" &) }
# Open thunderbird window if it doesn't exist, else move it to current workspace
thunderbird() { { wmctrl -l | grep Thunderbird; } && i3-msg '[class="thunderbird"] move workspace current, focus' || command thunderbird & }
zotero() {
	CONDITION=
	is_window_exists "^Zotero$" && i3-msg "$(window_get_condition "^Zotero$") move workspace current, focus" && exit 0
	command zotero &
}
