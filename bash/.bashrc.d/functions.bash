# shellcheck shell=bash
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
			*.t@(gz|lz|xz|b@(2|z?(2))|a@(z|r?(.@(Z|bz?(2)|gz|lzma|xz))))) c=(bsdtar xvf) ;;
			*.7z) c=(7z x) ;;
			*.Z) c=(uncompress) ;;
			*.bz2) c=(bunzip2) ;;
			*.exe) c=(cabextract) ;;
			*.gz) c=(gunzip) ;;
			*.rar) c=(unrar x) ;;
			*.xz) c=(unxz) ;;
			*.zip) c=(unzip) ;;
			*.zst) c=(unzstd) ;;
			*)
				echo "$0: unrecognized file extension: \`$i'" >&2
				continue
				;;
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
	printf "%s\n" "Updating Vim packages, LSPs, formatters, etc."
	nvim --headless -c 'autocmd User PackerComplete quitall' -c 'MasonToolsUpdate' -c 'PackerSync'
	printf "%s\n" "Updating Emacs packages"
	# Update packages and exit afterwards
	emacs --no-window-system --eval "(progn
  (add-hook 'emacs-startup-hook #'(lambda () (interactive) (save-buffers-kill-emacs)))
  (auto-packages-update-now))"

	make --file="${HOME}/dotfiles/Makefile" upgrade-pypi-packages

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
	EXIT_CODE="$?"
	((EXIT_CODE != 0)) && return
	cd "$path" || exit 1
}

# Use fasd and FZF to jump through directories
vf() {
	paths=$(fasd -flR "$@")
	path="."
	# If only one path exists, go to it
	if [[ $(echo -e "$paths" | wc -l) == 1 ]]; then
		path=$paths
	else # Use fzf otherwise
		path=$(echo -e "$paths" | fzf --preview-window hidden --keep-right --height=20 --layout=reverse)
	fi
	EXIT_CODE="$?"
	((EXIT_CODE != 0)) && return
	"${EDITOR}" "$path" || exit 1
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

copyline() { printf %s "$READLINE_LINE" | pbcopy &>/dev/null; }
bind -m vi-insert -x '"\C-y":copyline'
bind -m vi-command -x '"\C-y":copyline'

# Use Alt-h to view documentation for commands
run_help() {
	local -r cmd="$READLINE_LINE"
	help "$cmd" 2>/dev/null || man "$cmd" 2>/dev/null || $cmd --help | $PAGER
}
bind -m vi-insert -x '"\eh": run_help'

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

	local -r DIRECTORY_PATH="$1"
	shift
	[[ ! -d "${DIRECTORY_PATH}" ]] && notify-send "${DIRECTORY_PATH} doesn't exist" && exit 1

	# Open one file only
	(($# > 1)) && ONLY_OPTION="+only"

	# Check if it's in a terminal
	if [[ "$-" != *c* ]]; then
		# The -o +only arguments are a hack to mitigate nvim's warning upon
		# exiting for editing multiple files
		# -o open files in windows and +only keep one of them
		# shellcheck disable=SC2046,SC2116
		cd "${DIRECTORY_PATH}" && "${EDITOR}" ${ONLY_OPTION} -o $(echo "$@")
	else
		# shellcheck disable=SC2046,SC2116
		"${TERMINAL}" --directory "${DIRECTORY_PATH}" --detach -e "${EDITOR}" ${ONLY_OPTION} -o "$@"
	fi
}

alias rcreadline='open_file $XDG_CONFIG_HOME/readline inputrc $XDG_CONFIG_HOME/readline/*'
alias rcgpg='open_file $GNUPGHOME gpg-agent.conf $GNUPGHOME/*'
alias rcssh='open_file $HOME/.ssh config $HOME/.ssh*'
alias rcx11='open_file ${XINITRC%/*} xinitrc ${XINITRC%/*}/*'
alias rcbash='open_file $HOME/.bashrc.d ../.bashrc ../.bash_profile ../.profile $HOME/.bashrc.d/*'
alias rckitty='open_file $XDG_CONFIG_HOME/kitty kitty.conf $XDG_CONFIG_HOME/kitty/*'
alias rcvim='open_file $XDG_CONFIG_HOME/nvim ./lua/user/init.lua'
alias rci3='open_file $XDG_CONFIG_HOME/i3 config $XDG_CONFIG_HOME/i3/*'
alias rcneomutt='open_file $XDG_CONFIG_HOME/neomutt neomuttrc'
alias rcmutt='open_file $XDG_CONFIG_HOME/neomutt neomuttrc'
alias rctuir='open_file $XDG_CONFIG_HOME/tuir tuir.cfg $XDG_CONFIG_HOME/tuir/*'
alias rcnewsboat='open_file $XDG_CONFIG_HOME/newsboat $XDG_CONFIG_HOME/newsboat/*'
alias rcfeh='open_file $XDG_CONFIG_HOME/feh keys $XDG_CONFIG_HOME/feh/*'
alias rcrofi='open_file $XDG_CONFIG_HOME/rofi config.rasi $XDG_CONFIG_HOME/rofi/*'
alias rcdunst='open_file $XDG_CONFIG_HOME/dunst dunstrc $XDG_CONFIG_HOME/dunst/*'
alias rcpolybar='open_file $XDG_CONFIG_HOME/polybar config.ini $XDG_CONFIG_HOME/polybar/*'
alias rctmux='open_file $XDG_CONFIG_HOME/tmux tmux.conf $XDG_CONFIG_HOME/tmux/*'
alias rczathura='open_file $XDG_CONFIG_HOME/zathura zathurarc $XDG_CONFIG_HOME/zathura/*'
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
