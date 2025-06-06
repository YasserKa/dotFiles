# shellcheck shell=bash
# Source if the shell is interactive
# shellcheck disable=SC1091,SC2034,SC1090,SC2155,SC2016
[[ $- != *i* ]] && return

# Bash options https://www.gnu.org/software/bash/manual/html_node/The-Shopt-Builtin.html
set -o vi           # Vi mode
set -o noclobber    # Don't overwrite when redireting using shell
shopt -s autocd     # Auto cd without using cd
shopt -s histappend # Append to the history file, don't overwrite it
shopt -s cdspell    # Fix spelling errors when using cd
shopt -s dirspell   # Fix spelling errors during tab-completion
shopt -s extglob    # extend pattern matching
shopt -s cmdhist    # History records commands with multiple lines as such instead of using ";"
stty -ixon          # Enable search C-s for history searching (Enable XON/XOFF flow control)

# Override Zsh's History path
export HISTFILE=$HOME/.bash_history

source_file() {
	[[ -f "$1" ]] || exit
	# shellcheck source=/dev/null
	source "$1"
}

# Complete command names and file names for super user
complete -cf sudo
source_file /usr/share/doc/pkgfile/command-not-found.bash
# Gives autocompletion for options
# https://github.com/scop/bash-completion
source_file /usr/share/bash-completion/bash_completion

source_file "$XDG_CONFIG_HOME/tmux/tmux_completion"

for bash in "$HOME"/.bash_completion.d/*.bash; do
	source_file "$bash"
done
unset -v bash

if [ -n "$SSH_CONNECTION" ] || [ -n "$SSH_TTY" ]; then
	if [[ -e $XDG_CONFIG_HOME/git/git-prompt.sh ]]; then
		# https://www.git-scm.com/book/en/v2/Appendix-A%3A-Git-in-Other-Environments-Git-in-Bash
		. "$XDG_CONFIG_HOME/git/git-prompt.sh"
		export GIT_PS1_SHOWDIRTYSTATE=1
		PS1='\[$(tput bold)\]\[\e[36m\]\w\[\033[38;5;87m\] $(__git_ps1 "(%s)")\n\[\033[38;5;34m\]\u@\h: \[\e[m\]'
		PS1='\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
	else
		git_branch() { git branch 2>/dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ \1)/'; }
		PS1='\[$(tput bold)\]\[\e[36m\]\w\[\033[38;5;87m\] $(git_branch)\n\[\033[01;32m\]\u@\h\[\033[00m\]\[\033[01;34m\] \[\033[38;5;34m\]❯ \[\e[m\]'
	fi
else
	# Prompt with info about git repo
	if [[ -e $XDG_CONFIG_HOME/git/git-prompt.sh ]]; then
		# https://www.git-scm.com/book/en/v2/Appendix-A%3A-Git-in-Other-Environments-Git-in-Bash
		source_file "$XDG_CONFIG_HOME/git/git-prompt.sh"
		export GIT_PS1_SHOWDIRTYSTATE=1
		PS1='\[$(tput bold)\]\[\e[36m\]\w\[\033[38;5;87m\] $(__git_ps1 "(%s)")\n\[\033[38;5;34m\]❯ \[\e[m\]'
	else
		git_branch() { git branch 2>/dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ \1)/'; }
		PS1='\[$(tput bold)\]\[\e[36m\]\w\[\033[38;5;87m\] $(git_branch)\n\[\033[38;5;34m\]❯ \[\e[m\]'
	fi
fi

# Setup autojumping
# https://github.com/clvv/fasd
if command -v fasd >/dev/null; then
	fasd_cache="$HOME/.fasd-init-bash"
	# Cache the code for faster sourcing time
	if [[ ! -s "$fasd_cache" ]]; then
		fasd --init posix-alias bash-hook bash-ccomp bash-ccomp-install >|"$fasd_cache"
	fi
	source_file "$fasd_cache"
	unset -v fasd_cache

	_fasd_bash_hook_cmd_complete j
fi

# Commands with colors
# https://github.com/garabik/grc
if command -v grc >/dev/null; then
	GRC_ALIASES=true
	source_file /etc/profile.d/grc.sh
fi

if command -v fzf >/dev/null; then
	source_file /usr/share/fzf/completion.bash
	source_file /usr/share/fzf/key-bindings.bash

	# For C-r history search, make <tab> edit selected option and <enter> execute it
	FZF_CTRL_R_EDIT_KEY=ctrl-e
	FZF_CTRL_R_EXEC_KEY=enter
	source_file "$XDG_CONFIG_HOME/fzf/history-exec.bash"

	# Taken from FZF's key-bindings.bash to make C-t works
	# history-exec.bash seems to make the binding not work
	bind -m vi-insert -x '"\C-t": fzf-file-widget'
fi

# Readline with autopairs
source_file "$XDG_CONFIG_HOME/readline/autopairs"

# Source files
for bash in "$HOME"/.bashrc.d/*.bash; do
	source_file "$bash"
done
unset -v bash

# Files with colors
# https://github.com/trapd00r/LS_COLORS
source_file /usr/share/LS_COLORS/dircolors.sh

# Make sure that pinentry uses the correct TTY
export GPG_TTY=$(tty)
