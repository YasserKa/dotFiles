


[[ -f ~/.bashrc ]] && . ~/.bashrc

export TERMINAL=termite

if [ -z "$DISPLAY" ] && [ -n "$XDG_VTNR" ] && [ "$XDG_VTNR" -eq 1 ]; then
	export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"
	exec startx
fi
