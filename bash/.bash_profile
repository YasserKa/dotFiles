export TERMINAL=termite
export LIBVA_DRIVER_NAME=radeonsi
export VDPAU_DRIVER=radeonsi vainfo

if [ -z "$DISPLAY" ] && [ -n "$XDG_VTNR" ] && [ "$XDG_VTNR" -eq 1 ]; then
	export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"
	exec startx
fi
