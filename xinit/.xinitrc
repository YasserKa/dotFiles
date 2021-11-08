#!/bin/sh

usermodmap=$HOME/.Xmodmap

# Disable bell
xset -b
# disable screen saver
xset -dpms
xset s off

eval $(ssh-agent)

# deal with presence of monitors
autorandr --change

# source keyboard bindings
if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

# execute some programs
if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

exec i3
