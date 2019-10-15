#!/bin/sh
#
# ~/.xinitrc

# Executed by startx (run your window manager from here)
if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

/etc/X11/Xsession

# keyboard bindings sourcing if file doesn't exist
if [ -s ~/.Xmodmap ]; then
    xmodmap ~/.Xmodmap
fi

exec i3
# used instead of *exec i3* to make spotify hotkeys work
# exec dbus-launch --autolaunch=$(cat /var/lib/dbus/machine-id) i3
