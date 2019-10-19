#!/bin/sh

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

#disable bell
xset -b

# keyboard bindings sourcing if file doesn't exist
[[ -f ~/.Xmodmap ]] && xmodmap ~/.Xmodmap

exec i3
