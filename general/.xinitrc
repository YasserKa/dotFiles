#!/bin/sh

#disable bell
xset -b
[[ -f ~/.Xmodmap ]] && xmodmap ~/.Xmodmap

[[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources

for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
done

compton -f --inactive-dim 0.2 &
devilspie &
unset f
exec i3
