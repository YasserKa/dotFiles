#!/bin/sh

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

# nvidia  https://wiki.archlinux.org/index.php/NVIDIA_Optimus
# xrandr --setprovideroutputsource modesetting NVIDIA-0
xrandr --auto
xrandr --dpi 96

# Disable bell
xset -b
# disable screen saver
xset -dpms
xset s off

eval $(ssh-agent)

# Deals with presence of monitors
autorandr --change

# Keyboard bindings sourcing if file doesn't exist
[[ -f ~/.Xmodmap ]] && xmodmap ~/.Xmodmap

exec i3
