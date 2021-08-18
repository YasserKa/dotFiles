#!/bin/sh

usermodmap=$HOME/.Xmodmap

# nvidia  https://wiki.archlinux.org/index.php/NVIDIA_Optimus
# xrandr --setprovideroutputsource modesetting NVIDIA-0
# xrandr --auto
# xrandr --dpi 96

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
