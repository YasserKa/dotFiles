#!/bin/bash
# https://github.com/polybar/polybar/issues/763
flock 200


killall -q polybar

while pgrep -u $UID -x polybar > /dev/null; do sleep 0.5; done

outputs=$(xrandr --query | grep " connected" | cut -d" " -f1)
tray_output=$(echo $outputs | cut -d" " -f1)

# execute polybar for each screen and assign tray for the main window
for m in $outputs; do
    export MONITOR=$m
    export TRAY_POSITION=none
    if [[ $m == $tray_output ]]; then
        TRAY_POSITION=right
    fi

    polybar --reload mybar </dev/null >/var/tmp/polybar-$m.log 2>&1 200>&- &
    disown
done
