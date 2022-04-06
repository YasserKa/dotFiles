#!/bin/env sh

# Dependencies xdotool
# Centers the cursor the window after an i3 action (moving or navigating among windows)

CURR_WINDOW_ID=$(xdotool getwindowfocus)

eval i3-msg $*

eval $(xdotool getwindowgeometry --shell $CURR_WINDOW_ID)

NX=$(expr $WIDTH / 2)
NY=$(expr $HEIGHT / 2)

xdotool mousemove --window $WINDOW $NX $NY
