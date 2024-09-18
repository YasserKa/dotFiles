#!/bin/env bash
#
# Centers the cursor to the center of window after navigating and moving windows in i3
# Dependencies xdotool i3

eval i3-msg "$*"

CURR_WINDOW_ID=$(xdotool getwindowfocus)

# Provides WINDOW's PID, X start, Y start, WIDTH, HEIGHT
eval "$(xdotool getwindowgeometry --shell "$CURR_WINDOW_ID")"

NX=$((WIDTH / 2))
NY=$((HEIGHT / 2))

# Ignore, if there are no windows in workspace (i.e. NX == 0 and NY == 0)
[[ $NX == 0 || $NY == 0 ]] && exit 0

xdotool mousemove --window "$WINDOW" "$NX" "$NY"
