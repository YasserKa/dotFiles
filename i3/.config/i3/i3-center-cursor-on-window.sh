#!/bin/env bash
#
# Centers the cursor to the center of window after navigating and moving windows in i3
# Dependencies xdotool i3
#
# Provides WINDOW's PID, X start, Y start, WIDTH, HEIGHT

CURR_WINDOW_ID=$(xdotool getwindowfocus)
eval "$(xdotool getwindowgeometry --shell "$CURR_WINDOW_ID")"
CURR_X="$X"
CURR_Y="$Y"
CURR_WIDTH="$WIDTH"
CURR_HEIGHT="$HEIGHT"

i3-msg "$*"

if [[ "$1" == "focus" ]]; then
  CURR_WINDOW_ID=$(xdotool getwindowfocus)
fi
eval "$(xdotool getwindowgeometry --shell "$CURR_WINDOW_ID")"

i=0
while ((i<=5)) && [[ "$1" == "move" ]] && ((CURR_X==X)) && ((CURR_Y==Y)) && ((CURR_WIDTH==WIDTH)) && ((CURR_HEIGHT==HEIGHT)); do
  i3-msg "$*"
  # Provides WINDOW's PID, X start, Y start, WIDTH, HEIGHT
  eval "$(xdotool getwindowgeometry --shell "$CURR_WINDOW_ID")"
  i=$((i+1))
done

NX=$((WIDTH / 2))
NY=$((HEIGHT / 2))

# Ignore, if there are no windows in workspace (i.e. NX == 0 and NY == 0)
[[ $NX == 0 || $NY == 0 ]] && exit 0

xdotool mousemove --window "$WINDOW" "$NX" "$NY"
