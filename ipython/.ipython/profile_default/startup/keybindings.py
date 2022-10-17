#!/usr/bin/env python3

import os

from IPython import get_ipython
from prompt_toolkit.enums import DEFAULT_BUFFER
from prompt_toolkit.filters import HasFocus, HasSelection, ViInsertMode
from prompt_toolkit.keys import Keys


def tmux_left(event):
    os.system("tmux select-pane -L")


def tmux_down(event):
    os.system("tmux select-pane -D")


def tmux_up(event):
    os.system("tmux select-pane -U")


def tmux_right(event):  
    os.system("tmux select-pane -R")


ip = get_ipython()

# Register the shortcut if IPython is using prompt_toolkit
if getattr(ip, "pt_app", None):
    registry = ip.pt_app.key_bindings
    registry.add_binding(
        Keys.ControlH,
        filter=(HasFocus(DEFAULT_BUFFER) & ~HasSelection() & ~ViInsertMode()),
    )(tmux_left)
    registry.add_binding(
        Keys.ControlJ,
        filter=(HasFocus(DEFAULT_BUFFER) & ~HasSelection() & ~ViInsertMode()),
    )(tmux_down)
    registry.add_binding(
        Keys.ControlK,
        filter=(HasFocus(DEFAULT_BUFFER) & ~HasSelection() & ~ViInsertMode()),
    )(tmux_up)
    registry.add_binding(
        Keys.ControlL,
        filter=(HasFocus(DEFAULT_BUFFER) & ~HasSelection() & ~ViInsertMode()),
    )(tmux_left)
