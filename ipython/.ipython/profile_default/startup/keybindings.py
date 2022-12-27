#!/usr/bin/env python3

import os

import pyperclip
from IPython import get_ipython
from prompt_toolkit.enums import DEFAULT_BUFFER
from prompt_toolkit.filters import (
    HasFocus,
    HasSelection,
    ViInsertMode,
    ViNavigationMode,
    ViSelectionMode,
)
from prompt_toolkit.key_binding.vi_state import InputMode
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


def copy_selection_to_clipboard(event):
    buffer = event.current_buffer
    data = buffer.copy_selection()
    pyperclip.copy(data.text)


def copy_to_clipboard(event):
    buffer = event.current_buffer
    pyperclip.copy(buffer.text)


def paste_from_clipboard(event):
    buffer = event.current_buffer
    data = pyperclip.paste()
    event.cli.vi_state.input_mode = InputMode.INSERT
    buffer.insert_text(data)
    event.cli.vi_state.input_mode = InputMode.NAVIGATION


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

    # Add yank/paste shortcuts

    registry.add_binding("y", filter=HasFocus(DEFAULT_BUFFER) & ViSelectionMode())(
        copy_selection_to_clipboard
    )

    registry.add_binding(
        "y", "y", filter=HasFocus(DEFAULT_BUFFER) & ViNavigationMode()
    )(copy_to_clipboard)

    registry.add_binding(
        Keys.ControlY, filter=HasFocus(DEFAULT_BUFFER) & ViInsertMode()
    )(copy_to_clipboard)

    registry.add_binding("p", filter=HasFocus(DEFAULT_BUFFER) & ViNavigationMode())(
        paste_from_clipboard
    )
