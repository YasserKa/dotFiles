import os

# mypy: ignore-errors

c = c  # type: ignore # noqa: F821
config = config  #  type: ignore # noqa: F821

c.bindings.commands = {
    "normal": {
        "J": "forward",
        "K": "back",
        "H": "tab-prev",
        "L": "tab-next",
        "<": "tab-move -",
        ">": "tab-move +",
        "gJ": "forward -t",
        "gK": "back -t",
        "wao": "open -w {url}",
        "waO": "open -w {url} ;; tab-close",
        "cm": "clear-messages ;; download-clear",
        # Swap ; and :
        ";": "cmd-set-text :",
        ":I": "hint images tab",
        ":O": "hint links fill :open -t -r {hint-url}",
        ":R": "hint --rapid links window",
        ":Y": "hint links yank-primary",
        ":b": "hint all tab-bg",
        ":c": "hint code userscript ~/.config/qutebrowser/userscripts/code_select.py",
        ":d": "hint links download",
        ":f": "hint all tab-fg",
        ":h": "hint all hover",
        ":i": "hint images",
        ":o": "hint links fill :open {hint-url}",
        ":r": "hint --rapid links tab-bg",
        ":t": "hint inputs",
        # Open tabs in the newest window
        ":w": (
            "set new_instance_open_target_window last-opened;; set"
            " new_instance_open_target tab-silent;; hint --rapid links userscript"
            " ~/.config/qutebrowser/userscripts/open_rapid_tabs_in_new_window"
        ),
        # Needed to revert the changes that :w does
        "<Escape>": (
            "clear-keychain ;; search ;; fullscreen --leave;; set"
            " new_instance_open_target_window last-focused;; set"
            " new_instance_open_target tab"
        ),
        ":y": "hint links yank",
        ":e": "hint id userscript ~/.config/qutebrowser/userscripts/yank_link_id",
        ":a": (
            "spawn --userscript ~/.config/qutebrowser/userscripts/yank_amazon_lib_gen"
        ),
        # Override f and F by download documents from libgen
        ":p": "hint tr userscript ~/.config/qutebrowser/userscripts/override_f",
        ":g": "hint images userscript ~/.config/qutebrowser/userscripts/view_gif",
        "<space>q": "close",
        "<space>Q": "quit",
        # Sourcing files
        "<space>es": "spawn bash -ic rcqutebrowser",
        "<space>ss": 'config-source ;; message-info "Configuration file sourced"',
        # Clear search
        "<space>h": "search",
        "<space>c": (
            "spawn --userscript ~/.config/qutebrowser/userscripts/qute_org_capture"
        ),
        # Open/Delete download files
        "<space>od": (
            "spawn --userscript ~/.config/qutebrowser/userscripts/open_download"
        ),
        "<space>ord": (
            "spawn --userscript ~/.config/qutebrowser/userscripts/open_download"
            " --recent"
        ),
        "<space>dd": (
            "spawn --userscript ~/.config/qutebrowser/userscripts/open_download"
            " --delete"
        ),
        "<space>drd": (
            "spawn --userscript ~/.config/qutebrowser/userscripts/open_download"
            " --delete --recent"
        ),
        ",ht": (
            "jseval --quiet"
            " document.querySelector('hypothesis-sidebar').shadowRoot."
            "querySelector('[aria-label=\"Annotation"
            " sidebar\"]').click()"
        ),
        ",hT": (
            "jseval --quiet"
            " document.querySelector('hypothesis-sidebar').shadowRoot."
            "querySelector('[aria-label=\"Show"
            " highlights\"]').click()"
        ),
        ",hh": (
            "spawn --userscript"
            " ~/.config/qutebrowser/userscripts/hypothesis_highlight_and_yank_link"
            " highlight"
        ),
        ",ha": (
            "spawn --userscript"
            " ~/.config/qutebrowser/userscripts/hypothesis_highlight_and_yank_link"
            " annotate"
        ),
        # Yank/Goto URL without anchors
        "yp": (
            "spawn --userscript"
            " ~/.config/qutebrowser/userscripts/yank_url_with_scroll_link_handler"
        ),
        "gc": (
            "spawn --userscript"
            " ~/.config/qutebrowser/userscripts/go_to_url_without_anchors"
        ),
        # Yanking URLs
        "ya": "spawn --userscript ~/.config/qutebrowser/userscripts/yank_all",
        "yh": (
            "spawn --userscript"
            " ~/.config/qutebrowser/userscripts/yank_url_with_highlighted_text"
        ),
        "yo": "spawn --userscript ~/.config/qutebrowser/userscripts/yank_org_link",
        "yc": (  # Github repo names only
            "spawn --userscript ~/.config/qutebrowser/userscripts/yank_org_link --clean"
        ),
        "yl": "spawn --userscript ~/.config/qutebrowser/userscripts/yank_latex_link",
        "ys": "spawn --userscript ~/.config/qutebrowser/userscripts/link_shortener",
        # Google search
        "gs": "spawn --userscript ~/.config/qutebrowser/userscripts/google_search",
        "gS": "spawn --userscript ~/.config/qutebrowser/userscripts/google_search tab",
        # Go to domain or github project's root
        "gr": "spawn --userscript ~/.config/qutebrowser/userscripts/go_to_root",
        "gR": "spawn --userscript ~/.config/qutebrowser/userscripts/go_to_root tab",
        "wr": "spawn --userscript ~/.config/qutebrowser/userscripts/go_to_root window",
        # Overrides gt, by allowing chosen tabs in hidden workspaces to be focused
        "gt": "spawn --userscript ~/.config/qutebrowser/userscripts/override_gt",
        # Check open ports
        "gl": (
            "spawn --userscript ~/.config/qutebrowser/userscripts/open_localhost list"
        ),
        # Create session and store command to open it in clipboard
        "ss": (
            "spawn --userscript ~/.config/qutebrowser/userscripts/create_yank_session"
        ),
        # Google translate
        "<space>t": (
            "spawn --userscript ~/.config/qutebrowser/userscripts/qute_translate"
        ),
        # Use <C-S-v> instead of <C-v> for passthrough mode
        "<Ctrl-v>": "nop",
        "<Ctrl-Shift-v>": "mode-enter passthrough",
        # Password manager
        "<z><l>": "spawn --userscript ~/.config/qutebrowser/userscripts/qute_bitwarden",
        "<z><u><l>": (
            "spawn --userscript ~/.config/qutebrowser/userscripts/qute_bitwarden"
            " --username-only"
        ),
        "<z><p><l>": (
            "spawn --userscript ~/.config/qutebrowser/userscripts/qute_bitwarden"
            " --password-only"
        ),
        # Others
        "<space>H": (
            "spawn --userscript ~/.config/qutebrowser/userscripts/open_localhost list"
        ),
        # Unbind <C-h> from home
        "<Ctrl-h>": "nop",
    },
    "insert": {
        "<Ctrl-l>": (
            "fake-key <Shift-Home>;; cmd-later 50 spawn --userscript"
            " ~/.config/qutebrowser/userscripts/fix_last_typo"
        ),
        "<Ctrl-Shift-l>": (
            "fake-key <Shift-Home>;; cmd-later 50 spawn --userscript"
            " ~/.config/qutebrowser/userscripts/fix_last_typo choose"
        ),
        # Readline
        # Navigation
        "<Ctrl-a>": "fake-key <Home>",
        "<Ctrl-e>": "fake-key <End>",
        "<Ctrl-b>": "fake-key <Left>",
        "<Ctrl-f>": "fake-key <Right>",
        "<Alt-b>": "fake-key <Ctrl-Left>",
        "<Alt-f>": "fake-key <Ctrl-Right>",
        # Editing
        "<Ctrl-u>": "fake-key <Shift-Home>;; cmd-later 3 fake-key <Delete>",
        "<Ctrl-k>": "fake-key <Shift-End><Delete>",
        "<Ctrl-h>": "fake-key <backspace>",
        "<Ctrl-d>": "fake-key <Delete>",
        "<Ctrl-w>": "fake-key <Ctrl-backspace>",
        "<Alt-d>": "fake-key <Ctrl-Delete>",
        # Others
        "<Ctrl-p>": "fake-key <Up>",
        "<Ctrl-n>": "fake-key <Down>",
        "<Ctrl-j>": "fake-key <enter>",
        "<Ctrl-m>": "fake-key <enter>",
        "<Ctrl-Shift-a>": "fake-key <Ctrl-a>",
        # Copy
        "<Ctrl-y>": "fake-key <Ctrl-a><Ctrl-c><Right>",
        # Use another binding for editing in external editor
        "<Ctrl-Shift-e>": "edit-text",
    },
    "caret": {
        "o": "open selection",
        "O": "open -t selection",
    },
    "passthrough": {
        "<Ctrl-w>": "fake-key <Ctrl-backspace>",
        "<Ctrl-h>": "fake-key <backspace>",
        "<Ctrl-p>": "fake-key <Up>",
        "<Ctrl-n>": "fake-key <Down>",
        # NOTE: Commented out for the time being, since it messes up vim binding
        # in jupyter notebooks
        "<Ctrl-j>": "fake-key <enter>",
        "<Ctrl-Shift-v>": "mode-leave",
    },
    "command": {
        # Navigation
        "<Alt-b>": "rl-backward-word",
        "<Alt-f>": "rl-forward-word",
        "<Alt-Shift-b>": "rl-backward-word",  # Doesn't work
        "<Alt-Shift-f>": "rl-forward-word",  # Doesn't work
        # # Editing
        "<Ctrl-d>": "rl-delete-char",
        "<Ctrl-w>": "rl-backward-kill-word",
        "<Alt-d>": "rl-kill-word",
        "<Alt-w>": "rl-rubout ' '",
        "<Alt-Shift-d>": "rl-kill-word",  # Doesn't work
        # Candidates
        "<Ctrl-p>": "completion-item-focus prev",
        "<Ctrl-n>": "completion-item-focus next",
        "<Alt-Shift-p>": "completion-item-focus prev-page",
        "<Alt-Shift-n>": "completion-item-focus next-page",
        "<Alt-p>": "command-history-prev",
        "<Alt-n>": "command-history-next",
        "<Ctrl-m>": "command-accept",
        "<Alt-m>": "command-accept --rapid",
        "<Ctrl-Alt-d>": "completion-item-del",
    },
}
config.unbind(":", mode="normal")
# Don't close window on C-q
config.unbind("<Ctrl-q>", mode="normal")

# Needed to make window focusing work in Wayland
if "WAYLAND_DISPLAY" in os.environ:
    c.qt.force_platform = "XCB"

c.aliases = {
    "q": "close",
    "yank_footnote": 'yank inline "[] []: {url}"',
    "qa": "quit",
    "w": "session-save",
    "wq": "quit --save",
    "wqa": "quit --save",
    "h": "help -t",
    "H": "help -w",
    "chrome": "spawn --detach google-chrome-stable '{url}'",
    "awesome": "spawn --userscript ~/.config/qutebrowser/userscripts/goto_awesome_tracker {url}",
    "archive_page": "spawn archive_web_page {url}",
    "new_reddit": "spawn --userscript ~/.config/qutebrowser/userscripts/goto_new_reddit {url}",
    "gotoarchive_page_all": "spawn qutebrowser http://archive.yasserk.com/archive/{url} https://web.archive.org/{url} https://ghostarchive.org/search?term={url} https://archive.ph/newest/{url}",
    "gotoarchive_page_wayback_machine": "open -t https://web.archive.org/{url}",
    "gotoarchive_page_archive_box": "open -t http://archive.yasserk.com/archive/{url}",
    "gotoarchive_page_today": "open -t https://archive.ph/newest/{url}",
    "gotoarchive_page_ghost": "open -t https://ghostarchive.org/search?term={url}",
    "json": "open -t https://codebeautify.org/jsonviewer?url={url}",
    "download_youtube": (
        "spawn --userscript ~/.config/qutebrowser/userscripts/download_youtube"
    ),
    "gc": "spawn bash -ic '. $HOME/bin/git_clone {url}'",
    "paywall": "open https://12ft.io/proxy?q={url}",
    "import_zotero": "spawn ~/.config/Zotero/bin/zotero_utility import {url}",
    "import_zotero_recent": "spawn ~/.config/Zotero/bin/zotero_utility import --recent {url}",
    "import_zotero_read": "spawn ~/.config/Zotero/bin/zotero_utility import --add_org_entry --collection_name 'To_Read' {url}",
    "get_paper_citations": "spawn ~/bin/notify_paper_citation {url}",
    "get_paper_graph": 'spawn bash -ic "xdg-open https://inciteful.xyz/p/$(~/.config/Zotero/bin/zotero_utility get_metadata DOI {url})"',
    "kde_share": "spawn kde_share_url {url}",
    "yt-transcribe": "spawn --userscript ~/.config/qutebrowser/userscripts/yt-transcribe",
    "paper": "spawn --userscript ~/.config/qutebrowser/userscripts/get_paper",
    "generate_password": (
        "spawn --userscript ~/.config/qutebrowser/userscripts/generate_password"
    ),
    "qrencode": "spawn bash -ic 'qrencode {url} -o qr.png && feh qr.png && rm qr.png'",
    "hosts": "spawn --userscript ~/.config/qutebrowser/userscripts/open_localhost list",
    "translate": "spawn --userscript ~/.config/qutebrowser/userscripts/qute_translate",
}

config.load_autoconfig()

c.content.blocking.whitelist = ["https://analytics.google.com/analytics/*"]
c.content.blocking.method = "both"
c.content.notifications.enabled = False
c.content.tls.certificate_errors = "ask-block-thirdparty"
# Enable save to clipbaord buttons
c.content.javascript.clipboard = "access-paste"

with config.pattern("https://www.google.com") as p:
    p.content.geolocation = True

c.auto_save.session = True

c.hints.selectors["code"] = [
    # Selects all code tags whose direct parent is not a pre tag
    ":not(pre) > code",
    "pre",
]
c.hints.selectors["tr"] = [
    # Selects all code tags whose direct parent is not a pre tag
    "tr td:last-child",
]
# Add a CSS selector for yank_link_id script
c.hints.selectors["id"] = ["[id]"]
# Make gi focus issues search bar
with config.pattern("https://github.com/*/issues") as p:
    p.hints.selectors = {"inputs": ["input[id='js-issues-search']"]}

c.hints.border = "1px solid #CCCCCC"
c.fonts.default_family = ["Inconsolata"]
c.fonts.default_size = "12pt"
c.fonts.web.size.default = 19

c.fileselect.handler = "external"

c.fileselect.folder.command = f"kitty -e {os.environ['XDG_CONFIG_HOME']}/qutebrowser/userscripts/qute_file_picker {{}} directory".split()
c.fileselect.single_file.command = f"kitty -e {os.environ['XDG_CONFIG_HOME']}/qutebrowser/userscripts/qute_file_picker {{}} file".split()
c.fileselect.multiple_files.command = f"kitty -e {os.environ['XDG_CONFIG_HOME']}/qutebrowser/userscripts/qute_file_picker {{}} files".split()

c.spellcheck.languages = ["en-US"]
# Open new tabs next to the current one
c.tabs.new_position.unrelated = "next"
c.scrolling.smooth = False
c.scrolling.bar = "never"

# Status bar and title format
c.statusbar.widgets = ["url", "progress", "scroll"]
c.tabs.title.format = "{current_title}"

# Don't show file browser in download prompt
c.prompt.filebrowser = False
c.downloads.location.suggestion = "both"
c.downloads.remove_finished = 0
c.downloads.location.directory = f"{os.environ['HOME']}/Downloads"

c.editor.command = [os.getenv("EDITOR_GUI"), "{file}", "--nofork"]
c.completion.open_categories = ["quickmarks", "bookmarks", "history"]

c.url.default_page = "https://www.google.com"
c.url.start_pages = "https://www.google.com"
c.url.searchengines = {
    "DEFAULT": "https://www.google.com/search?q={}",
    "duck": "https://www.duckduckgo.com/?q={}",
    "doi": "https://doi.org/{}",
    "go": "https://www.google.com/search?q={}",
    "def": "https://www.merriam-webster.com/dictionary/{}",
    "wiki": "https://en.wikipedia.org/wiki/{}",
}

# gruvbox dark hard qutebrowser theme by Florian Bruhin <me@the-compiler.org>
#
# Originally based on:
#   base16-qutebrowser (https://github.com/theova/base16-qutebrowser)
#   Base16 qutebrowser template by theova and Daniel Mulford
#   Gruvbox dark, hard scheme by Dawid Kurek (dawikur@gmail.com), morhetz (https://github.com/morhetz/gruvbox)

bg0_hard = "#1d2021"
bg0_soft = "#32302f"
bg0_normal = "#282828"

bg0 = bg0_normal
bg1 = "#3c3836"
bg2 = "#504945"
bg3 = "#665c54"
bg4 = "#7c6f64"

fg0 = "#fbf1c7"
fg1 = "#ebdbb2"
fg2 = "#d5c4a1"
fg3 = "#bdae93"
fg4 = "#a89984"

bright_red = "#fb4934"
bright_green = "#b8bb26"
bright_yellow = "#fabd2f"
bright_blue = "#83a598"
bright_purple = "#d3869b"
bright_aqua = "#8ec07c"
bright_gray = "#928374"
bright_orange = "#fe8019"

dark_red = "#cc241d"
dark_green = "#98971a"
dark_yellow = "#d79921"
dark_blue = "#458588"
dark_purple = "#b16286"
dark_aqua = "#689d6a"
dark_gray = "#a89984"
dark_orange = "#d65d0e"

# Completion

# Text color of the completion widget. May be a single color to use for
# all columns or a list of three colors, one for each column.
c.colors.completion.fg = [fg1, bright_aqua, bright_yellow]

# Background color of the completion widget for odd rows.
c.colors.completion.odd.bg = bg0

# Background color of the completion widget for even rows.
c.colors.completion.even.bg = c.colors.completion.odd.bg

# Foreground color of completion widget category headers.
c.colors.completion.category.fg = bright_blue

# Background color of the completion widget category headers.
c.colors.completion.category.bg = bg1

# Top border color of the completion widget category headers.
c.colors.completion.category.border.top = c.colors.completion.category.bg

# Bottom border color of the completion widget category headers.
c.colors.completion.category.border.bottom = c.colors.completion.category.bg

# Foreground color of the selected completion item.
c.colors.completion.item.selected.fg = fg0

# Background color of the selected completion item.
c.colors.completion.item.selected.bg = bg4

# Top border color of the selected completion item.
c.colors.completion.item.selected.border.top = bg2

# Bottom border color of the selected completion item.
c.colors.completion.item.selected.border.bottom = (
    c.colors.completion.item.selected.border.top
)

# Foreground color of the matched text in the selected completion item.
c.colors.completion.item.selected.match.fg = bright_orange

# Foreground color of the matched text in the completion.
c.colors.completion.match.fg = c.colors.completion.item.selected.match.fg

# Color of the scrollbar handle in the completion view.
c.colors.completion.scrollbar.fg = c.colors.completion.item.selected.fg

# Color of the scrollbar in the completion view.
c.colors.completion.scrollbar.bg = c.colors.completion.category.bg

# Context menu

# Background color of disabled items in the context menu.
c.colors.contextmenu.disabled.bg = bg3

# Foreground color of disabled items in the context menu.
c.colors.contextmenu.disabled.fg = fg3

# Background color of the context menu. If set to null, the Qt default is used.
c.colors.contextmenu.menu.bg = bg0

# Foreground color of the context menu. If set to null, the Qt default is used.
c.colors.contextmenu.menu.fg = fg2

# Background color of the context menu’s selected item. If set to null, the Qt default
# is used.
c.colors.contextmenu.selected.bg = bg2

# Foreground color of the context menu’s selected item. If set to null, the Qt default
# is used.
c.colors.contextmenu.selected.fg = c.colors.contextmenu.menu.fg

# Downloads

# Background color for the download bar.
c.colors.downloads.bar.bg = bg0

# Color gradient start for download text.
c.colors.downloads.start.fg = bg0

# Color gradient start for download backgrounds.
c.colors.downloads.start.bg = bright_blue

# Color gradient end for download text.
c.colors.downloads.stop.fg = c.colors.downloads.start.fg

# Color gradient stop for download backgrounds.
c.colors.downloads.stop.bg = bright_aqua

# Foreground color for downloads with errors.
c.colors.downloads.error.fg = bright_red

# Hints

# Font color for hints.
c.colors.hints.fg = bg0

# Background color for hints.
c.colors.hints.bg = "rgba(250, 191, 47, 200)"  # bright_yellow

# Font color for the matched part of hints.
c.colors.hints.match.fg = bg4

# Keyhint widget

# Text color for the keyhint widget.
c.colors.keyhint.fg = fg4

# Highlight color for keys to complete the current keychain.
c.colors.keyhint.suffix.fg = fg0

# Background color of the keyhint widget.
c.colors.keyhint.bg = bg0

# Messages

# Foreground color of an error message.
c.colors.messages.error.fg = bg0

# Background color of an error message.
c.colors.messages.error.bg = bright_red

# Border color of an error message.
c.colors.messages.error.border = c.colors.messages.error.bg

# Foreground color of a warning message.
c.colors.messages.warning.fg = bg0

# Background color of a warning message.
c.colors.messages.warning.bg = bright_purple

# Border color of a warning message.
c.colors.messages.warning.border = c.colors.messages.warning.bg

# Foreground color of an info message.
c.colors.messages.info.fg = fg2

# Background color of an info message.
c.colors.messages.info.bg = bg0

# Border color of an info message.
c.colors.messages.info.border = c.colors.messages.info.bg

# Prompts

# Foreground color for prompts.
c.colors.prompts.fg = fg2

# Border used around UI elements in prompts.
c.colors.prompts.border = f"1px solid {bg1}"

# Background color for prompts.
c.colors.prompts.bg = bg3

# Background color for the selected item in filename prompts.
c.colors.prompts.selected.bg = bg2

# Statusbar

# Foreground color of the statusbar.
c.colors.statusbar.normal.fg = fg2

# Background color of the statusbar.
c.colors.statusbar.normal.bg = bg0

# Foreground color of the statusbar in insert mode.
c.colors.statusbar.insert.fg = bg0

# Background color of the statusbar in insert mode.
c.colors.statusbar.insert.bg = dark_aqua

# Foreground color of the statusbar in passthrough mode.
c.colors.statusbar.passthrough.fg = bg0

# Background color of the statusbar in passthrough mode.
c.colors.statusbar.passthrough.bg = dark_blue

# Foreground color of the statusbar in private browsing mode.
c.colors.statusbar.private.fg = bright_purple

# Background color of the statusbar in private browsing mode.
c.colors.statusbar.private.bg = bg0

# Foreground color of the statusbar in command mode.
c.colors.statusbar.command.fg = fg3

# Background color of the statusbar in command mode.
c.colors.statusbar.command.bg = bg1

# Foreground color of the statusbar in private browsing + command mode.
c.colors.statusbar.command.private.fg = c.colors.statusbar.private.fg

# Background color of the statusbar in private browsing + command mode.
c.colors.statusbar.command.private.bg = c.colors.statusbar.command.bg

# Foreground color of the statusbar in caret mode.
c.colors.statusbar.caret.fg = bg0

# Background color of the statusbar in caret mode.
c.colors.statusbar.caret.bg = dark_purple

# Foreground color of the statusbar in caret mode with a selection.
c.colors.statusbar.caret.selection.fg = c.colors.statusbar.caret.fg

# Background color of the statusbar in caret mode with a selection.
c.colors.statusbar.caret.selection.bg = bright_purple

# Background color of the progress bar.
c.colors.statusbar.progress.bg = bright_blue

# Default foreground color of the URL in the statusbar.
c.colors.statusbar.url.fg = fg4

# Foreground color of the URL in the statusbar on error.
c.colors.statusbar.url.error.fg = dark_red

# Foreground color of the URL in the statusbar for hovered links.
c.colors.statusbar.url.hover.fg = bright_orange

# Foreground color of the URL in the statusbar on successful load
# (http).
c.colors.statusbar.url.success.http.fg = fg0

# Foreground color of the URL in the statusbar on successful load
# (https).
c.colors.statusbar.url.success.https.fg = fg0

# Foreground color of the URL in the statusbar when there's a warning.
c.colors.statusbar.url.warn.fg = bright_purple

# tabs

# Background color of the tab bar.
c.colors.tabs.bar.bg = bg0

# Color gradient start for the tab indicator.
c.colors.tabs.indicator.start = bright_blue

# Color gradient end for the tab indicator.
c.colors.tabs.indicator.stop = bright_aqua

# Color for the tab indicator on errors.
c.colors.tabs.indicator.error = bright_red

# Foreground color of unselected odd tabs.
c.colors.tabs.odd.fg = fg2

# Background color of unselected odd tabs.
c.colors.tabs.odd.bg = bg2

# Foreground color of unselected even tabs.
c.colors.tabs.even.fg = c.colors.tabs.odd.fg

# Background color of unselected even tabs.
c.colors.tabs.even.bg = bg3

# Foreground color of selected odd tabs.
c.colors.tabs.selected.odd.fg = fg2

# Background color of selected odd tabs.
c.colors.tabs.selected.odd.bg = bg0

# Foreground color of selected even tabs.
c.colors.tabs.selected.even.fg = c.colors.tabs.selected.odd.fg

# Background color of selected even tabs.
c.colors.tabs.selected.even.bg = bg0

# Background color of pinned unselected even tabs.
c.colors.tabs.pinned.even.bg = bright_green

# Foreground color of pinned unselected even tabs.
c.colors.tabs.pinned.even.fg = bg2

# Background color of pinned unselected odd tabs.
c.colors.tabs.pinned.odd.bg = bright_green

# Foreground color of pinned unselected odd tabs.
c.colors.tabs.pinned.odd.fg = c.colors.tabs.pinned.even.fg

# Background color of pinned selected even tabs.
c.colors.tabs.pinned.selected.even.bg = bg0

# Foreground color of pinned selected even tabs.
c.colors.tabs.pinned.selected.even.fg = c.colors.tabs.selected.odd.fg

# Background color of pinned selected odd tabs.
c.colors.tabs.pinned.selected.odd.bg = c.colors.tabs.pinned.selected.even.bg

# Foreground color of pinned selected odd tabs.
c.colors.tabs.pinned.selected.odd.fg = c.colors.tabs.selected.odd.fg

# Background color for webpages if unset (or empty to use the theme's
# color).
# c.colors.webpage.bg = bg4
