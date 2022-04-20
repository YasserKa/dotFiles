# pylint: disable=C0111
c = c  # noqa: F821 pylint: disable=E0602,C0103
config = config  # noqa: F821 pylint: disable=E0602,C0103

c.bindings.commands = {
    'normal': {
        'J': 'forward',
        'K': 'back',

        'H': 'tab-prev',
        'L': 'tab-next',

        'gJ': 'forward -t',
        'gK': 'back -t',
        'wO': 'open -w {url} ;; tab-close',

        '<': 'tab-move -',
        '>': 'tab-move +',

        # Swap ; and :
        ';': 'set-cmd-text :',

        ':I': 'hint images tab',
        ':O': 'hint links fill :open -t -r {hint-url}',
        ':R': 'hint --rapid links window',
        ':Y': 'hint links yank-primary',
        ':b': 'hint all tab-bg',
        ':d': 'hint links download',
        ':f': 'hint all tab-fg',
        ':h': 'hint all hover',
        ':i': 'hint images',
        ':o': 'hint links fill :open {hint-url}',
        ':r': 'hint --rapid links tab-bg',
        ':t': 'hint inputs',
        ':y': 'hint links yank',
        ':e': 'hint id userscript ~/.config/qutebrowser/userscripts/yank_link_id',

        # Use <C-S-v> instead of <C-v> for passthrough mode
        '<Ctrl-v>': 'nop',
        '<Ctrl-Shift-v>': 'mode-enter passthrough',

        # Allows chosen tabs in hidden workspaces to be focused
        'gt': 'spawn --userscript ~/.config/qutebrowser/userscripts/override_gt',

        # Yank URL without anchors
        'yc': 'spawn --userscript ~/.config/qutebrowser/userscripts/yank_url_without_anchors',
        'gc': 'spawn --userscript ~/.config/qutebrowser/userscripts/go_to_url_without_anchors',

        # Override f and F by download documents from libgen
        ':p': 'hint all userscript ~/.config/qutebrowser/userscripts/override_f',

        'f': 'hint all;; set-mark o;; later 1000 set-mark i',
        '<Ctrl-o>': 'jump-mark o;; clear-messages',
        '<Ctrl-i>': 'jump-mark i;; clear-messages',


        # 'F': 'hint all userscript ~/.config/qutebrowser/userscripts/override_F',

        ',es': 'config-edit',
        ',ss': 'config-source ;; message-info "Configuration file sourced"',
        ',h': 'search',
        ',c': 'spawn emacsclient org-protocol://capture?url={url}&title={title}',

        ',od': 'spawn --userscript ~/.config/qutebrowser/userscripts/open_download',
        ',ord': 'spawn --userscript ~/.config/qutebrowser/userscripts/open_download --recent',
        ',t': 'spawn --userscript ~/.config/qutebrowser/userscripts/qute-translate',

        'cm': 'clear-messages ;; download-clear',

        'ya': 'spawn --userscript ~/.config/qutebrowser/userscripts/yank_all',
        'yo': 'spawn --userscript ~/.config/qutebrowser/userscripts/yank_org_link',
        'yl': 'spawn --userscript ~/.config/qutebrowser/userscripts/yank_latex_link',
        'yu': 'spawn --userscript ~/.config/qutebrowser/userscripts/download_youtube',
        'ys': 'spawn --userscript ~/.config/qutebrowser/userscripts/link_shortener',
        'gl': 'spawn --userscript ~/.config/qutebrowser/userscripts/localhost list',

        # Google search
        'gs': 'spawn --userscript ~/.config/qutebrowser/userscripts/google_search',
        'gS': 'spawn --userscript ~/.config/qutebrowser/userscripts/google_search tab',

        # Go to domain or github project's root
        'gr': 'spawn --userscript ~/.config/qutebrowser/userscripts/go_to_root',
        'gR': 'spawn --userscript ~/.config/qutebrowser/userscripts/go_to_root tab',
        'wr': 'spawn --userscript ~/.config/qutebrowser/userscripts/go_to_root window',

        # Password manager
        '<z><l>': 'spawn --userscript ~/.config/qutebrowser/userscripts/qute-bitwarden',
        '<z><u><l>': 'spawn --userscript ~/.config/qutebrowser/userscripts/qute-bitwarden --username-only',
        '<z><p><l>': 'spawn --userscript ~/.config/qutebrowser/userscripts/qute-bitwarden --password-only',
    },
    'insert': {
            '<Ctrl-w>': 'fake-key <Ctrl-backspace>',
            '<Ctrl-h>': 'fake-key <backspace>',

            '<Ctrl-p>': 'fake-key <Up>',
            '<Ctrl-n>': 'fake-key <Down>',
            '<Ctrl-j>': 'fake-key <enter>',
            },
    'caret': {
            'o': 'open selection',
            'O': 'open -t selection',
            },
    'passthrough': {
            '<Ctrl-w>': 'fake-key <Ctrl-backspace>',
            '<Ctrl-h>': 'fake-key <backspace>',

            '<Ctrl-p>': 'fake-key <Up>',
            '<Ctrl-n>': 'fake-key <Down>',
            '<Ctrl-j>': 'fake-key <enter>',

            '<Ctrl-Shift-v>': 'mode-leave',
            },

    'command': {
            '<Ctrl-p>': 'completion-item-focus --history prev',
            '<Ctrl-n>': 'completion-item-focus --history next',
            '<Ctrl-w>': 'rl-backward-kill-word',
            },
}
config.unbind(':', mode='normal')

c.aliases = {
        'q': 'close',
        'qa': 'quit',
        'w': 'session-save',
        'wq': 'quit --save',
        'wqa': 'quit --save',
        'h': 'help -t',
        'json': 'open -t https://codebeautify.org/jsonviewer?url={url}',
        'paywall': 'open https://12ft.io/proxy?q={url}',
        'zotero': 'spawn --userscript ~/.config/qutebrowser/userscripts/qute-zotero',
        'hosts': 'spawn --userscript ~/.config/qutebrowser/userscripts/localhost list',
        'translate': 'spawn --userscript ~/.config/qutebrowser/userscripts/qute-translate',
        'open_download': 'spawn --userscript ~/.config/qutebrowser/userscripts/open_download',
        }

config.load_autoconfig()

c.content.blocking.whitelist = ['https://analytics.google.com/analytics/*']
c.content.blocking.method = 'both'
c.content.notifications.enabled = False
c.content.tls.certificate_errors = "ask-block-thirdparty"
# Enable save to clipbaord buttons
c.content.javascript.can_access_clipboard = True

with config.pattern('https://www.google.com') as p:
    p.content.geolocation = True

c.auto_save.session = True

# Add a CSS selector for yank_link_id script
c.hints.selectors['id'] = ["[id]"]
# Make gi focus issues search bar
with config.pattern('https://github.com/*/issues') as p:
    p.hints.selectors = {'inputs': ["input[id='js-issues-search']"]}
c.hints.border = "1px solid #CCCCCC"
c.fonts.default_size = "12pt"
c.fonts.web.size.default = 19

c.spellcheck.languages = ['en-US']
# Open new tabs next to the current one
c.tabs.new_position.unrelated = 'next'
c.scrolling.smooth = False
c.scrolling.bar = 'never'

# Status bar and title format
c.statusbar.widgets = ["url", "progress", "scroll"]
c.tabs.title.format = '{current_title}'

# Don't show file browser in download prompt
c.prompt.filebrowser = False
c.downloads.location.suggestion = 'both'
c.downloads.remove_finished = 0

c.editor.command = ["nvim-qt", "{file}", "--nofork" ]
c.completion.open_categories = ["quickmarks", "bookmarks", "history"]

c.url.searchengines = {
        'DEFAULT': 'https://www.duckduckgo.com/?q={}',
        'duck': 'https://www.duckduckgo.com/?q={}',
        'go': 'https://www.google.com/search?q={}',
        'def': 'https://www.merriam-webster.com/dictionary/{}',
        'wiki': 'https://en.wikipedia.org/wiki/{}',
        }

# gruvbox dark hard qutebrowser theme by Florian Bruhin <me@the-compiler.org>
#
# Originally based on:
#   base16-qutebrowser (https://github.com/theova/base16-qutebrowser)
#   Base16 qutebrowser template by theova and Daniel Mulford
#   Gruvbox dark, hard scheme by Dawid Kurek (dawikur@gmail.com), morhetz (https://github.com/morhetz/gruvbox)

bg0_hard = "#1d2021"
bg0_soft = '#32302f'
bg0_normal = '#282828'

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
c.colors.completion.item.selected.border.bottom = c.colors.completion.item.selected.border.top

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

# Background color of the context menu’s selected item. If set to null, the Qt default is used.
c.colors.contextmenu.selected.bg = bg2

# Foreground color of the context menu’s selected item. If set to null, the Qt default is used.
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
c.colors.hints.bg = 'rgba(250, 191, 47, 200)'  # bright_yellow

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
c.colors.prompts.border = f'1px solid {bg1}'

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
