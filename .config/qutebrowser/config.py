# pylint: disable=C0111
c = c  # noqa: F821 pylint: disable=E0602,C0103
config = config  # noqa: F821 pylint: disable=E0602,C0103

c.aliases['h'] = 'help -t'
c.aliases['js'] = 'open -t https://codebeautify.org/jsonviewer?url={url}'

c.bindings.commands = {
    'normal': {
        'j': 'run-with-count 2 scroll down',
        'k': 'run-with-count 2 scroll up',

        'J': 'forward',
        'K': 'back',

        'H': 'tab-prev',
        'L': 'tab-next',

        'gJ': 'forward -t',
        'gK': 'back -t',
        'wO': 'open -w {url} ;; tab-close',

        '<': 'tab-move -',
        '>': 'tab-move +',

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

        ',es': 'config-edit',
        ',ss': 'config-source',
        ',h': 'search',

        '<Ctrl-Shift-V>': 'enter-mode passthrough',
        'cm': 'clear-messages ;; download-clear',

        'ye': 'spawn --userscript ~/.config/qutebrowser/userscripts/emacsMarkdown',
        'yu': 'spawn --userscript ~/.config/qutebrowser/userscripts/youtube',

        # password
        '<z><l>': 'spawn --userscript ~/.config/qutebrowser/userscripts/qute-pass',
        '<z><u><l>': 'spawn --userscript \
                ~/.config/qutebrowser/userscripts/qute-pass --username-only',
        '<z><p><l>': 'spawn --userscript \
                ~/.config/qutebrowser/userscripts/qute-pass --password-only',
        '<z><o><l>': 'spawn --userscript \
                ~/.config/qutebrowser/userscripts/qute-pass --otp-only',
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

        '<Ctrl-Shift-V>': 'leave-mode',
    },
    'command': {
        '<Ctrl-p>': 'completion-item-focus --history prev',
        '<Ctrl-n>': 'completion-item-focus --history next',
    },
}

config.unbind('<Ctrl-V>', mode='normal')
config.unbind(':', mode='normal')

config.set('spellcheck.languages', ['en-US'])
config.set('tabs.title.format', '{current_title}')

config.set('url.searchengines', {
    'DEFAULT': 'https://www.duckduckgo.com/?q={}',
    'duck': 'https://www.duckduckgo.com/?q={}',
    'go': 'https://www.google.com/search?q={}',
    'def': 'https://www.google.com/search?q=define {}',
    'wiki': 'https://en.wikipedia.org/wiki/{}',
})

# removes expired certificate prompt
config.set('content.ssl_strict', True)
# config.set('content.proxy', "http://0.0.0.0:8080")
config.set('scrolling.bar', 'never')

config.set('editor.command', [
    "nvim-qt", "{file}", "--nofork"
])

config.set('scrolling.smooth', False)
