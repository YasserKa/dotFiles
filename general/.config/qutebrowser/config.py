# pylint: disable=C0111
c = c  # noqa: F821 pylint: disable=E0602,C0103
config = config  # noqa: F821 pylint: disable=E0602,C0103

c.aliases['h'] = 'help -t'
c.aliases['js'] = 'open -t https://codebeautify.org/jsonviewer?url={url}'

c.bindings.commands = {
    'normal': {
        'j': 'scroll-px 0 100',
        'k': 'scroll-px 0 -100',

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

config.set('scrolling.bar', 'never')

config.set('editor.command', [
    "nvim-qt", "{file}", "--", "-c", "normal {line}G{column0}l"
])
# doesn't work
config.set('scrolling.smooth', True)
