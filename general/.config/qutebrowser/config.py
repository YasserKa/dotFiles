c = c  # noqa: F821 pylint: disable=E0602,C0103
config = config  # noqa: F821 pylint: disable=E0602,C0103

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

            'es': 'config-edit',
            'ss': 'config-source',

            '<Ctrl-Shift-V>': 'enter-mode passthrough',

            'ye': 'spawn --userscript \
            ~/.config/qutebrowser/userscripts/emacsMarkdown',
            'yu': 'spawn --userscript \
            ~/.config/qutebrowser/userscripts/youtube',

                            },
        'insert': {
            '<Ctrl-w>': 'fake-key <Ctrl-backspace>',
            '<Ctrl-h>': 'fake-key <backspace>',

            '<Ctrl-p>': 'fake-key <Up>',
            '<Ctrl-n>': 'fake-key <Down>',
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
        }
config.unbind('<Ctrl-V>', mode='normal')

config.set('spellcheck.languages', ['en-US'])

config.set('scrolling.bar', 'never')

# doesn't work
config.set('scrolling.smooth', True)
