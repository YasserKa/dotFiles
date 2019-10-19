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
            ',h': 'help -t',

            '<Ctrl-Shift-V>': 'enter-mode passthrough',

            'ye': 'spawn --userscript ~/.config/qutebrowser/userscripts/emacsMarkdown',

            'yu': 'spawn --userscript ~/.config/qutebrowser/userscripts/youtube',

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
config.unbind(':', mode='normal')

config.set('spellcheck.languages', ['en-US'])
config.set('tabs.title.format', '{index} {current_title}')

config.set('scrolling.bar', 'never')

# doesn't work
config.set('scrolling.smooth', True)
