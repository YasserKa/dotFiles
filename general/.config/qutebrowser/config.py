#  import config
config.bind('J', 'forward')
config.bind('K', 'back')

config.bind('tJ', 'forward -t')
config.bind('tK', 'back -t')

config.bind('H', 'tab-prev')
config.bind('L', 'tab-next')

config.bind('j', 'run-with-count 3 scroll down')
config.bind('k', 'run-with-count 3 scroll up')

config.bind('k', 'run-with-count 3 scroll up')

config.bind(';', 'set-cmd-text :')

config.bind('<Ctrl-w>', 'fake-key <Ctrl-backspace>', mode='insert')
config.bind('<Ctrl-h>', 'fake-key <backspace>', mode='insert')

config.bind('<<', 'tab-move -')
config.bind('>>', 'tab-move +')

config.bind('o', 'open selection', mode='caret')
config.bind('O', 'open -t selection', mode='caret')

config.set('scrolling.bar', 'never')
config.set('spellcheck.languages', ['en-US'])


# doesn't work
config.set('scrolling.smooth', True)
