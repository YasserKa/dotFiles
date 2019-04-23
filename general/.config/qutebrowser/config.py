# navigation
config.bind('j', 'run-with-count 3 scroll down')
config.bind('k', 'run-with-count 3 scroll up')

config.bind('J', 'forward')
config.bind('K', 'back')

config.bind('H', 'tab-prev')
config.bind('L', 'tab-next')

config.bind('tJ', 'forward -t')
config.bind('tK', 'back -t')

config.bind('<', 'tab-move -')
config.bind('>', 'tab-move +')

config.bind(';', 'set-cmd-text :')

# edit
config.bind('<Ctrl-w>', 'fake-key <Ctrl-backspace>', mode='insert')
config.bind('<Ctrl-h>', 'fake-key <backspace>', mode='insert')

config.bind('<Ctrl-w>', 'fake-key <Ctrl-backspace>', mode='passthrough')
config.bind('<Ctrl-h>', 'fake-key <backspace>', mode='passthrough')

config.bind('o', 'open selection', mode='caret')
config.bind('O', 'open -t selection', mode='caret')

config.unbind('<Ctrl-V>', mode='normal')
config.bind('<Ctrl-Shift-V>', 'leave-mode', mode='passthrough')
config.bind('<Ctrl-Shift-V>', 'enter-mode passthrough')

config.bind('ye', 'spawn --userscript ~/.config/qutebrowser/userscripts/emacsMarkdown ;; message-info "yanked emacs markdown"')


config.bind('es', 'config-edit')
config.bind('ss', 'config-source')

# open a tab in a new window then close it in the current window
config.bind('wO', 'open -w {url} ;; tab-close')

config.set('spellcheck.languages', ['en-US'])

config.set('scrolling.bar', 'never')
# doesn't work
#  config.set('scrolling.smooth', True)
