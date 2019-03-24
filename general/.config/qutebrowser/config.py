#  import config
config.bind('J', 'forward')
config.bind('K', 'back')

config.bind('H', 'tab-prev')
config.bind('L', 'tab-next')

config.bind('j', 'run-with-count 3 scroll down')
config.bind('k', 'run-with-count 3 scroll up')

config.bind(';', 'set-cmd-text :')

config.bind('p', 'open -t selection', mode='caret')
config.set('scrolling.smooth', True)
config.set('scrolling.bar', 'never')
