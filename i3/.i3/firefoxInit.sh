#! /bin/bash

i3-msg 'append_layout ~/.i3/firefox.json; exec firefox www.duckduckgo.com'
sleep 1
# i3-msg 'workspace ; append_layout ~/.i3/firefox.json; exec firefox -new-window https://trello.com/b/gpLrkneq/machine-learning; workspace '
