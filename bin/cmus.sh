#!/bin/bash

stat=$(cmus-remote -Q 2> /dev/null | grep status | cut -d ' ' -f2-)
duration=$(cmus-remote -Q 2> /dev/null | grep duration | cut -d ' ' -f2-)
current=$(cmus-remote -Q 2> /dev/null | grep position | cut -d ' ' -f2-)
artist=$(cmus-remote -Q 2> /dev/null | grep ' artist ' | cut -d ' ' -f3-)
song=$(cmus-remote -Q 2> /dev/null | grep title | cut -d ' ' -f3-)

dunstify "$artist" "$song" --timeout=3000
