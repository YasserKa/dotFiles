#Enter script code
#!/usr/bin/env python
# coding=utf-8
# Description: A userscript for qutebrowser to insert snippets
# Dependencies: python-rofi


import os

from rofi import Rofi


r = Rofi()

myvars = {}

snippets_file_path = os.path.expanduser("~/.config/.snippets")

with open(snippets_file_path) as myfile:
    for line in myfile:
        name, var = line.partition("=")[::2]
        myvars[name.strip()] = var.strip()

options = myvars.keys()
index, key = r.select("Insert snippet", options)

if key != 0:
    exit()

snippet = list(myvars.values())[index]

keyboard.send_keys(snippet)

