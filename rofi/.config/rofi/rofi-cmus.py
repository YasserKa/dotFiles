#!/bin/python
from os import environ
import subprocess
import re
import sys

CMUS_PATH_LIB = environ['XDG_CONFIG_HOME'] + '/cmus/lib.pl'
MUSIC_PATH = environ['HOME'] + '/Music'


def main():
    if len(sys.argv) == 1:
        # Show all the available music tracks
        result = [re.match(r'^/(.+/)*(.+)\.(.+)$', line).group(2)
                  for line in open(CMUS_PATH_LIB)]
        print("\n".join(result))
    else:
        # Find the directory of the music track and play it
        music_path = subprocess.getoutput(
            'find ' + MUSIC_PATH + ' -name "' + re.escape(sys.argv[1])+'*"')

        subprocess.run(
            ['cmus-remote', '-C', 'add -q '+music_path])
        subprocess.run(['cmus-remote', '--next', '--play'])


if __name__ == "__main__":
    main()
