#!/usr/bin/env python
# coding=utf-8
# Description: yanks URLs of open tabs as org or markdown URLs
# Dependencies: xclip

from qutescript import userscript
import yaml
import time
import os


SESSION_FILE_NAME = 'userscript_session'
SESSION_PATH = f"{os.environ['HOME']}/.local/share/qutebrowser/sessions"


def get_markdown_urls():
    file_path = f"{SESSION_PATH}/{SESSION_FILE_NAME}.yml"
    urls = {}

    with open(file_path, 'r') as stream:
        try:
            windows = yaml.safe_load(stream)['windows']
            for window in windows:
                # get the last url in the history of each tab (aka current url)
                for tab in window['tabs']:
                    current_url = tab['history'][-1]
                    urls[current_url['title']] = current_url['url']
        except yaml.YAMLError:
            pass

    org_urls = [f"[[{title}][{url}]]" for (url, title) in urls.items()]
    # markdown_urls = [f"[{url}]({title})" for (url, title) in urls.items()]

    parsed_urls = '\n'.join(org_urls)

    return parsed_urls


@userscript
def main(request):
    # store session file
    request.send_command(f"session-save {SESSION_FILE_NAME}")
    # wait until session file is updated/created
    time.sleep(0.3)
    # extract urls for buffers
    markdown_urls = get_markdown_urls()
    command = f"echo -n '{markdown_urls}' | xclip -selection c"
    os.system(command)
    request.send_command("message-info 'yanked urls'")


if __name__ == '__main__':
    main()
