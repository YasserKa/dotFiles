import re
import subprocess

def mark(text, args, Mark, extra_cli_args, *a):
    # This function is responsible for finding all
    # matching text. extra_cli_args are any extra arguments
    # passed on the command line when invoking the kitten.
    # We mark all individual word for potential selection
    for idx, m in enumerate(re.finditer(r'\w+', text)):
        start, end = m.span()
        mark_text = text[start:end].replace('\n', '').replace('\0', '')
        # The empty dictionary below will be available as groupdicts
        # in handle_result() and can contain arbitrary data.
        yield Mark(idx, start, end, mark_text, {})


def handle_result(args, data, target_window_id, boss, extra_cli_args, *a):
    # This function is responsible for performing some
    # action on the selected text.
    # matches is a list of the selected entries and groupdicts contains
    # the arbitrary data associated with each entry in mark() above
    matches, groupdicts = [], []
    for m, g in zip(data['match'], data['groupdicts']):
        if m:
            matches.append(m), groupdicts.append(g)
    for word, match_data in zip(matches, groupdicts):
        path='/home/yasser/bin'
        # cmd = [x.format(path=path, line=line) for x in ('vim', '+{line}', '{path}')]

        # w = boss.window_id_map.get(target_window_id)
        # w.open_url(word, hyperlink_id=1)
        # Lookup the word in a dictionary, the open_url function
        # will open the provided url in the system browser
        # boss.open_url(f'https://www.google.com/search?q=define:{word}')
