#!/usr/bin/env python
# coding=utf-8
import os
import tempfile

HTML_BODY = """\
<html>
<head>
<title>Qutescript: {prefix}</title>
</head>
<body>
<pre>{script_path}</pre>
{content}
</body>
"""

HTML_MESSAGE_TEMPLATE = """\
<pre>
{}
</pre>
"""

log_file_path = './qutescript.log'


def write_log(message, file_path=None, console=False):
    if console:
        print('***', message)
    file_path = file_path or log_file_path
    file_path = os.path.abspath(os.path.expanduser(file_path))
    with open(file_path, 'a') as logfile:
        logfile.write('*** {}\n\n'.format(message))


def normalize_prefix(prefix):
    if prefix:
        prefix = prefix.replace('.', '_')
        if not prefix.endswith('_'):
            prefix = prefix + '_'
    return prefix


def log_to_browser(*messages, prefix: str = None, console=True, script_path=None):
    """
    Write messages to logs and a temporary file,
    Attempt to open the file through FIFO in the browser.
    """
    [write_log(msg, console=console) for msg in messages]
    html_messages = '\n'.join([HTML_MESSAGE_TEMPLATE.format(m) for m in messages])
    html_body = HTML_BODY.format(
        content=html_messages,
        prefix=prefix,
        script_path=script_path,
    )
    send_html(html_body, prefix=prefix, script_path=script_path)


def send_html(text, prefix: str = None, script_path=None):
    fifo = os.getenv('QUTE_FIFO')
    if not fifo:
        return

    prefix = normalize_prefix(prefix)
    prefix = 'qutescript_{}'.format((prefix or ''))
    script_path = script_path or ''
    with tempfile.NamedTemporaryFile(mode='w', prefix=prefix, suffix='.html', delete=False) as out_file:
        out_file.writelines(text)
        with open(fifo, 'w') as fifo_file:
            fifo_file.write('open -t file://{}\n'.format(
                os.path.abspath(out_file.name)))
