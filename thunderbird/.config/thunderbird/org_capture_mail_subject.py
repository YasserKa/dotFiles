#!/usr/bin/env python3
"""Org capture mail subject from inside thunderbird"""

import json
import subprocess
import sys
from urllib.parse import quote


def main():
    # Read the message
    raw_length = sys.stdin.buffer.read(4)
    if len(raw_length) == 0:
        return
    message_length = int.from_bytes(raw_length, byteorder="little")
    message = sys.stdin.buffer.read(message_length)
    data = json.loads(message)["subject"]

    header_message_id_encoded = quote(data["headerMessageId"], safe="")
    subprocess.Popen(
        (
            f"org_capture 'tb:{header_message_id_encoded}' 'LTU email: {data['subject']}'"
        ),
        shell=True,
        text=True,
    )

    subprocess.Popen(
        ("dunstify 'Note captured'"),
        shell=True,
        text=True,
    )

    # Reply back
    response = json.dumps({"status": "ok"}).encode("utf-8")
    sys.stdout.buffer.write(len(response).to_bytes(4, byteorder="little"))
    sys.stdout.buffer.write(response)
    sys.stdout.flush()


if __name__ == "__main__":
    main()
