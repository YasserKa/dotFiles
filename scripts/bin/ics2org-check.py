#!/usr/bin/env -S uv run --script
#
# /// script
# requires-python = ">=3.12"
# dependencies = [
#     "icalendar",
#     "recurring-ical-events"
# ]
# ///
"""
ics2org-check — make sure every confirmed Outlook event has a matching
SCHEDULED block in your Org agenda.

For each confirmed event in the ICS (from a URL or a file) inside the look-ahead
window, it checks whether the event's time span is covered by SCHEDULED
timestamps in your Org files. Uncovered events are reported and, with --create,
appended to an inbox Org file as TODOs scheduled over the event's time span.

Requires:  pip install icalendar recurring-ical-events
"""

import argparse
import datetime as dt
import os
import re
import subprocess
import sys
import urllib.request
from pathlib import Path
from zoneinfo import ZoneInfo

import icalendar
import recurring_ical_events

# --------------------------------------------------------------------------
# Org parsing
# --------------------------------------------------------------------------
# <2026-11-17 Tue 10:00-11:30 +1w>  or  <2026-11-17 Tue 10:00>--<2026-11-17 Tue 11:30>
TS = r"<(\d{4}-\d{2}-\d{2})(?:\s+[^\s>\d]+)?(?:\s+(\d{1,2}:\d{2})(?:-(\d{1,2}:\d{2}))?)?((?:\s+[.+]?\+\d+[hdwmy])?)(?:\s+-\d+[hdwmy])?>"
RANGE_RE = re.compile(TS + r"(?:--" + TS + r")?")
SCHED_RE = re.compile(r"SCHEDULED:\s*(" + RANGE_RE.pattern + ")")
HEADING_RE = re.compile(r"^\*+\s")
UID_RE = re.compile(r":OUTLOOK_UID:\s*(\S+)")


def _dt(date_s, time_s, tz):
    d = dt.date.fromisoformat(date_s)
    if not time_s:
        return dt.datetime.combine(d, dt.time(0, 0), tz)
    h, m = map(int, time_s.split(":"))
    return dt.datetime.combine(d, dt.time(h % 24, m), tz)


def _shift(t, n, unit):
    if unit == "h":
        return t + dt.timedelta(hours=n)
    if unit == "d":
        return t + dt.timedelta(days=n)
    if unit == "w":
        return t + dt.timedelta(weeks=n)
    months = n if unit == "m" else 12 * n
    y, mo = divmod(t.month - 1 + months, 12)
    try:
        return t.replace(year=t.year + y, month=mo + 1)
    except ValueError:  # e.g. 31st in a shorter month
        return None


def parse_org_intervals(files, tz, window_end, include_active=False):
    """Return (list of (start, end) intervals, set of OUTLOOK_UIDs seen)."""
    intervals, uids = [], set()
    for f in files:
        try:
            text = Path(f).read_text(encoding="utf-8", errors="replace")
        except OSError as e:
            print(f"warn: cannot read {f}: {e}", file=sys.stderr)
            continue
        uids.update(UID_RE.findall(text))
        rx = RANGE_RE if include_active else SCHED_RE
        for line in text.splitlines():
            for m in rx.finditer(line):
                g = m.groups()
                if not include_active:
                    g = g[1:]  # drop the outer capture
                d1, t1a, t1b, rep, d2, t2a, _, _ = g
                if not t1a:  # untimed: no time block, doesn't cover a meeting
                    continue
                start = _dt(d1, t1a, tz)
                if d2 and t2a:
                    end = _dt(d2, t2a, tz)
                elif t1b:
                    end = _dt(d1, t1b, tz)
                    if end <= start:
                        end += dt.timedelta(days=1)
                else:
                    continue  # point-in-time, no span to cover
                intervals.append((start, end))
                rm = re.search(r"\+(\d+)([hdwmy])", rep or "")
                if rm:  # expand repeater forward through the window
                    n, u = int(rm.group(1)), rm.group(2)
                    s, e, k = start, end, 0
                    while k < 1000:
                        s2, e2 = _shift(s, n, u), _shift(e, n, u)
                        if s2 is None or e2 is None or s2 > window_end:
                            break
                        intervals.append((s2, e2))
                        s, e, k = s2, e2, k + 1
    return intervals, uids


def is_covered(start, end, intervals, tolerance):
    """True if [start, end] is covered by the union of intervals (± tolerance)."""
    rel = sorted((s, e) for s, e in intervals if e > start and s < end)
    cursor = start + tolerance
    target = end - tolerance
    for s, e in rel:
        if s - tolerance > cursor:
            return False
        cursor = max(cursor, e)
        if cursor >= target:
            return True
    return cursor >= target


# --------------------------------------------------------------------------
# ICS parsing
# --------------------------------------------------------------------------
def load_ics(src):
    if re.match(r"^(https?|webcal)://", src):
        url = re.sub(r"^webcal://", "https://", src)
        req = urllib.request.Request(url, headers={"User-Agent": "ics2org-check"})
        with urllib.request.urlopen(req, timeout=60) as r:
            return r.read()
    return Path(src).expanduser().read_bytes()


def _addr(v):
    return str(v).lower().replace("mailto:", "").strip()


def is_confirmed(ev, my_emails, include_tentative):
    """Decide whether *you* have committed to this event."""
    if str(ev.get("STATUS", "")).upper() == "CANCELLED":
        return False
    if str(ev.get("TRANSP", "")).upper() == "TRANSPARENT":
        return False  # shown as Free
    busy = str(ev.get("X-MICROSOFT-CDO-BUSYSTATUS", "")).upper()
    if busy == "FREE" or (busy == "TENTATIVE" and not include_tentative):
        return False
    if my_emails:
        if ev.get("ORGANIZER") and _addr(ev["ORGANIZER"]) in my_emails:
            return True
        att = ev.get("ATTENDEE")
        att = att if isinstance(att, list) else ([att] if att else [])
        for a in att:
            if _addr(a) in my_emails:
                ps = str(a.params.get("PARTSTAT", "")).upper()
                ok = {"ACCEPTED"} | ({"TENTATIVE"} if include_tentative else set())
                return ps in ok
    # Published Outlook calendars usually strip attendees: fall back to
    # busy status (BUSY / OOF / WORKINGELSEWHERE, or unset).
    return True


def to_local(v, tz):
    if isinstance(v, dt.datetime):
        return (v.replace(tzinfo=tz) if v.tzinfo is None else v).astimezone(tz)
    return dt.datetime.combine(v, dt.time(0, 0), tz)  # all-day


def confirmed_events(cal, tz, start, end, my_emails, include_tentative, include_allday):
    out = []
    for ev in recurring_ical_events.of(cal).between(start, end):
        s_raw = ev["DTSTART"].dt
        allday = not isinstance(s_raw, dt.datetime)
        if allday and not include_allday:
            continue
        if not is_confirmed(ev, my_emails, include_tentative):
            continue
        s = to_local(s_raw, tz)
        if "DTEND" in ev:
            e = to_local(ev["DTEND"].dt, tz)
        elif "DURATION" in ev:
            e = s + ev["DURATION"].dt
        else:
            e = s + (dt.timedelta(days=1) if allday else dt.timedelta(0))
        if e <= s:
            continue
        uid = str(ev.get("UID", "")) + "@" + s.strftime("%Y%m%dT%H%M")
        out.append(
            {
                "uid": uid,
                "summary": str(ev.get("SUMMARY", "(no title)")),
                "location": str(ev.get("LOCATION", "") or ""),
                "start": s,
                "end": e,
            }
        )
    return sorted(out, key=lambda x: x["start"])


# --------------------------------------------------------------------------
# Output
# --------------------------------------------------------------------------
def org_range(s, e):
    a = s.strftime("%Y-%m-%d %a")
    if s.date() == e.date():
        return f"<{a} {s:%H:%M}-{e:%H:%M}>"
    return f"<{a} {s:%H:%M}>--<{e:%Y-%m-%d %a %H:%M}>"


def org_entry(ev, keyword, tags):
    tag = f" :{':'.join(tags)}:" if tags else ""
    lines = [
        f"* {keyword} {ev['summary']}{tag}".replace("  ", " "),
        f"  SCHEDULED: {org_range(ev['start'], ev['end'])}",
        "  :PROPERTIES:",
        f"  :OUTLOOK_UID: {ev['uid']}",
    ]
    if ev["location"]:
        lines.append(f"  :LOCATION: {ev['location']}")
    lines.append("  :END:")
    return "\n".join(lines) + "\n"


def notify(title, body):
    for cmd in (
        ["notify-send", "-a", "ics2org", title, body],
        ["osascript", "-e", f'display notification "{body}" with title "{title}"'],
    ):
        try:
            subprocess.run(cmd, check=False, timeout=10, capture_output=True)
            return
        except (FileNotFoundError, subprocess.TimeoutExpired):
            continue


def expand_org_files(paths):
    files = []
    for p in paths:
        p = Path(os.path.expanduser(p))
        if p.is_dir():
            files += sorted(p.rglob("*.org"))
        elif p.exists():
            files.append(p)
    return files


def resolve_ics(a, ap):
    """ICS source, in order: --ics, --ics-cmd, $ICS2ORG_URL, $ICS2ORG_CMD."""
    if a.ics:
        return a.ics
    cmd = a.ics_cmd or os.environ.get("ICS2ORG_CMD")
    if not cmd and os.environ.get("ICS2ORG_URL"):
        return os.environ["ICS2ORG_URL"].strip()
    if not cmd:
        ap.error("give --ics, --ics-cmd, or set ICS2ORG_URL / ICS2ORG_CMD")
    r = subprocess.run(cmd, shell=True, capture_output=True, text=True, timeout=60)
    url = (r.stdout.splitlines() or [""])[0].strip()  # pass: first line = secret
    if r.returncode != 0 or not url:
        sys.exit(
            f"error: ICS command failed (exit {r.returncode}); "
            f"is gpg-agent unlocked?\n{r.stderr.strip()}"
        )
    return url


def main():
    ap = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter
    )
    src = ap.add_mutually_exclusive_group()
    src.add_argument(
        "--ics",
        help="ICS URL (https/webcal) or local file. Visible in `ps`; "
        "prefer --ics-cmd or $ICS2ORG_URL for secret links",
    )
    src.add_argument(
        "--ics-cmd",
        help="Shell command printing the ICS URL, e.g. 'pass show outlook/ics'",
    )
    ap.add_argument(
        "--org", nargs="+", required=True, help="Org files or directories to check"
    )
    ap.add_argument(
        "--inbox", default="~/org/outlook-inbox.org", help="Org file for created tasks"
    )
    ap.add_argument(
        "--days", type=int, default=14, help="Look-ahead window in days (default 14)"
    )
    ap.add_argument(
        "--me", nargs="*", default=[], help="Your email address(es), to read PARTSTAT"
    )
    ap.add_argument("--tz", default="Europe/Stockholm")
    ap.add_argument(
        "--tolerance", type=int, default=0, help="Minutes of slack at either end"
    )
    ap.add_argument("--include-tentative", action="store_true")
    ap.add_argument("--include-allday", action="store_true")
    ap.add_argument(
        "--include-active",
        action="store_true",
        help="Also count plain active timestamps, not only SCHEDULED",
    )
    ap.add_argument(
        "--create", action="store_true", help="Append missing tasks to --inbox"
    )
    ap.add_argument("--keyword", default="TODO")
    ap.add_argument("--tags", nargs="*", default=["outlook"])
    ap.add_argument(
        "--notify", action="store_true", help="Desktop notification on gaps"
    )
    a = ap.parse_args()

    tz = ZoneInfo(a.tz)
    now = dt.datetime.now(tz)
    win_start = now.replace(hour=0, minute=0, second=0, microsecond=0)
    win_end = win_start + dt.timedelta(days=a.days)

    ics = resolve_ics(a, ap)
    try:
        raw = load_ics(ics)
    except Exception as e:  # never echo the secret URL into logs
        sys.exit(
            f"error: could not fetch calendar ({type(e).__name__}: "
            f"{getattr(e, 'code', '') or getattr(e, 'reason', '') or 'see --ics source'})"
        )
    cal = icalendar.Calendar.from_ical(raw)
    events = confirmed_events(
        cal,
        tz,
        win_start,
        win_end,
        {m.lower() for m in a.me},
        a.include_tentative,
        a.include_allday,
    )
    events = [e for e in events if e["end"] > now]  # ignore what already finished

    inbox = Path(os.path.expanduser(a.inbox))
    org_files = expand_org_files(a.org + ([str(inbox)] if inbox.exists() else []))
    org_files = list(dict.fromkeys(org_files))
    intervals, uids = parse_org_intervals(org_files, tz, win_end, a.include_active)

    tol = dt.timedelta(minutes=a.tolerance)
    missing = [
        e
        for e in events
        if e["uid"] not in uids and not is_covered(e["start"], e["end"], intervals, tol)
    ]

    print(
        f"{len(events)} confirmed event(s) in the next {a.days} days; "
        f"{len(missing)} without a matching scheduled task."
    )
    for e in missing:
        print(f"  MISSING  {org_range(e['start'], e['end'])}  {e['summary']}")

    if missing and a.create:
        inbox.parent.mkdir(parents=True, exist_ok=True)
        new = not inbox.exists() or inbox.stat().st_size == 0
        with inbox.open("a", encoding="utf-8") as fh:
            if new:
                fh.write("#+TITLE: Outlook events\n#+STARTUP: overview\n\n")
            for e in missing:
                fh.write(org_entry(e, a.keyword, a.tags))
        print(f"Added {len(missing)} task(s) to {inbox}")

    if missing and a.notify:
        body = "\n".join(f"{e['start']:%a %H:%M} {e['summary']}" for e in missing[:5])
        notify(f"{len(missing)} Outlook event(s) not in your agenda", body)

    return 1 if (missing and not a.create) else 0


if __name__ == "__main__":
    sys.exit(main())
