#!/usr/bin/env sh

QUEUEDIR=$HOME/.msmtpqueue

# Set secure permissions on created directories and files
umask 077

# Change to queue directory (create it if necessary)
if [ ! -d "$QUEUEDIR" ]; then
  mkdir -p "$QUEUEDIR" || exit 1
fi
cd "$QUEUEDIR" || exit 1

# Create new unique filenames of the form
# MAILFILE:  ccyy-mm-dd-hh.mm.ss[-x].mail
# MSMTPFILE: ccyy-mm-dd-hh.mm.ss[-x].msmtp
# where x is a consecutive number only appended if you send more than one
# mail per second.
BASE="$(date +%Y-%m-%d-%H.%M.%S)"
if [ -f "$BASE.mail" ] || [ -f "$BASE.msmtp" ]; then
  TMP="$BASE"
  i=1
  while [ -f "$TMP-$i.mail" ] || [ -f "$TMP-$i.msmtp" ]; do
    i=$((i + 1))
  done
  BASE="$BASE-$i"
fi
MAILFILE="$BASE.mail"
MSMTPFILE="$BASE.msmtp"

# Write command line to $MSMTPFILE
echo "$@" > "$MSMTPFILE" || exit 1

# Write the mail to $MAILFILE
cat > "$MAILFILE" || exit 1

# If we are online, run the queue immediately.
$HOME/bin/wait_internet
if [ $? -eq 0 ]; then
  $XDG_CONFIG_HOME/neomutt/bin/msmtp/msmtp-runqueue.sh > /dev/null &
else
  # Notify me if it gets queuedd
  dunstify --timeout=60000 "Mail is queued"
fi

exit 0
