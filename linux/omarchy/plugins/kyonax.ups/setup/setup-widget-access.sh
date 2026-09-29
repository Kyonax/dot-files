#!/bin/sh
# One-time setup for the kyonax.ups widget's control buttons.
# Run it from your own account:   sudo sh setup-widget-access.sh
#
#   1. Adds a restricted upsd user "widget" that may send ONLY beeper.toggle,
#      test.battery.start.quick and test.battery.stop. Never load.off, never a
#      shutdown command: upsd refuses those for this user.
#   2. Stores its random password for you in ~/.config/kyonax-ups/upsd-widget
#      (mode 600). bin/ups-instcmd sends it over upsd's socket, never in argv.
#   3. Installs the upssched-cmd that tags each journal line with UPS_EVENT, so
#      the widget reads outages from a field. Its shutdown-timer line is the
#      same as before.
#   4. Reloads upsd (no connection dropped; upsmon keeps watching) and proves
#      both paths: a login that sends no command, and one "Test" event through
#      to a quiet notification.
#
# Safe to run again: it replaces the [widget] section and the password.
set -u
[ "$(id -u)" -eq 0 ] || { echo "Run this with sudo." >&2; exit 1; }
U=${SUDO_USER:-}
[ -n "$U" ] && [ "$U" != root ] || { echo "Run it with sudo from your own account." >&2; exit 1; }
H=$(getent passwd "$U" | cut -d: -f6)
G=$(id -gn "$U")
[ -d "$H" ] || { echo "No home directory for $U." >&2; exit 1; }
HERE=$(CDPATH='' cd -- "$(dirname -- "$0")" && pwd)
umask 077

# 1. The restricted upsd user, replacing any earlier [widget] section.
PW=$(tr -dc 'A-Za-z0-9' < /dev/urandom | head -c 32)
[ "${#PW}" -eq 32 ] || { echo "Could not generate a password." >&2; exit 1; }
cp -a /etc/nut/upsd.users "/etc/nut/upsd.users.bak-$(date +%Y%m%d-%H%M%S)" || exit 1
awk '/^# kyonax.ups widget/ { next }
     /^\[widget\]/          { skip = 1; next }
     /^\[/                  { skip = 0 }
     !skip' /etc/nut/upsd.users > /etc/nut/upsd.users.new || exit 1
printf '\n# kyonax.ups widget: harmless instant commands only.\n[widget]\n\tpassword = %s\n\tinstcmds = beeper.toggle\n\tinstcmds = test.battery.start.quick\n\tinstcmds = test.battery.stop\n' \
  "$PW" >> /etc/nut/upsd.users.new || exit 1
chown root:nut /etc/nut/upsd.users.new && chmod 0640 /etc/nut/upsd.users.new \
  && mv /etc/nut/upsd.users.new /etc/nut/upsd.users || exit 1
echo "upsd user 'widget' written: beeper.toggle, test.battery.start.quick, test.battery.stop."

# 2. The credential, readable by you only.
D="$H/.config/kyonax-ups"
install -d -m 0700 -o "$U" -g "$G" "$D" || exit 1
printf 'user=widget\npassword=%s\n' "$PW" > "$D/upsd-widget.new" || exit 1
chown "$U:$G" "$D/upsd-widget.new" && chmod 0600 "$D/upsd-widget.new" \
  && mv "$D/upsd-widget.new" "$D/upsd-widget" || exit 1
unset PW
echo "Credential stored in $D/upsd-widget (mode 600)."

# 3. upssched-cmd with UPS_EVENT tags.
install -m 0750 -o root -g nut "$HERE/nut/etc-nut/upssched-cmd" /etc/nut/upssched-cmd || exit 1
echo "upssched-cmd updated."

# 4. Reload upsd, then prove both paths.
systemctl reload nut-server.service || { echo "upsd reload failed." >&2; exit 1; }
sleep 1
echo
printf 'Login test (sends no command): '
runuser -u "$U" -- bash "$HERE/../bin/ups-instcmd" --cred "$D/upsd-widget" --check
printf 'Event test (a quiet "Test" notification should appear): '
if runuser -u nut -- /etc/nut/upssched-cmd selftest && sleep 1 \
   && journalctl -t ups-event -n 1 -o json --output-fields=UPS_EVENT --no-pager | grep -q '"UPS_EVENT":"test"'; then
  echo OK
else
  echo "FAILED (look at: journalctl -t ups-event -n 3)"
fi
echo
echo "Done. The widget's beeper and battery-test buttons are live."
