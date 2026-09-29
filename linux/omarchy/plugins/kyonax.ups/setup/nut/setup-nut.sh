#!/bin/sh
# Set up NUT for the USB UPS on kyo-labs. Run with:  sudo sh setup-nut.sh
#
#   1. Backs up /etc/nut and installs the prepared config. One random password,
#      never printed, is shared by upsd.users and upsmon.conf.
#   2. Starts the UPS driver and the NUT data server.
#   3. ONLY IF the UPS reads "OL" (on mains): enables everything at boot and
#      starts upsmon, the part that can power the PC off. A misread therefore
#      can't cause a surprise shutdown while renders are running.
set -u
[ "$(id -u)" -eq 0 ] || { echo "Run this with sudo." >&2; exit 1; }
umask 027
HERE=$(CDPATH='' cd -- "$(dirname -- "$0")" && pwd)
SRC="$HERE/etc-nut"

BK="/etc/nut.orig-$(date +%Y%m%d-%H%M%S)"
cp -a /etc/nut "$BK" || exit 1
echo "Stock config backed up to $BK"

PW=$(tr -dc 'A-Za-z0-9' < /dev/urandom | head -c 32)
[ "${#PW}" -eq 32 ] || { echo "Could not generate a password." >&2; exit 1; }

install -m 0644 -o root -g root "$SRC/nut.conf"      /etc/nut/nut.conf      || exit 1
install -m 0644 -o root -g root "$SRC/ups.conf"      /etc/nut/ups.conf      || exit 1
install -m 0644 -o root -g root "$SRC/upssched.conf" /etc/nut/upssched.conf || exit 1
install -m 0750 -o root -g nut  "$SRC/upssched-cmd"  /etc/nut/upssched-cmd  || exit 1
for f in upsd.users upsmon.conf; do
  sed "s|@UPSMON_PASSWORD@|$PW|" "$SRC/$f" > "/etc/nut/$f.new" || exit 1
  chown root:nut "/etc/nut/$f.new" && chmod 0640 "/etc/nut/$f.new" \
    && mv "/etc/nut/$f.new" "/etc/nut/$f" || exit 1
done
unset PW
echo "Config installed."

systemctl start nut-driver-enumerator.service
systemctl start nut-server.service

# nutdrv_qx reports "WAIT" while it probes the protocol (~8 s), and upsd
# reconnects on its own: wait up to 40 s for a real reading. A single read
# after a fixed sleep saw WAIT and stopped (2026-09-29).
i=0
while [ "$i" -lt 20 ]; do
  STATUS=$(upsc ups@localhost ups.status 2>&1)
  case "$STATUS" in Error*|""|WAIT*) sleep 2; i=$((i + 1)) ;; *) break ;; esac
done
echo
echo "=== UPS reading ==="
upsc ups@localhost 2>&1
echo
case "$STATUS" in
  OL*)
    systemctl enable nut.target nut-driver.target nut-driver-enumerator.path \
      nut-driver-enumerator.service nut-server.service nut-monitor.service
    systemctl start nut-driver-enumerator.path nut-monitor.service
    sleep 2
    echo "=== services ==="
    systemctl --no-pager --lines=5 status nut-server.service nut-monitor.service 'nut-driver@*'
    echo
    echo "DONE: the UPS reads '$STATUS'. Monitoring is on and starts at every boot."
    ;;
  *)
    echo "STOPPED SAFELY: the UPS reads '$STATUS', not OL (on mains)."
    echo "upsmon was NOT started, so nothing can shut the PC down. Driver log:"
    journalctl -b --no-pager -n 25 -u 'nut-driver@*' -u nut-driver-enumerator.service -u nut-server.service
    exit 2
    ;;
esac
