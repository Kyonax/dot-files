#!/bin/sh
# Switch the UPS driver to nutdrv_qx (nut-scanner's first choice), then finish
# the setup like setup-nut.sh: upsmon, the part that can power the PC off,
# only starts if the UPS reads "OL" (on mains).
# Run with:  sudo sh use-nutdrv_qx.sh
set -u
[ "$(id -u)" -eq 0 ] || { echo "Run this with sudo." >&2; exit 1; }
HERE=$(CDPATH='' cd -- "$(dirname -- "$0")" && pwd)

systemctl stop nut-driver@ups.service 2>/dev/null
install -m 0644 -o root -g root "$HERE/etc-nut/ups.conf" /etc/nut/ups.conf || exit 1
echo "ups.conf now uses nutdrv_qx."
systemctl restart nut-driver-enumerator.service
systemctl restart nut-driver@ups.service

# nutdrv_qx probes the protocol first (about 8 s, status "WAIT" meanwhile), and
# upsd reconnects on its own: wait up to 40 s for a real reading.
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
    systemctl stop nut-driver@ups.service
    echo "STOPPED SAFELY: the UPS reads '$STATUS', not OL (on mains)."
    echo "upsmon was NOT started, and the driver's retry loop is stopped."
    echo
    echo "=== one debug run of the driver (15 s) ==="
    timeout 15 /usr/lib/nut/nutdrv_qx -a ups -u nut -DD 2>&1 | tail -45
    exit 2
    ;;
esac
