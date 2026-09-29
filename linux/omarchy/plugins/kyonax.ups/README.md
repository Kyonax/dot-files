# kyonax.ups — the desk UPS in the Omarchy bar

A bar widget for the NUT-monitored UPS on kyo-labs (a Richcomm `0925:1234`
USB board driven by `nutdrv_qx`). A dim plug while all is well; on battery it
shows the countdown to the clean shutdown, in the urgent colour.

## The bar

| State | Shows |
|---|---|
| On mains, protected | dim plug; no label, or load % / input V (setting `barLabel`) |
| On battery | battery glyph + `2:41`, the countdown to the clean shutdown |
| Battery low · timer fired | `LOW` · `shutdown` |
| Shutdown monitor down | shield-off + `unprotected` |
| No readings | plug-off + `UPS?` |

Left click = popup · right = read now · middle = outage log in a terminal.

## The popup

Hero (state, the one sentence that matters, a protected pill) · a rail that
fills toward the shutdown while on battery · tiles (load, battery, mains, UPS
temperature) · READINGS · PROTECTION (the policy parsed from
`/etc/nut/upssched.conf`, the three NUT units, the notifier) · OUTAGES (the last
outage and recent events from `journalctl -t ups-event`) · actions.

Keys: `r` read · `b` beeper · `t` battery test · `e` log · `s` shut down · Esc
closes. Confirmations open on **Cancel**: arrows or Tab switch, Enter or Space
take it, `y` / `n` also work.

## Actions

- **Beeper** and **quick battery test** go to the UPS through `bin/ups-instcmd`,
  as a restricted upsd user that can do nothing else. One-time setup, from your
  own account: `sudo sh setup/setup-widget-access.sh`.
- **Shut down now** runs `omarchy-system-shutdown` (closes apps first).
- **Log** opens `ups-log` in a terminal.

## IPC

```sh
quickshell ipc -p /usr/share/omarchy/shell call kyonax.ups <fn>
```

`diag`, `state`, `status`, `refresh`, `beeper`, `log`, `askTest`,
`askShutdown`, `open` / `close` / `toggle`. The test and the shutdown only
raise their confirmation over IPC; a person still accepts it.

## Files

| File | What |
|---|---|
| `Panel.qml` | bar button + popup (one file on purpose, see traps) |
| `Service.qml` | the reads (one Process per source, each on its own cadence) and the commands |
| `Model.js` | all logic, pure; `node Model.test.mjs` |
| `bin/ups-probe` | NUT units, notifier, and whether the control credential exists |
| `bin/ups-instcmd` | the only path to instant commands; the password never reaches argv |
| `setup/setup-widget-access.sh` | the one-time control setup (sudo) |
| `setup/nut/` | the NUT install as done on 2026-09-29: `setup-nut.sh`, `use-nutdrv_qx.sh`, `etc-nut/` |
| `setup/desktop/` | reference copies of `ups-notify`, its user unit, and `ups-log` |

This folder lives in `dot-files/linux/omarchy/plugins/` and is symlinked into
`~/.config/omarchy/plugins/`, like kyonax.shipwright and kyonax.tempo-hours.
Installed outside it:

- `/etc/nut/*`, from `setup/nut/etc-nut/`.
- `~/.local/bin/ups-log`, a symlink to `setup/desktop/ups-log`, like the
  tempo-hours wrappers.
- `~/.local/bin/ups-notify` and `~/.config/systemd/user/ups-notify.service`.
  These are real copies ON PURPOSE: outage notifications must not depend on
  the data disk being mounted. `setup/desktop/` holds the versioned source.
- The credential `~/.config/kyonax-ups/upsd-widget` (mode 600). It never
  enters this repo.

## Traps honoured

From the kyonax.tempo-hours bar-plugin traps: the root takes its implicit size
from the button; one `Panel.qml`; `Quickshell.execDetached` for anything that
launches; single-word terminal commands that re-take `/dev/tty`; no property
named `data`.

Found here:

- `PanelHero`'s detail pill has no maximum width, so a long detail squeezes
  the title to nothing. Keep it to a word or two.
- After an edit the shell keeps serving the cached QML/JS, `rescanPlugins`
  included, until its next start.
- `richcomm_usb` fails on this board ("driver callback failed: Entity not
  found"). `nutdrv_qx` works, but reports `WAIT` for about 8 s at start, so any
  "is it on mains?" gate must wait through `WAIT`.
